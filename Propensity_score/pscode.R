#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(broom)
library(WeightIt)
library(twang)
library(PSweight)
library(cobalt)
library(survey)
library(modelsummary)

#全部の変数を消す
rm(list=ls())

#datasetの読み込み
data("lindner", package = "twang")
view(lindner)

#abcix：アブシキシマブ投与の有無（1：あり、0：なし）
#stent：ステント挿入の有無（1：あり、0：なし）
#height：身長（cm）
#female：性別（1：女性、0：男性）
#diabetic：糖尿病の有無（1：あり、0：なし）
#acutemi：最近の急性心筋梗塞の有無（1：あり、0：なし）
#ejecfrac：左室駆出率（%）
#ves1proc：PCIの対象になった血管の数
#sixMonthSurvive：PCI6ヶ月後の生存状況（TRUE：生存、FALSE：死亡）

# 共変量調整をしない解析
mod_naive <- glm(sixMonthSurvive ~ abcix, data = lindner, family = gaussian)

## tidy(conf.int = TRUE)により推定値と信頼区間をデータフレーム化
mod_naive %>% tidy(conf.int = TRUE)

#共変量調整をしな回帰表
modelsummary (mod_naive)

#背景因子影響をプロット
love.plot(abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc,
          data = lindner, estimand = "ATE",
          abs = TRUE, stats = "mean.diffs", thresholds = c(m = 0.1),
          s.d.denom = "pooled", binary = "std")

#WeightItパッケージ
# 傾向スコアの推定と重みの算出
psmodel_wtit <- weightit(abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc,
                         data = lindner, method = "ps", estimand = "ATE")

# 傾向スコアと重みをもとのデータセットに格納
lindner_iptw_wtit <- lindner %>%
  mutate(ps = psmodel_wtit$ps, weight = psmodel_wtit$weights)

# propensity scoreの分布
bal.plot(lindner_iptw_wtit, treat = lindner_iptw_wtit$abcix, var.name = "ps")

#重み付け後の背景因子のバランス
love.plot(abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc,
          data = lindner_iptw_wtit, weights = lindner_iptw_wtit$weight,
          abs = TRUE, stats = "mean.diffs", thresholds = c(m = 0.1),
          s.d.denom = "pooled", binary = "std")

# 重みやデータの情報を格納
design_wtit <- svydesign(id = ~ 1, weights = ~ weight, data = lindner_iptw_wtit)

# 重み付けの線形回帰
mod_iptw_wtit <- svyglm(sixMonthSurvive ~ abcix,
                        design = design_wtit, family = gaussian())

# 割合の差の推定値を出力
mod_iptw_wtit %>% tidy(conf.int = TRUE)

#共変量調整をした回帰表
modelsummary (mod_iptw_wtit)

#coefpoot
install.packages("coefplot")
library(coefplot)
coefplot(mod_naive, intercept = FALSE)
coefplot(mod_iptw_wtit, intercept = FALSE)

#ggplot
tidy(mod_naive, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% #切片削除
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

#ggplot
tidy(mod_iptw_wtit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% #切片削除
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
