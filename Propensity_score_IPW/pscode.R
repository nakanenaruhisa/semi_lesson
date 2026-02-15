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

#共変量調整をしない回帰表
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

view(lindner_iptw_wtit)

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

# 必要なパッケージを読み込む
library(ggplot2)
library(gridExtra)
library(broom)

# データを整形
data_naive <- tidy(mod_naive, conf.int = TRUE) %>% filter(term != "(Intercept)")
data_iptw_wtit <- tidy(mod_iptw_wtit, conf.int = TRUE) %>% filter(term != "(Intercept)")

# x軸の範囲を計算
x_range <- range(c(-0.05, 0.25, -0.05, 0.25))

# 最初のグラフ
p1 <- ggplot(data_naive) +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term)) +
  theme_gray(base_size = 12) +
  ggtitle("調整前")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  xlim(x_range)


# 二番目のグラフ
p2 <- ggplot(data_iptw_wtit) +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term)) +
  theme_gray(base_size = 12) +
  ggtitle("調整後")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  xlim(x_range)

# 2つのグラフを縦に並べる
grid.arrange(p1, p2, ncol = 1)


--------------------
#PSweightパッケージ
psmodel_pswt <- abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc

lindner_iptw_pswt <- lindner %>%
  weightit(psmodel_pswt, data = ., method = "ps", estimand = "ATE") %>%
  mutate(ps = psmodel_pswt$ps, weight = psmodel_pswt$weights)

# 傾向スコアや重みの情報を取得
bal_pswt <- SumStat(ps.formula = psmodel_pswt, data = lindner, weight = "IPW")

# 傾向スコアの密度
plot(bal_pswt, type = "density")

# 背景因子のバランス
plot(bal_pswt, type = "balance")

# IPTW
ate_pswt <- PSweight(ps.formula = psmodel_pswt, yname = "sixMonthSurvive", data = lindner,
                     weight = "IPW")



# 結果の出力（tidy関数には対応していないようです）
summary(bal_pswt, type = "DIF")

# 自力での傾向スコアの推定
psmodel_man <- glm(abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc,
                   data = lindner, family = binomial)
## 傾向スコア推定値を格納
ps_man <- psmodel_man$fitted.values

# 傾向スコア推定値と逆確率重みをデータフレームへ格納
lindner_iptw_man <- lindner %>%
  mutate(ps = ps_man,
         weight = case_when(abcix == 1 ~ 1/ps, TRUE ~ 1/(1-ps)))

View(lindner_iptw_man)

# IPTW
design_man <- svydesign(id = ~ 1, weights = ~ weight, data = lindner_iptw_man)
mod_iptw_man <- svyglm(sixMonthSurvive ~ abcix,
                       design = design_man, family = gaussian())
mod_iptw_man %>% tidy(conf.int = TRUE)

