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
smokerbaby_weight <- read_csv("Propensity_score_IPW/smokerbaby_weight.csv")
view(smokerbaby_weight)

# 'mbsmoke'と'fbaby'が 'Yes' の場合は 1 に、'No' の場合は 0 に置き換える
smokerbaby_weight$mbsmoke <- ifelse(smokerbaby_weight$mbsmoke == "smoker", 1, 0)

#mbsmokeをカテゴリ変数に変換
smokerbaby_weight$mbsmoke <- as.factor(smokerbaby_weight$mbsmoke)

#Bweightを連続変数に変換
smokerbaby_weight$bweight <- as.numeric(smokerbaby_weight$bweight)

#mbsmoke0と1ごとにbweightのヒストグラムを重ねて作成
ggplot(smokerbaby_weight, aes(x = bweight, group = mbsmoke)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal()

##mbsmokeごとにbweightの平均値と信頼区間のテーブルを作成
smokerbaby_weight %>%
  group_by(mbsmoke) %>%
  summarise(mean = mean(bweight), sd = sd(bweight), n = n()) %>%
  mutate(se = sd/sqrt(n), lower = mean - 1.96*se, upper = mean + 1.96*se)

##mbsmokeごとにbweightの平均値に差があるかをT検定
t.test(bweight ~ mbsmoke, data = smokerbaby_weight)

#喫煙者と非喫煙者の出産時体重の箱ひげ図を書いて比較してみて
ggplot(data = smokerbaby_weight) +     # tenkukan20データでキャンバス準備
  aes(x = mbsmoke, y = bweight, fill = mbsmoke)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_boxplot() +                  # はこひげ図を描く
  xlab("mbsmoke") + ylab("weight") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#mbsmokeを連続変数に変換
smokerbaby_weight$mbsmoke <- as.numeric(smokerbaby_weight$mbsmoke)

# 喫煙習慣の決定をロジスティック回帰分析
smokerbaby_weight_naive <- glm(mbsmoke ~  mmarried + fbaby + mage + medu, data = smokerbaby_weight)

smokerbaby_weight_naive %>% tidy(conf.int = TRUE)

#共変量調整をしない回帰表
modelsummary (smokerbaby_weight_naive)

#coefpoot
install.packages("coefplot")
library(coefplot)

#ggcoef
ggcoef(smokerbaby_weight_naive,
       mapping = aes_string(y = "term", x = "estimate"),
       conf.int = TRUE,
       exclude_intercept = TRUE,
       conf.level = 0.95,
       exponentiate = FALSE,
       vline = TRUE,
       vline_color = "red",
       vline_linetype =  "solid",
       errorbar_color = "black",
       errorbar_height = .15)


#背景因子影響をプロット
love.plot(mbsmoke ~  mmarried + fbaby + mage + medu,
          data = smokerbaby_weight, estimand = "ATE",
          abs = TRUE, stats = "mean.diffs", thresholds = c(m = 0.1),
          s.d.denom = "pooled", binary = "std")

#WeightItパッケージ
# 傾向スコアの推定と重みの算出
smokerbaby_weight_iptw_wtit <- weightit(mbsmoke ~  mmarried + fbaby + mage+ medu　,
                         data = smokerbaby_weight, method = "ps", estimand = "ATE")

# 傾向スコアと重みをもとのデータセットに格納
smokerbaby_weight_iptw_wtit <- smokerbaby_weight %>%
  mutate(ps = psmodel_wtit$ps, weight = psmodel_wtit$weights)

view(smokerbaby_weight_iptw_wtit)

# propensity scoreの分布
bal.plot(smokerbaby_weight_iptw_wtit, treat = smokerbaby_weight_iptw_wtit$mbsmoke, var.name = "ps")


# 重みやデータの情報を格納
smokerbaby_weight_iptw_wtit <- svydesign(id = ~ 1, weights = ~ weight, data = smokerbaby_weight_iptw_wtit)

# 重み付けの線形回帰
smokerbaby_weight_iptw_wtit <- svyglm(bweight ~ mbsmoke,
                        design = smokerbaby_weight_iptw_wtit, family = gaussian())

# 割合の差の推定値を出力
smokerbaby_weight_iptw_wtit %>% tidy(conf.int = TRUE)

#共変量調整をした回帰表
modelsummary (smokerbaby_weight_iptw_wtit)

#ggplot
tidy(smokerbaby_weight_naive, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% #切片削除
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

#ggplot
tidy(smokerbaby_weight_iptw_wtit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% #切片削除
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# データを整形
data_naive <- tidy(smokerbaby_weight_naive, conf.int = TRUE) %>% filter(term != "(Intercept)")
data_iptw_wtit <- tidy(smokerbaby_weight_iptw_wtit, conf.int = TRUE) %>% filter(term != "(Intercept)")

# x軸の範囲を計算
x_range <- range(c(-500, 500, -500, 500))

# 最初のグラフ
p1 <- ggplot(data_naive) +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term)) +
  theme_gray(base_size = 12) +
  ggtitle("調整前") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_cartesian(xlim = x_range)

# 二番目のグラフ
p2 <- ggplot(data_iptw_wtit) +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term)) +
  theme_gray(base_size = 12) +
  ggtitle("調整後") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_cartesian(xlim = x_range)

p1
p2


# 2つのグラフを縦に並べる
grid.arrange(p1, p2, ncol = 1)

