# パッケージのインストール
install.packages("cobalt")
install.packages("Matching")
install.packages("MatchIt")
install.packages("survey")
install.packages("coefplot")
install.packages("extrafont")
install.packages("showtext")


# フォントのインポートと設定
font_add_google("Noto Sans JP", "noto")
showtext_auto()


## ライブラリ読み込み
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(readxl)
library(ggthemes)
library(gtsummary)
library(broom)
library(WeightIt)
library(twang)
library(PSweight)
library(Matching) 
library(MatchIt)
library(cobalt)
library(survey)
library(coefplot)
library(modelsummary)
library(gridExtra)
library(extrafont)
library(showtext)



#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))




##データ読み込み

dataset <- read_excel("Propensity_score_matting/learnig_book_test.xlsx")

View(dataset)    

##treatmentをカテゴリ変数に変換
dataset$treatment <- as.factor(dataset$treatment)


## テキスト利用と成績変化との相関を確認
ggplot(dataset, 
       aes(x = factor(treatment), y = outcome, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Distribution of outcome by Treatment",
       x = "Treatment",
       y = "outcome") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")+
  theme_minimal()

# 共変量調整をしない解析
mod_naive <- lm(outcome~ treatment, data = dataset)

## tidy(conf.int = TRUE)により推定値と信頼区間をデータフレーム化
mod_naive %>% tidy(conf.int = TRUE)

#共変量調整をしない回帰表
modelsummary (mod_naive)

#背景因子影響をプロット
love.plot(treatment ~ gender + learnig_time_at_home,
          data = dataset, estimand = "ATE",
          abs = TRUE, stats = "mean.diffs", thresholds = c(m = 0.1),
          s.d.denom = "pooled", binary = "std")


#WeightItパッケージ
# 傾向スコアの推定と重みの算出
psmodel_wtit <- weightit(treatment ~ gender + learnig_time_at_home,
                         data = dataset, method = "ps", estimand = "ATE")

# 傾向スコアと重みをもとのデータセットに格納
dataset_iptw_wtit <- dataset %>%
  mutate(ps = psmodel_wtit$ps, weights = psmodel_wtit$weights)

view(dataset_iptw_wtit)

# propensity scoreの分布(調整前)
bal.plot(dataset_iptw_wtit, treat = dataset_iptw_wtit$treatment, var.name = "ps")

#重み付け後の背景因子のバランス
love.plot(treatment ~ gender + learnig_time_at_home,
          data = dataset_iptw_wtit, weights = dataset_iptw_wtit$weights,
          abs = TRUE, stats = "mean.diffs", thresholds = c(m = 0.1),
          s.d.denom = "pooled", binary = "std")

# 重みやデータの情報格納
design_wtit <- svydesign(id = ~ 1, weights = ~ weights, data = dataset_iptw_wtit)

# 重み付けの線形回帰
mod_iptw_wtit <- svyglm(outcome ~ treatment,
                        design = design_wtit, family = gaussian())

# 割合の差の推定値を出力
mod_iptw_wtit %>% tidy(conf.int = TRUE)

#共変量調整をした回帰表
modelsummary (mod_iptw_wtit)

#coefpoot

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

# データを整形
data_naive <- tidy(mod_naive, conf.int = TRUE) %>% filter(term != "(Intercept)")
data_iptw_wtit <- tidy(mod_iptw_wtit, conf.int = TRUE) %>% filter(term != "(Intercept)")

# x軸の範囲を計算
x_range <- range(c(-5, 25, -5, 25))

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


# 介入群別に色分けしたプロット
ggplot(data = dataset, aes(x = treatment, y = outcome, colour = treatment)) + # ten_kukan20データでキャンバス準備
  geom_point(size = 2) +                  # 散布図を描く
  scale_colour_tableau() +
  theme_gray(base_size = 15) + # grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") + # 文字化けしないおまじない
  geom_smooth(method = "lm", formula = treatment ~ outcome) 


# 勉強時間とoutcomeの散布図
ggplot(data = dataset, aes(x = learnig_time_at_home, y = outcome, colour = treatment)) + # ten_kukan20データでキャンバス準備
  geom_point(size = 2) +                  # 散布図を描く
  scale_colour_tableau() +
  theme_gray(base_size = 15) + # grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3")  # 文字化けしないおまじない
