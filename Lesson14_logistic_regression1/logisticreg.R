#Rを使う下準備
#全部の変数を消す
rm(list=ls())

#packageの準備
library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(plotrix)
library(modelsummary)
library(car)

# 作業ディレクトリの確認
getwd()
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
if (grepl("^Lesson[0-9]", basename(getwd())) || basename(getwd()) %in% c("folder_format", "applied")) setwd("..")

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#dataをインポート
logsticreg_sample <- read_excel("Lesson14_logistic_regression1/logsticreg_sample.xlsx")
data <- logsticreg_sample

view(data)

#dataにラベルをつける
data <- data %>%
  mutate(
    regidencial_status = factor(regidencial_status, labels = c("在宅", "施設")),
    gender = factor(gender, labels = c("男性", "女性")),
    nintei_grade = as.numeric(nintei_grade), # 連続変数として扱う
    family_status = factor(family_status, labels = c("同居家族なし", "同居家族あり")),
    public_assistance = factor(public_assistance, labels = c("介護保険利用なし", "介護保険利用あり")),
    public_pension = factor(public_pension, labels = c("公的年金なし", "公的年金あり"))
  )

#datasetの要約表
tbl_summary(data)

#regidencial_statusをfactor型に変換
data$regidencial_status <- as.factor(data$regidencial_status)

#居住状態と年齢の単回帰のプロット書いてみて
ggplot(data = data) +
  aes(x = age, y = regidencial_status)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#居住状態と同居家族の単回帰のプロット書いてみて
ggplot(data = data) +
  aes(x = family_status, y = regidencial_status)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_jitter(height=0.1, width =0.1) +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#居住状態を従属変数としてロジスティック回帰分析
logireg <- glm(data = data, regidencial_status ~ age +gender+nintei_grade+family_status+public_assistance+public_pension,family=binomial(link="logit"))

# 多重共線性のチェック
vif(logireg)
#オッズ比の計算
exp(logireg$coefficients)
summary(logireg)

#居住状態を従属変数としてロジスティック回帰分析
logireg <- list()
logireg[['Model 1']] <- glm(data = data, regidencial_status ~ age+gender,family=binomial(link="logit"))
logireg[['Model 2']] <- glm(data = data, regidencial_status ~ age+gender+nintei_grade,family=binomial(link="logit"))
logireg[['Model 3']] <- glm(data = data, regidencial_status ~ age+gender+nintei_grade+family_status,family=binomial(link="logit"))
logireg[['Model 4']] <- glm(data = data, regidencial_status ~ age+gender+nintei_grade+family_status+public_assistance+public_pension,family=binomial(link="logit"))

# 決定係数を含むモデル要約表を表示
modelsummary(logireg,
            statistic = "conf.int",
            stars = TRUE,
            gof_map = c("n", "r2", "aic", "bic"),
            title = "ロジスティック回帰分析の結果")



install.packages("coefplot")
library(coefplot)

#ggcoef
logireg <- glm(data = data, regidencial_status ~ age + gender + nintei_grade+family_status+public_assistance+public_pension,
family=binomial(link="logit"))

ggcoef(logireg,
       mapping = aes_string(y = "term", x = "estimate"),
       conf.int = TRUE,
       conf.level = 0.95,
       exponentiate = FALSE,
       exclude_intercept = TRUE,
       vline = TRUE,
       vline_color = "red",
       vline_linetype =  "solid",
       errorbar_color = "black",
       errorbar_height = .25
       )



