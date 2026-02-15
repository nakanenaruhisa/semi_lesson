#Rを使う下準備
#packageの準備
library(stringr)
library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(plotrix)
library(modelsummary)
library(car)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#dataをインポート
logsticreg_sample <- read_excel("logisticreg/logsticreg_sample.xlsx")
data <- logsticreg_sample

#datasetの要約表
tbl_summary(data)

logireg <- glm(data = data, regidencial_status ~ age+gender+nintei_grade+family_status+public_assistance+public_pension,family=binomial(link="logit"))

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
modelsummary (logireg)



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


#居住状態と年齢のプロット書いてみて
ggplot(data = data) +
  aes(x = age, y = regidencial_status)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#居住状態と同居家族のプロット書いてみて
ggplot(data = data) +
  aes(x = family_status, y = regidencial_status)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_jitter(height=0.1, width =0.1) +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

