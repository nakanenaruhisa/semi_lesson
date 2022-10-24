packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_bw(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

library(lme4)

#データセットを読み込む
load("~/Library/CloudStorage/OneDrive-個人用/R/semi_lesson/multilevel/st_df.rda")

data <- st_df
view(data)

#X(勉強時間）とY（成績）のプロット書いてみて
ggplot(data = data) +     # dataデータでキャンバス準備
  aes(x = X, y = Y)+ 
  geom_point() +# 散布図を描く
  geom_smooth(method = "lm")+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#単回帰式を作るよ
reg_data <- data %>% 
  lm(formula = X ~ Y) 

#単回帰の票を書くよ
reg_data %>% 
  tbl_regression(intercept = TRUE,
                 pvalue_fun = function(reg_data) style_pvalue(reg_data, digits = 2),
                 estimate_fun = function(reg_data) style_ratio(reg_data, digits = 3)) %>% 
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる

#学校別を考慮したマルチレベル分析のプロットを書く
library(lattice)
xyplot(Y~ X, data = data, group = school, pch = 19, type = c("p","r"), auto.key = list(pch = 19, corner=c(0,1), border = T, padding.text = 1.5, columns = 1), par.settings = simpleTheme(pch = 16))

#学校別を考慮した単回帰式を作るよ
multireg_data <- lmList(Y ~ X|school,data = data) 

#学校別を考慮した単回帰票を書くよ
summary(multireg_data)


#学校別を考慮したマルチレベル分析をするよ
fitm1 <- lmer(X ~ Y + (Y|school),data = data)
summary(fitm1)

