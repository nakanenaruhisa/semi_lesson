#packageの準備
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
data(package="lme4") #パッケージのサンプルを見る

head(cbpp)
data <- cbpp
view(data)

#incidenceと集団サイズのプロット書いてみて
ggplot(data = data) +     # dataデータでキャンバス準備
  aes(x = size, y = incidence)+ 
  geom_point() +# 散布図を描く
  geom_smooth(method = "lm")+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#単回帰式を作るよ
reg_data <- data %>% 
  lm(formula = incidence ~ size) 

#単回帰の票を書くよ
reg_data %>% 
  tbl_regression(intercept = TRUE,
                 pvalue_fun = function(reg_data) style_pvalue(reg_data, digits = 2),
                 estimate_fun = function(reg_data) style_ratio(reg_data, digits = 3)) %>% 
  modify_header(label ~ "病気x集団サイズ") # ""の部分には好きな文字列を入れられる

#調査時期を考慮したマルチレベル分析のプロットを書く
library(lattice)
xyplot(incidence  ~ size, data = data, group = period, pch = 19, type = c("p","r"), auto.key = list(pch = 19, corner=c(0,1), border = T, padding.text = 1.5, columns = 1), par.settings = simpleTheme(pch = 16))

#調査時期を考慮した単回帰式を作るよ
multireg_data <- lmList(incidence ~ size|period,data = data) 

#調査時期を考慮した単回帰票を書くよ
summary(multireg_data)


#調査時期を考慮したマルチレベル分析をするよ
fitm1 <- lmer(incidence ~ size + (size|period),data = data)
summary(fitm1)
