#Rを使う下準備

#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(broom)


#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))


#全部の変数を消す
rm(list=ls())


#datasetを読み込む


#変数を箱に入れるよ
gdp <- (gdp_wage$gdp)
averagewage <- (gdp_wage$averagewage)
country <- (gdp_wage$country)


#データセット全体のテーブルを自動でつくる
gdp_wage %>% 
  tbl_summary()


#記述統計量にラベルを付けてテーブルで出力する
gdp_wage %>% 
  select(gdp,averagewage) %>% 
  tbl_summary(label = list(gdp ~ "GDP", averagewage ~ "平均賃金", #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
                           statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                           digits = all_continuous() ~ 1)) #数値の部分が小数点第y位の部分の値

#GDPと平均賃金のプロット
  ggplot(data = gdp_wage) +     
    aes(x = gdp, y = averagewage)+ # gdpをx,y軸にaveragewage
    geom_point() +                  # 散布図を描く
    scale_colour_tableau()+
    theme_gray(base_size = 15) + #grayテーマで
    theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
  

#スコア変数の作成
gdpscore <- gdp_wage$gdp 
averagewagescore <- gdp_wage$averagewage


#回帰式を書く
reg_gdp_wage <- lm(data = gdp_wage, formula = gdpscore ~ averagewagescore)


#回帰式を表にする
gdp_wage %>% tbl_regression(intercept = TRUE)

library(modelsummary)
modelsummary(reg_gdp_wage)

#外れ値を取り除く
gdp_wage_new2 <- gdp_wage %>% 
  filter(gdp != 136240) %>% 
  filter(gdp != 108312)

#プロットを回帰直線をつけて書く
ggplot(data = gdp_wage_new2) +
  aes(x = gdp, y = averagewage,  label = country)+
  geom_point() +  # 散布図を描く
  geom_smooth(method = "lm")+
  geom_text(aes(y = gdp, label = country), size = 3, vjust = 4)+#回帰直線を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_set(theme_gray(base_size = 10, base_family = "Meiryo"))        # Windows用
