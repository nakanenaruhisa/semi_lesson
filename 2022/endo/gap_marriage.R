#全部の変数を消す
rm(list=ls())

#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(plotrix)
library(modelsummary)
library(ggrepel)

#datasetの読み込み
read.csv("D:/nakane/genderwagegap.csv")#このフォルダ名は個々人違うので修正すること
read.csv("D:/nakane/marriagerate.csv")

gap <- genderwagegap
marriage <- marriagerate

#gap2019年のデータのみを作る
gap2019 <- gap　%>%
  filter(Year == 2019)#Yearが2019のやつだけ抜き出して

#marriage2019年のデータのみを作る
marriage2019 <- marriage　%>%
  filter(Year == 2019)#yearが2019のやつだけ抜き出して

#dataの結合
gap_marriage <- full_join(gap2019,marriage2019, by= "Country")

#dataの整理（欠損値がある国名を削除)
gap_marriage_na <- gap_marriage[!(is.na(gap_marriage$Value.x) | is.na(gap_marriage$Value.y)),]

#結合したdataの整理（year.yを削除、year.xの名称を変更)
gap_marriage_new <- gap_marriage_na %>% 
  select(Country,Year = Year.x,gap=Value.x,marriage=Value.y)

gapscore <- gap_marriage_new$gap #gapスコア変数を作成
marriagescore <- gap_marriage_new$marriage　#marriageスコア変数を作成

#とりあえず、tableを作ってみる
gap_marriage_new %>% 
  select(gap,marriage) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))#平均と標準偏差つけてtable作って


#男女間賃金格差と粗婚姻率の関係の回帰式を書くよ
reg_gap_marriage <- lm(data = gap_marriage_new, formula = gapscore ~ marriagescore)

#男女間賃金格差と粗婚姻率の回帰式を表にする
reg_gap_marriage %>% tbl_regression(intercept = TRUE)

#gapとmarriageでプロットを書く
ggplot(data = gap_marriage_new) +
  aes(x = gapscore, y = marriagescore)+
  geom_point() +                  # 散布図を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

as_tibble(gap_marriage_new)

#gapとmarriageで国名をつけてプロットを書く
ggplot(data = gap_marriage_new) +
  aes(x = gapscore, y = marriagescore, label = Country)+
  geom_point() +
  scale_colour_tableau()+
  geom_text_repel()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#gapとmarriageで国名をつけてプロットを回帰直線をつけて書く
ggplot(data = gap_marriage_new) +
  aes(x = gapscore, y = marriagescore, label = Country)+
  geom_point() +  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  geom_text_repel()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
