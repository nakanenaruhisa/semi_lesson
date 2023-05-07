#全部の変数を消す
rm(list=ls())

#packageの準備
library(tidyverse)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#フォルダの固定
here::here()

#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_bw(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#datasetを読み込む
anscombe <- read_csv("anscombe/anscombe.csv")

#datasetを格納
anscombe %>% as_tibble()

#datasetが1に所属するものを抽出
anscombe %>% 
  filter(dataset == 1)

#xとyの列だけ抜き出す
anscombe %>% 
  select(x,y)

#x,y,datasetを変数化する
x <- (anscombe$x)
y <- (anscombe$y)
dataset <- (anscombe$dataset)

#datasetごとにx,yの平均値を出す
anscombe %>% 
  group_by(dataset) %>%
  summarise(
    xの平均値 = mean(x),
    xの標準偏差 = sd(x),
    yの平均値 = mean(y),
    yの標準偏差 = sd(y),
    )

#記述統計量をテーブルで出力する
anscombe %>% 
  tbl_summary()

#記述統計量をdatasetごとにテーブルで出力する
anscombe %>% 
  tbl_summary(by = dataset,
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる


#相関行列
ggpairs(anscombe)

#散布図を書く
anscombe %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(shape = 0) + 
  geom_smooth(method = "lm", se = TRUE) + 
  facet_wrap(~dataset)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#dataset1のxとyの回帰式を書くよ
dataset1 <- anscombe %>% 
  filter(dataset == 1) %>% 
  lm(formula = x ~ y) 

#dataset1の回帰式を表にする
dataset1 %>% 
  tbl_regression(intercept = TRUE) %>% 
  modify_header(label ~ "dataset1") # ""の部分には好きな文字列を入れられる

#dataset2のxとyの回帰式を書くよ
dataset2 <- anscombe %>% 
  filter(dataset == 2) %>% 
  lm(formula = x ~ y) 

#dataset2の回帰式を表にする
dataset2 %>% 
  tbl_regression(intercept = TRUE) %>% 
  modify_header(label ~ "dataset2") # ""の部分には好きな文字列を入れられる

