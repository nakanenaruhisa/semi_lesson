#Rを使う下準備
#全部の変数を消す
rm(list=ls())
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#datasetを読み込む
statics_intro <- read_csv("semi_lesson/pass_time/statics_intro.csv")
#変数を箱に入れるよ
gender <- (statics_intro$gender)
depart <- (statics_intro$department)
time <- (statics_intro$time)

#ジェンダー・学科をカテゴリ変数に変換
statics_intro <- statics_intro %>% 
  mutate(性別 = factor(gender,levels = 1:2,labels = c("男性","女性"))) %>% 
  mutate(学科 = factor(depart,levels = 1:2,labels = c("公共","福社")))

#データセット全体のテーブルを自動でつくる
statics_intro %>% 
  tbl_summary()

#変数を絞ってテーブルで出力する
statics_intro %>% 
  select(ID,性別,学科,time) %>% 
  tbl_summary() 

#せっかくだから男女別に書いてみる
statics_intro %>% 
  select(ID,性別,学科,time) %>% 
  tbl_summary(by = 性別,
              digits = all_continuous() ~ 1) 

#せっかくだから学科別に書いてみる
statics_intro %>% 
  select(ID,性別,学科,time) %>% 
  tbl_summary(by = 学科,
              digits = all_continuous() ~ 1) 
