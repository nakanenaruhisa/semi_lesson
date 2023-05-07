#クロス集計とモザイクプロット

#大学のネットワークの場合
options(RCurlOptions = list(proxy = "proxy.kpu.ac.jp:8080"))
Sys.setenv("http_proxy"="http://proxy.kpu.ac.jp:8080")
options(repos=local({ r <- getOption("repos"); r["CRAN"] <- "http://cran.ism.ac.jp"; r }))

#packageの準備
install.packages("devtools")
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(ggmosaic)

#フォルダの固定
here::here()

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))


#datasetを読み込む
remarrigewill_a <- read_csv("~/OneDrive/R/semi_lesson/cross_mosaic/remarrigewill_a.csv")

#変数を箱に入れるよ
gender <- (remarrigewill_a$gender)
remarrige_will <- (remarrigewill_a$remarrige_will)

#genderと再婚意思をカテゴリ変数に変換
remarrigewill<- remarrigewill_a %>% 
  mutate(gender_c = factor(gender,
                        levels = 1:2,labels = c("男性","女性")))

remarrigewill_c <- remarrigewill %>% 
  mutate(remarrige_will_c = factor(remarrige_will,
                       levels = 1:3,
                       labels = c("機会があれば再婚したい","当面は再婚の意思はない","再婚する意思はない")))

#データセット全体のテーブルを自動でつくる
remarrigewill_c %>% 
  tbl_summary()

#記述統計量にラベルを付けてテーブルで出力する
remarrigewill_c %>% 
  select(gender_c,remarrige_will_c) %>% 
  tbl_summary(label = list(gender_c ~ "性別",
                           remarrige_will_c ~ "再婚意思"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#せっかくだから男女別に書いてみる
remarrigewill_c %>% 
  select(gender_c,remarrige_will_c) %>% 
  tbl_summary(label = list(gender_c ~ "性別",
                           remarrige_will_c ~ "再婚意思"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = gender_c,
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#モザイクプロットを書く
ggplot(remarrigewill_c) +
  geom_mosaic(aes(x = product(gender_c, remarrige_will_c), fill = gender_c),na.rm = TRUE)+
  labs(x = "remarrige_will_c", y = "gender_c", title = "再婚意思の男女比較") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
