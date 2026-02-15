#クロス集計とモザイクプロット

#packageの準備
install.packages("devtools")
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(ggmosaic)

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

# データをカテゴリー変数でクロス集計
table_data <- table(remarrigewill_c$gender_c, remarrigewill_c$remarrige_will_c)

# χ二乗検定の実行
chisq_result <- chisq.test(table_data)

# χ二乗検定結果の表示
print(chisq_result)

#せっかくだから男女別に書いてみる
table_data　<- remarrigewill_c %>% 
  select(gender_c,remarrige_will_c) %>% 
  tbl_summary(label = list(gender_c ~ "性別",
                           remarrige_will_c ~ "再婚意思"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = gender_c,
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

# add_p()でχ二乗検定を行い、その結果を表に追加
table_data_with_p <- table_data %>% 
  add_p(test = list(remarrige_will_c ~ "chisq.test"))

table_data_with_p

#モザイクプロットを書く
ggplot(remarrigewill_c) +
  geom_mosaic(aes(x = product(gender_c, remarrige_will_c), fill = gender_c),na.rm = TRUE)+
  labs(x = "remarrige_will_c", y = "gender_c", title = "再婚意思の男女比較") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
