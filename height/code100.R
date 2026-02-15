#Rを使う下準備

#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(broom)
library(ggdist)

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


#身長の平均値を出してみて

#体重の平均値を出してみて

#男性の身長の平均値を出してみて


#女性の身長の平均値を出してみて


#身長の標準偏差出してみて

#体重の標準偏差出してみて


#標準誤差を出すためにパッケージを追加する。


#身長の標準誤差を出す


#体重の標準誤差を出す


#男性の身長の標準偏差出してみて


#女性の身長の標準偏差出してみて


#データセット全体のテーブルを自動でつくる


#記述統計量にラベルを付けてテーブルで出力する


#せっかくだから男女別に書いてみる

#身長と体重のプロット書いてみて


#身長と体重の性別ごとのプロット書いてみて


#男性と女性の身長の箱ひげ図を書いて比較してみて


#男性と女性の身長の平均の比較をseエラーバーをつけて書く（ggplot）


#男性と女性の身長の雨雲図を書いて比較してみて
ggplot(data = ten_kukan100) +     # tenkukan20データでキャンバス準備
  aes(x = sex_c, y = height, fill = sex_c)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  ggdist::stat_halfeye(
    adjust =0.5,
    justification = -.2,
    .width = 0,
    point_colour = NA)+
  # はこひげ図を描く
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5) +
  ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25)+
  xlab("sex") + ylab("height") +
  scale_fill_tq()+
  theme_tq()+
  theme_gray(base_family = "HiraKakuPro-W3") +#文字化けしないおまじない
  coord_flip()

