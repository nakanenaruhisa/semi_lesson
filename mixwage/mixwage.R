#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#datasetの読み込み
library(readr)

mixwagedata <- read_csv("mixwage/mixwagedata.csv",col_types = cols(`2021` = col_number(), 
                                                                   `2022` = col_number()))

#政令市をカテゴリ変数に変換
mixwagedata <- mixwagedata %>% 
  mutate(city_c = factor(city_designated,levels = 0:1,labels = c("政令市なし都道府県","政令市あり都道府県")))

mixwagedata %>% 
tbl_summary(by = city_c)

#政令市と非政令市の最低賃金の箱ひげ図を書いて比較してみて
ggplot(data = mixwagedata) +     # tenkukan20データでキャンバス準備
  aes(x = city_c, y = `2022`, fill = city_c)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_boxplot() +                  # はこひげ図を描く
  xlab("都市規模") + ylab("最低賃金平均") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#政令市と非政令市の最低賃金の雨雲図を書いて比較してみて

library(ggdist)
library(tidyquant)

ggplot(data = mixwagedata) +     # tenkukan20データでキャンバス準備
  aes(x = city_c, y = `2022`, fill = city_c)+ # height,weight列をx,y軸にmapping,sexごとに色分け
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
  #
  ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25)+
  xlab("都市規模") + ylab("最低賃金平均") +
  scale_fill_tq()+
  theme_tq()+
  theme_gray(base_family = "HiraKakuPro-W3") +#文字化けしないおまじない
  coord_flip()


#政令市と非政令市の最低賃金の比較をseエラーバーをつけて書く（ggplot）
ggplot(data = mixwagedata)+
  aes(x = city_c, y = `2022`, color = city_c)+
  stat_summary(aes(x = city_c),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("都市規模") + ylab("最低賃金平均") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
