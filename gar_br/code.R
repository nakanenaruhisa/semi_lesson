#package読み込む
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#gap_brのデータセットを読み込む
gap_br <- read_csv("gap_br.csv")

gap_br %>% tibble::as_tibble()

#gdp_oecdのデータセットを読み込む
# dplyr:select()を使って「行」を抜き出す方法
# select(df, ID, Name, Pref, Score)でもOK
# 項目を「*** = 変数」で一覧に表示される変数も変更できる（原則半角英数字）
gdp_oecd <- read_csv("gdp_oecd.csv")

gdp_oecd %>% tibble::as_tibble()

gdp_oecd_jpn <- gdp_oecd %>% 
  select(`LOCATION`, year = TIME, gdp = Value)

#JPNだけを抜き出す
gdp_oecd_jpn <- gdp_oecd_jpn %>% 
  filter(gdp_oecd_jpn$`﻿LOCATION` == "JPN")

#データセットを結合する
gap_br_gdp <- merge(gdp_oecd_jpn, gap_br, by = "year") 

#LOCATIONを取り除く
gap_br_gdp <- gap_br_gdp %>%
  select(-`﻿LOCATION`)

#変数を箱に入れる
gap <- gap_br_gdp$gap
gdp <- gap_br_gdp$gdp
br <- gap_br_gdp$birthrate

#一?テーブルにしておく
gap_r_gdp %>%
  tbselect(-year) %>%
  l_summary()

#pgdとbrで散布図を書く
ggplot(data = gap_br_gdp)+
  aes( x= gdp, y = birthrate, label=year) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#gapとbr?で散布図を書く
ggplot(data = gap_br_gdp)+
  aes( x= gdp, y = gap, label=year) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#gdpの折れ線グラフを書く
ggplot(data = gap_br_gdp)+
  (aes(x = year, y = gdp))+
  geom_line(color = "red")+
  geom_point(color = "red") +
  scale_colour_tableau()+
  labs(color = "year")+ #凡例のタイトルを指定
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

