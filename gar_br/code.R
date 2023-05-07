#package読み込む
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#フォルダの固定
here::here()

#gap_brのデータセットを読み込む
gap_br <- read_csv("gar_br/gap_br.csv")

view(gap_br)

#gdp_oecdのデータセットを読み込む
# dplyr:select()を使って「行」を抜き出す方法
# select(df, ID, Name, Pref, Score)でもOK
# 項目を「*** = 変数」で一覧に表示される変数も変更できる（原則半角英数字）
gdp_oecd <- read_csv("gar_br/gdp_oecd.csv")

gdp_oecd %>% tibble::as_tibble()

gdp_oecd_jpn <- gdp_oecd %>% 
  select(`﻿LOCATION`, year = TIME, gdp = Value)

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

<<<<<<< Updated upstream
#一?テーブルにしておく
gap_r_gdp %>%
  tbselect(-year) %>%
  l_summary()

#pgdとbrで散布図を書く
=======
#GDPとbirthrateで散布図を書く(回帰直線なし)
>>>>>>> Stashed changes
ggplot(data = gap_br_gdp)+
  aes( x= gdp, y = br, label=year) +
  geom_point() +                  # 散布図を描く
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#GDPとbirthrateで散布図を書く（回帰直線あり）
ggplot(data = gap_br_gdp)+
  aes( x= gdp, y = br, label=year) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

<<<<<<< Updated upstream
#gapとbr?で散布図を書く
=======
#gapとbrで散布図を書く（回帰直線なし）
>>>>>>> Stashed changes
ggplot(data = gap_br_gdp)+
  aes( x= gap, y = br, label=year) +
  geom_point() +                  # 散布図を描く
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#gapとbrで散布図を書く（回帰直線あり）
ggplot(data = gap_br_gdp)+
  aes( x= gap, y = br, label=year) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
