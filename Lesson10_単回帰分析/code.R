#作業環境をクリーンにするため、既存の変数をすべて削除
rm(list=ls())

#必要なパッケージを読み込む - データ操作と可視化に必要なライブラリをインポート
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gtsummary)
library(readr)
library(tidyplots)
library(modelsummary)


#賃金格差と出生率のデータを読み込む
gap_br <- read_csv("Lesson10_単回帰分析/gap_br.csv")

#データの内容を確認
view(gap_br)

#賃金格差と出生率の分布を確認するためのヒストグラム
hist(gap_br$gap)
hist(gap_br$birthrate)

#賃金格差と出生率の関係を可視化する散布図(年を表示、回帰直線なし)
ggplot(data = gap_br)+
  aes( x= gap, y = birthrate, label=year) +
  geom_point() +                  
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()
  #theme_igray(base_family = "HiraKakuPro-W3")

#賃金格差と出生率の関係を可視化する散布図(年を表示、回帰直線あり,tidyplot)
gap_br %>% 
  tidyplot(x = gap, y = birthrate) %>% 
  add_data_points(size =1) %>%
  add_data_labels(label=year,label_position = "above",fontsize = 10)%>%
  add(ggplot2::geom_smooth(method = "lm"))%>%
  adjust_size(width = 210, height = 120) %>%
  adjust_font(fontsize = 18, family = NULL, face = NULL, color = "black")


#賃金格差が出生率に与える影響を分析する単回帰分析
gap_br_reg <- lm(data = gap_br, formula = birthrate ~ gap)
summary(gap_br_reg)

#回帰表テーブル
modelsummary (gap_br_reg)

#ここから新しい作業

#OECDのGDPデータを読み込む
gdp_oecd <- read_csv("Lesson10_単回帰分析/gdp_oecd.csv")

#データフレームの内容を確認
view(gdp_oecd)

#必要な列を選択し、列名を変更
gdp_oecd <- gdp_oecd %>%
  select(country =`﻿LOCATION`, year = TIME, GDP = Value) 

view(gdp_oecd)

#日本のデータのみを抽出
gdp_oecd_jpn <- gdp_oecd %>% 
  filter(gdp_oecd$country == "JPN")

#日本のデータフレームの内容を確認
view(gdp_oecd_jpn)

#賃金格差・出生率データとGDPデータを結合
gap_br_gdp <- merge(gdp_oecd_jpn, gap_br, by = "year") 

#結合したデータフレームを確認
view(gap_br_gdp)

#不要な列を削除
gap_br_gdp <- gap_br_gdp %>%
  select(-country)

#削除したデータフレームを確認
view(gap_br_gdp)


#分析用の変数を作成
gap <- gap_br_gdp$gap
GDP <- gap_br_gdp$GDP
br <- gap_br_gdp$birthrate

#データの要約統計量を表示
gap_br_gdp %>%
  select(-year) %>%
  tbl_summary()

#GDPと出生率の関係を可視化する散布図(年を表示、回帰直線なし)
ggplot(data = gap_br_gdp)+
  aes( x= GDP, y = br, label=year) +
  geom_point() +                  
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()
  #theme_igray(base_family = "HiraKakuPro-W3")

#x軸にGDP,y軸に賃金ギャップを可視化する散布図
ggplot(data = gap_br_gdp)+
  aes( x= GDP, y = gap, label=year) +
  geom_point() +                  
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()
  #theme_igray(base_family = "HiraKakuPro-W3")

#GDPと出生率の関係を可視化する散布図(年を表示、回帰直線あり)
ggplot(data = gap_br_gdp)+
  aes( x= GDP, y = br, label=year) +
  geom_point() +                  
  geom_smooth(method = "lm")+
  geom_text_repel(max.overlaps = 6)+
  theme_igray(base_size = 15) + 
  scale_colour_tableau()+
  theme_igray(base_family = "HiraKakuPro-W3")

