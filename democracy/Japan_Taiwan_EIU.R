#--------------------ライブラリの読み込み--------------------
#データ操作・可視化に必要なパッケージを読み込む
library(tidyverse)#tidyverse: データ操作全般のパッケージ群
library(readxl)
library(gtsummary)#gtsummary: データの要約統計量を作成するパッケージ
library(ggplot2)#ggplot2: グラフ描画のためのパッケージ
library(ggthemes)#ggthemes: グラフのテーマを設定するパッケージ

#--------------------データの準備と前処理--------------------
#EIU2020データセットを読み込む
EIU2020 <- read_csv("democracy/EIU2020.csv")
#EIU2020データセットをtibble形式に変換して確認
as_tibble(EIU2020)

#日本と台湾のデータのみを抽出
japan_taiwan <- EIU2020 %>%
  filter(country %in% c("Taiwan","Japan"))

#--------------------記述統計の作成--------------------
#国別の民主主義スコアの要約統計量を作成
#mean: 平均値, sd: 標準偏差を表示
japan_taiwan %>% 
  select(country, eiu, year) %>% 
  tbl_summary(
    label = list(
      country ~ "国名",
      eiu ~ "民主主義スコア",
      year ~ "年"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    by = country,
    digits = all_continuous() ~ 1
  )

#--------------------グラフの作成--------------------
#日本と台湾の民主主義スコアの経年変化を折れ線グラフで可視化
ggplot(data = japan_taiwan) +
  aes(x = year, y = eiu, colour = country) + #x軸:年, y軸:民主主義スコア, 色分け:国
  geom_line() +                              #折れ線グラフを描画
  geom_point() +                             #データポイントを表示
  scale_colour_tableau() +                   #Tableauの配色を使用
  labs(
    title = "日本と台湾の民主主義スコアの推移",
    x = "年",
    y = "民主主義スコア",
    color = "国名"
  ) +
  theme_gray(
    base_size = 15,                          #基本フォントサイズを15に設定
    base_family = "HiraKakuPro-W3"          #日本語フォントを設定
  )

