#Rを使う準備

library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#dataをインポート
data <- read_csv("multireg_3/glmm.csv")

#flmはスコア、idは被験者ID、periodは観測時点、stageは症状の進行度

view(data)

#混合効果モデルのパッケージ
library(lmerTest)
library(nlme)

#混合効果モデルの多重比較
library(multcomp)

#多重比較のためのパッケージ
library(multcomp)

#id, period, stageをカテゴリ変数に変更
data$id <- factor(data$id)
data$period <- factor(data$period)
data$stage <- factor(data$stage)

# periodoの基準を bl に設定
data$period <- relevel(data$period, ref="bl") 

# stageの基準を IV に設定
data$stage <- relevel(data$stage, ref="IV") 

#widedataに変換
wide_data <- tidyr::spread(data, key=period, value=fim)
View(wide_data)

#従属変数の分布を確認する（正規分布か、ポアソン分布か？）
ggplot(data = wide_data) +
  aes(x =bl ) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")

# 基本的なグラフの設定
g1 <- ggplot2::ggplot(data, aes(x = period, y = fim)) +
  scale_x_discrete(
    "観測時点", 
    labels = c(
      "bl" = "ベースライン", 
      "w3" = "3週後",
      "w6" = "6週後"
    )
  )

# ジッターの追加とテーマ性の設定
g2 <- g1 +
  geom_jitter(height = 0, width = 0.1, size = 2) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# グラフの出力
print(g2)

#stage（症状の進行度）とID（個人差）を追加
g3 <- g1 + 
  geom_jitter(height = 0, width = 0.1, size = 3, aes(colour = stage)) +
  geom_text(mapping = aes(label = id), vjust = 1.5) 

print(g3)

# 基本的なグラフの設定とステージごとの色分け
g3 <- g1 + 
  geom_jitter(aes(colour = stage), height = 0, width = 0, size = 3) +
  geom_text(mapping = aes(label = ""), vjust = 1.5)

# ステージの色に合わせて線を追加
g4 <- g3 + 
  geom_line(aes(group = id, colour = stage), linewidth = 1)

print(g4)

data2 <- subset(
  data,
  id != 12 & id != 16 & period != "bl"
)
summary(data2)

#data2を使用してグラフを作成
g4 <- ggplot(data2, aes(x = period, y = fim, group = id, colour = id)) +
  geom_line() +
  scale_x_discrete(
    "観測時点", 
    labels = c(
      "w3" = "3週後", 
      "w6" = "6週後"
    )
  )

print(g4)

# widedataの行（12行目と16行目）を除外して新しいデータセットを作成
wide_data2 <- wide_data[c(-12, -16), ]
View(wide_data2)

#3wと6wの差を検定
t.test(wide_data2$w6 - wide_data2$w3)

