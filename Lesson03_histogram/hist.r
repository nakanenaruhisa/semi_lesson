#Rを使う下準備

#packageの準備

library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#全部の変数を消す

rm(list = ls())


# 1. サンプルデータの作成 ---------------------------------------------
# set.seed() を使って乱数のシードを固定することで、毎回同じ結果が得られるようにします。
set.seed(123)

# 正規分布（平均0、標準偏差1）に従う1000個の乱数データを作成します。
data <- rnorm(1000, mean = 0, sd = 1)

# データの中身を見せて（対話環境でのみ開く。Rscriptではスキップ）
print(data)

# データフレームに変換(どちらも同じ意味です)

df <- tibble(data = data)
df <- data.frame(data)

# 2. ヒストグラムの作成（頻度表示） -------------------------------
# ggplot2で頻度ベースのヒストグラムを描画します。
ggplot(data = df, aes(x = data)) +
  geom_histogram(breaks = seq(min(data), max(data), length.out = 31),
                 fill = "lightblue", color = "white") +
  labs(title = "ヒストグラム：データの頻度分布",
       x = "値",
       y = "頻度") +
  theme_minimal(base_family = "sans")

# 3. ヒストグラムにカーネル密度推定の曲線を追加 ---------------------
# ggplot2で密度スケールのヒストグラムと密度曲線を重ねます。
ggplot(data = df, aes(x = data)) +
  geom_histogram(aes(y = after_stat(density)),
                 breaks = seq(min(data), max(data), length.out = 31),
                 fill = "lightblue", color = "white") +
  geom_density(adjust = 1, color = "red", linewidth = 1) +
  labs(title = "ヒストグラム：データの密度分布(密度スケール)",
       x = "値",
       y = "密度") +
  theme_minimal(base_family = "sans")
