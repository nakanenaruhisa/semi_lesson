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
View(data)

# 2. ヒストグラムの作成（頻度表示） -------------------------------
par(family = "sans")

# hist() 関数を使ってヒストグラムを描画します。
hist(
  data,
  breaks = 30,
  main = "ヒストグラム：データの頻度分布",
  xlab = "値",
  col = "lightblue",
  border = "white",
  family = "MS Gothic" # 文字化け防止
)

# 3. カーネル密度推定の計算 ------------------------------------------
# density() 関数で、カーネル密度推定（滑らかな分布の推定）を計算します。
dens2 <- density(data, adjust = 1) # adjustパラメータでなめらかさを調整

# 4. ヒストグラムにカーネル密度推定の曲線を追加 ---------------------
# hist()のfreq=FALSEにしない場合、yのスケールが合わずフラットになる
# 密度で重ねる場合はhist()のfreq=FALSEを忘れずに
hist(
  data,
  breaks = 30,
  main = "ヒストグラム：データの密度分布(密度スケール)",
  xlab = "値",
  col = "lightblue",
  border = "white",
  freq = FALSE,
  family = "MS Gothic"
)
lines(dens2, col = "red", lwd = 2)
