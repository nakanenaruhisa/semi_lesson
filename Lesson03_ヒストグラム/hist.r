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



# 1. サンプルデータの作成 ---------------------------------------------
# set.seed() を使って乱数のシードを固定することで、毎回同じ結果が得られるようにします。
set.seed(123)

# 正規分布（平均0、標準偏差1）に従う1000個の乱数データを作成します。
data <- rnorm(1000, mean = 0, sd = 1)

#データの中身を見せて
View(data)

# 2. ヒストグラムの作成（頻度表示） -------------------------------
# hist() 関数を使ってヒストグラムを描画します。
# breaks=30 により、30個のビン（区間）に分割して表示します。
# main, xlab はそれぞれタイトルとX軸のラベルを設定しています。
# col と border で棒の色と境界線の色を指定しています。
hist(data,
     breaks = 30,
     main = "ヒストグラム：データの頻度分布",
     xlab = "値",
     col = "lightblue",
     border = "white")

# 3. カーネル密度推定の計算 ------------------------------------------
# density() 関数で、カーネル密度推定（滑らかな分布の推定）を計算します。
dens2 <- density(data)

# 4. ヒストグラムにカーネル密度推定の曲線を追加 ---------------------
# lines() 関数を使って、カーネル密度推定の曲線を追加します。
# col で曲線の色を指定し、lwd で線の太さを設定しています。
lines2(dens2,
      col = "red",
      lwd = 2)

# 5. ヒストグラムから特定のゾーン（平均値-1と+２の周辺）のサンプルが減った場合をつくって
# ヒストグラムを再描画する例 ------------------------------------------
# データの中から、値が0.5未満のサンプルを抽出します。


data2 <- data[data < 0.5]



# 6. ヒストグラムを密度表示（freq = FALSE）にして再描画する例 --------
# hist() 関数の freq = FALSE により、ヒストグラムを密度表示に変更します。
# また、カーネル密度推定の曲線も再度追加しています。
hist(data,
     breaks = 30,
     main = "ヒストグラム：データの密度分布",
     xlab = "値",
     col = "lightblue",
     border = "white",
     freq = FALSE)
lines(dens,
      col = "red",
      lwd = 2)

hist(data2,
     breaks = 30,
     main = "ヒストグラム：データの密度分布",
     xlab = "値",
     col = "lightblue",
     border = "white",
     freq = FALSE)
lines(dens2,
      col = "red",
      lwd = 2)
