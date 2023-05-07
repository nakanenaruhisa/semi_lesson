#ggplotのインストール
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(cli)

#全部の変数を消す
rm(list=ls())

#irisdatasetを読み込む
str(iris)
irs <- (iris)
view(iris)

# ggplotのキャンバスを作成
# あわせてdataとaesも指定
irisplot0<-
  ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length))

# 書き出してみる
irisplot0

# ggplot2ではレイヤーなどを重ねるのに `+` を使います
irisplot1 <- irisplot0 + 
  layer(geom = "point", stat = "identity", position = "identity")
# 描いてみる
irisplot1

# aesにcolor = Speciesを追加
irisplot3 <- 
  ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point()
# 出力
irisplot3