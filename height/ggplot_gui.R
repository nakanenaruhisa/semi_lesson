#Rを使う下準備

#packageの準備
install.packages("ggplotgui", dependencies = TRUE)
install.packages("esquisse", dependencies = TRUE)
install.packages("ggraptR", dependencies = TRUE)
library(ggplotgui)
library(esquisse)
library(ggraptR)
library(readr)

#全部の変数を消す
rm(list=ls())

#データセットを読み込む
ten_kukan100 <- read_csv("height_weight/ten_kukan100.csv")
as.character(ten_kukan100$`﻿ID`)

#ggplotguiの実行
ggplot_shiny(data=ten_kukan100)

# esquisseの実行
esquisser(data=ten_kukan100)

ggraptR()
