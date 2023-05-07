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

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

p2 <- 
  iris %>% 
  ggpairs(mapping = aes(color = Species), #Speciesでグループ分け
          diag=list(continuous="barDiag"),
          # diagの連続×連続を積み上げヒストグラムに変更
          lower=list(continuous="smooth", combo = "facetdensity"))
# lowerの連続×連続を回帰線に，連続×離散を密度図に変更)
p2
