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
 
#theme_set(theme_gray(base_size = 10, base_family = "Meiryo"))        # Windows用

#chatGPTを起動する
library(shiny)
library(chattr)
chattr_app()
