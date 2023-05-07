#Rを使う下準備

#大学のネットワークの場合
options(RCurlOptions = list(proxy = "proxy.kpu.ac.jp:8080"))
Sys.setenv("http_proxy"="http://proxy.kpu.ac.jp:8080")
options(repos=local({ r <- getOption("repos"); r["CRAN"] <- "http://cran.ism.ac.jp"; r }))


#packageの準備
install.packages("here")

library(tidyverse)
library(here)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#フォルダの固定
here::here()
renv::init()

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
#theme_set(theme_gray(base_size = 10, base_family = "IPAGothic"))     # Ubuntu用
#showtext::showtext_auto()                                            # Cloud用
#theme_set(theme_gray(base_size = 10, base_family = "noto"))          # Cloud用

