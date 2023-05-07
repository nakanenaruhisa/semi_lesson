#Rを使う下準備

#大学のネットワークの場合
options(RCurlOptions = list(proxy = "proxy.kpu.ac.jp:8080"))
Sys.setenv("http_proxy"="http://proxy.kpu.ac.jp:8080")
options(repos=local({ r <- getOption("repos"); r["CRAN"] <- "http://cran.ism.ac.jp"; r }))


#packageの準備
install.packages("here")
install.packages("datasauRus")

library(tidyverse)
library(here)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(datasauRus)


#フォルダの固定
here::here()

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#
suppressPackageStartupMessages(library(dplyr))
  datasaurus_dozen %>% 
    group_by(dataset) %>% 
    summarize(
      mean_x    = mean(x),
      mean_y    = mean(y),
      std_dev_x = sd(x),
      std_dev_y = sd(y),
      corr_x_y  = cor(x, y)
    )

view(datasaurus_dozen
     )



ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
    geom_point()+
    theme_void()+
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol = 3)

## Loading required namespace: ggplot2

#身長と体重のプロット書いてみて
datasaurus_dozen %>% 
  (dataset = "dino") %>%
  
ggplot(data = datasaurus_dozen) +
  aes(x = x, y = y) +
  geom_point() +
  scale_color_brewer(palette = "Set1") + # 代替のカラースケール
  theme_gray(base_size = 15) + # グレーのテーマ
  theme(text = element_text(family = "HiraKakuPro-W3")) # フォントファミリーの設定

