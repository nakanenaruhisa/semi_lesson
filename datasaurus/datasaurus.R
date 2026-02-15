#Rを使う下準備
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

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

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

#tble_summaryを使って統計量を出す
datasaurus_dozen %>% 
  select(x, y, dataset) %>% 
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ± {sd}", 
                     all_categorical() ~ "{n} ({p}%)"
    )
  )


ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
    geom_point()+
    theme_void()+
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol = 3)

## Loading required namespace: ggplot2

#プロット書いてみて
datasaurus_dozen %>% 
  (dataset = "dino") 

