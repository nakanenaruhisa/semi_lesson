#Rを使う準備
library(ggplot2)
library(ggsci)
library(tidyverse)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(plotrix)
library(reshape2)

as_tibble(EIU2020)
japan_taiwan <- EIU2020 %>%
  filter(country %in% c("Taiwan","Japan"))

#せっかくだから男女別に書いてみる
japan_taiwan %>% 
  select(country,eiu,year) %>% 
  tbl_summary(label = list(country ~ "国名",
                           eiu ~ "民主主義スコア",
                           year ~ "年"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = country,
              digits = all_continuous() ~ 1)#数値の部分が小数点第y位の部分の値

ggplot(data = japan_taiwan)+
  (aes(x = year, y = eiu, colour = as.factor(country)))+
  geom_line()+
  geom_point() +
  scale_colour_tableau()+
  labs(color = "year")+ #凡例のタイトルを指定
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
