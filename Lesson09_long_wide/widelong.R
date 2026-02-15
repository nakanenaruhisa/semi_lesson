#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
# readxl はこのスクリプトでは未使用（CSV のみ読み込み）

#全部の変数を消す
rm(list=ls())

#wide型のdatasetを読み込む
userdataset <- read_csv("Lesson09_long_wide/userdataset.csv")
view(userdataset)


#数値の変数を文字列変数に変換
userdataset <- userdataset %>%
  mutate(across(where(is.numeric), as.character))

data_long <- userdataset %>%
  pivot_longer(cols = -c(ID, servicename),
               names_to = "date",
               values_to = "user") %>%
  separate(date, into = c("year", "month", "day"), sep = "/") %>%
  mutate(date = paste(year, month, day, sep = "-")) %>%
  select(ID, servicename, date, user)


view(data_long)
