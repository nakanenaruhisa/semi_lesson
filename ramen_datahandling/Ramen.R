#packageの準備
library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

Ramen <- read_csv("OneDrive−個人用/R/semi_lesson/ramen/Ramen.csv")

# dplyr:select()を使って「行」を抜き出す方法
# select(df, ID, Name, Pref, Score)でもOK
Ramen %>%
  select(ID, Pref, Score, Name)

# dplyr:select()を使って「行」を抜き出す方法
# select(df, ID, Name, Pref, Score)でもOK
# 項目を「*** = 変数」で一覧に表示される変数も変更できる（原則半角英数字）
Ramen %>%
  select(ID, 都道府県 = Pref, Score, 店名 = Name)