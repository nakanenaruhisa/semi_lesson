# パッケージ読み込み
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

#すべての変数を消す
rm(list = ls())

df  <- read_excel("~/Library/CloudStorage/OneDrive-学校法人立命館/lecture/semi/R/semi_lesson/Lesson9_ロングワイド変換/年次別離婚率.xlsx")

# 列名から"年"を削除し、数値に変換
names(df) <- str_replace(names(df), "年", "")
names(df)[-1] <- as.numeric(names(df)[-1])  # ID列以外の列名を数値に変換

#　2列目を削除
df <- df %>%
  select(-2)

#dfをロングデータに変換
df_long <- df %>%
  pivot_longer(
    cols = -ID,  # 都道府県列以外の全ての列を変換
    names_to = "year",    # 新しい列名を"year"とする
    values_to = "divorce_num", # 値の列名を"divorce_num"とする
    names_transform = list(年 = as.numeric) # 年を数値型に変換
  )

# IDの列を削除して、年数を昇順に並べ替える
df_long <- df_long %>%
  select(-ID) %>%
  arrange(year)

#　1列目にIDを追加して、ID順に並び替える
df_long <- df_long %>%
  mutate(ID = row_number()) %>%
  arrange(ID)

#　変数の並びを、ID,year,divorce_numにする
df_long <- df_long %>%
  select(ID, year, divorce_num)

view(df_long)
