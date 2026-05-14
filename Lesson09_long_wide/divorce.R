# パッケージ読み込み（Excel読み込みに使う）
library(readxl)
# パッケージ読み込み（データ操作に使う）
library(dplyr)
# パッケージ読み込み（縦持ち・横持ち変換に使う）
library(tidyr)
# パッケージ読み込み（文字列処理に使う）
library(stringr)
# パッケージ読み込み（複数の便利パッケージをまとめて使える）
library(tidyverse)

# すべての変数を消す（前回実行のデータが残っていると混乱するため）
rm(list = ls())

# Excelファイルを読み込み、dfという名前で保存する
df  <- read_excel("~/Library/CloudStorage/OneDrive-学校法人立命館/lecture/semi/R/semi_lesson/Lesson9_ロングワイド変換/年次別離婚率.xlsx")

# 列名から「年」という文字を取り除く（例: 2001年 -> 2001）
names(df) <- str_replace(names(df), "年", "")
# 1列目以外の列名を数値型に変換する（年を数値として扱いやすくする）
names(df)[-1] <- as.numeric(names(df)[-1])  # ID列以外の列名を数値に変換

# 2列目を削除する（不要な列を落とす）
df <- df %>%
  select(-2)

# dfをロング形式（縦長）データに変換する
df_long <- df %>%
  pivot_longer(
    cols = -ID,  # ID列以外のすべての列（各年の列）を縦にまとめる
    names_to = "year",    # 列名（年）を入れる新しい列名をyearにする
    values_to = "divorce_num", # 各年の値を入れる新しい列名をdivorce_numにする
    names_transform = list(year = as.numeric) # names_to の列 year を数値型にする
  )

# 既存のID列を一度削除し、yearで昇順に並べ替える
df_long <- df_long %>%
  select(-ID) %>%
  arrange(year)

# 新しいIDを1から連番で作成し、ID順に並べる
df_long <- df_long %>%
  mutate(ID = row_number()) %>%
  arrange(ID)

# 列の順番をID, year, divorce_numにそろえる
df_long <- df_long %>%
  select(ID, year, divorce_num)

# 変換後のデータを表示する
print(df_long)

# 使ったDatasetを保存する
write.csv(df_long, "Lesson09_long_wide/divorce_long.csv")
