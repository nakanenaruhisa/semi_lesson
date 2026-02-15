#すべての変数を消す
rm(list = ls())

# ワーキングディレクトリの確認
getwd()
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
if (grepl("^Lesson[0-9]", basename(getwd())) || basename(getwd()) %in% c("folder_format", "applied")) setwd("..")

#packageの準備
library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#データの読み込み
Ramen <- read_csv("Lesson11_data_handling/Ramen.csv")

#データの確認
view(Ramen)

# dplyr:select()を使って「行」を抜き出す方法
# select(df, ID, Name, Pref, Score)でもOK

# 不完全なコード例（コメントアウト）
# Ramen %>%
#   select(I

# 変数の選択（ID, Pref, Score, Nameを選択）
Ramen_select <- Ramen %>%
  select(ID, Pref, Score, Name)

#選択した変数を含むデータフレームの確認
view(Ramen_select)

# dplyr:select()を使って「行」を抜き出す方法
# select(df, ID, Name, Pref, Score)でもOK
# 項目を「*** = 変数」で一覧に表示される変数も変更できる（原則半角英数字）

#選択したデータの要約統計量を表示
summary(Ramen_select)

#京都府と和歌山県のお店だけ抜き出してみよう
Ramen_kyoto_wakayama <- Ramen_select %>%
  filter(Pref == "京都府" | Pref == "和歌山県")

#フィルタリングした結果の確認
view(Ramen_kyoto_wakayama)

#ScoreがNA出ないものだけを抜き出してみよう
Ramen_kyoto_wakayama_na <- Ramen_kyoto_wakayama %>%
  filter(!is.na(Score))

#フィルタリングした結果の確認
view(Ramen_kyoto_wakayama_na)

# ============================================
# 課題1: group_by()とsummarise()による集計
# ============================================
# 都道府県（Pref）ごとの平均Scoreを計算してみよう
# ヒント: group_by(Pref) %>% summarise(mean_score = mean(Score))


# ============================================
# 課題2: 複数の条件を使ったfilter()
# ============================================
# Scoreが4.0以上かつ都道府県が「京都府」の店舗だけを抽出してみよう
# ヒント: filter(Pref == "京都府" & Score >= 4.0)
