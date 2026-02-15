# Rを使う前に以下のコードを必ず実行すること

# 作業フォルダの確認
getwd()

# 作業フォルダを semi_lesson に変更（このファイルが folder_format 内にある場合は1つ上へ）
if (basename(getwd()) == "folder_format") setwd("..")

#全部の変数を消す
rm(list=ls())

