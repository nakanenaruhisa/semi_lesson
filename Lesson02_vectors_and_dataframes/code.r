#　Rの準備
#　全部の変数を消す
rm(list = ls())

#ワーキングディレクトリを確認（semi_lessonじゃなかったら移動する）
getwd()
setwd(
  "/Users/naruhisa/Library/CloudStorage/OneDrive-学校法人立命館/lecture/semi/R/semi_lesson"
)

#　ベクトルを作成
ID <- c(1, 2, 3, 4, 5)
name <- c("山田", "鈴木", "佐藤", "磯辺", "小林")
x <- c(100, 200, 300, 400, 500)
y <- c(600, 700, 800, 900, 1000)

#　ベクトルの要素を表示
print(ID)
print(name)
print(x)
print(y)

#　ベクトルの要素をデータフレームに格納する
df <- data.frame(ID = ID, name = name, x = x, y = y)

#　データフレームの要素を表示
print(df)

# データフレームの要素をテーブルとして表示
View(df)

# 関数"view"を見つけることができませんでした、と出た時は…
library(tidyverse)

#IDを文字列にする（左によってたら文字だよ）
df$ID <- as.character(df$ID)

#IDを数値にする（右によってたら数値だよ）
df$ID <- as.numeric(df$ID)

#　xの平均を出す
mean(df$x)

#　yの平均を出す
mean(df$y)

#xを文字列にしてから

#xの平均を出すとどうなる？

# データセットをcsvに保存する
write.csv(df, "Lesson02_vectors_and_dataframes/data.csv")
