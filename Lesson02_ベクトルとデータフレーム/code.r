#　Rの準備
#　全部の変数を消す
rm(list = ls())

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

#データフレームの要素をテーブルとして表示
view(df)

#関数""を見つけることができませんでした、と出た時は…
library(tidyverse)

#データフレームの要素をテーブルとして表示
view(df)

#IDを文字列にする
df$ID <- as.character(df$ID)

#IDを数値にする
df$ID <- as.numeric(df$ID)

#　xの平均を出す
mean(df$x)

#　yの平均を出す
mean(df$y)

#xを文字列にしてから

#xの平均を出す

#現在のディレクトリを表示する
getwd()


#データセットをcsvに保存する
write.csv(df, "Lesson2_ベクトルとデータフレーム/data.csv")


