#Rを使う下準備
#packageの準備

install.packages("magrittr")

library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

p2 <- 
  iris %>% 
  ggpairs(mapping = aes(color = Species), #Speciesでグループ分け
          diag=list(continuous="barDiag"),
          # diagの連続×連続を積み上げヒストグラムに変更
          lower=list(continuous="smooth", combo = "facetdensity"))
# lowerの連続×連続を回帰線に，連続×離散を密度図に変更)
p2

#️ランダムフォレスト

install.packages("randomForest") # 必要に応じてインストール
library(randomForest)

#ランダムフォレストの実行

iris.rf <- randomForest(Species ~ ., data = iris) 

iris.rf

#重要度

importance(iris.rf)

#重要度のプロット
varImpPlot(iris.rf)

# 訓練データとテストデータに分割
set.seed(100)
df.rows = nrow(iris) # 150
train.rate = 0.7 # 訓練データの比率
train.index <- sample(df.rows, df.rows * train.rate)
df.train = iris[train.index,] # 訓練データ
df.test = iris[-train.index,] # テストデータ
cat("train=", nrow(df.train), " test=", nrow(df.test))

model.rf <- randomForest(Species ~ ., data = df.train) # ランダムフォレスト、データは訓練用を指定

# テストデータの予測
prediction = predict(model.rf, df.test)

(result <- table(prediction, df.test$Species)) # ()で括って内容表示

# 精度の計算（行列の対角合計 / 行列の合計）
(accuracy_prediction = sum(diag(result)) / sum(result))

# モデル予測結果を図示（Petal.Length / Petal.Width）
par(mar = c(6, 7, 5, 2))
par(mgp = c(4, 1.2, 0.4))
par(lwd = 2)

plot(df.test$Petal.Length, df.test$Petal.Width,
     main = "Iris (random forest)",
     xlab = "Petal length (cm)",
     ylab = "Petal width (cm)",
     #col = prediction,
     pch=c(1,2,3)[prediction],
     cex = 2,
     cex.main = 2,
     cex.lab = 2,
     cex.axis = 1.5,
     xlim = c(0, 7),
     ylim = c(0, 3.5),
     yaxp = c(0, 3, 3)
)

par(family = "serif")
par(font = 3)

legend("topleft",
       legend = levels(prediction),
       pch = c(1, 2, 3),
       # col = c(1, 2, 3),
       cex = 1.7,
       pt.cex = 2,
       bty = 'n',
       inset = c(0.05, 0.03))

