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

## ライブラリ読み込み
install.packages("Matching")
library(Matching) 

## smokerbabyデータ読み込み
dataSet <- smokerbaby_weight <- read_csv("Propensity_score_matting/smokerbaby_weight.csv")
view(dataSet)

# 'mbsmoke'と'fbaby'が 'Yes' の場合は 1 に、'No' の場合は 0 に置き換える
dataSet$mbsmoke <- ifelse(dataSet$mbsmoke == "smoker", 1, 0)
dataSet$fbaby <- ifelse(dataSet$fbaby == "Yes", 1, 0)

# 全変数で体重を従属変数で回帰分析
propensityScoreModel <- lm(bweight ~ mmarried + 	mhisp+	fhisp	+foreign+	alcohol+	deadkids+	mage+	medu+	fage+	fedu+nprenatal+	monthslb+	order	+ mbsmoke+	mrace	+frace	+prenatal+	birthmonth+	lbweight+	fbaby	+prenatal1, dataSet)

summary(propensityScoreModel)

## 各患者の傾向スコアを取得
propensityScores = propensityScoreModel$fitted.values

propensityScores

## Matchライブラリを使って傾向スコアマッチングを行う
propensityScoreMatching = Match(Y = as.integer(dataSet$bweight), Tr = (dataSet$mbsmoke==1), X = propensityScores, M=1, ties=FALSE, replace = TRUE)
summary(propensityScoreMatching)



## IPWを実践
### 変数長いのでまとめる
Y  <- as.integer(dataSet$death)-1
z1 <- dataSet$swang1
ipwe1 <- sum((z1*Y/propensityScores)/sum(z1/propensityScores))
ipwe0 <- sum(((1-z1)*Y)/(1-propensityScores))/sum((1-z1)/(1-propensityScores))

ipwe1 - ipwe0
