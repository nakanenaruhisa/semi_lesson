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

## RHCデータ読み込み
dataSet = rhc <- read_csv("Propensity_score_matting/rhc.csv")
view(dataSet)

## 施術と死亡との相関を確認
table(dataSet[,c("swang1", "death")])

# 'death' と 'swang1' が 'Yes' の場合は 1 に、'No' の場合は 0 に置き換える
dataSet$death <- ifelse(dataSet$death == "Yes", 1, 0)
dataSet$swang1 <- ifelse(dataSet$swang1 == "RHC", 1, 0)

## 全変数でロジスティック回帰
propensityScoreForAllVal <- glm(swang1 ~ age + sex + race + edu + income + ninsclas + cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 + wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 + paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 + bili1 + alb1 + resp + card + neuro + gastr + renal + meta + hema + seps + trauma + ortho + cardiohx + chfhx + dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx + malighx + immunhx + transhx + amihx,
                                family  = binomial(link = "logit"),
                                data    = dataSet)
## AIC基準で最適な変数を選ぶ
step(propensityScoreForAllVal)

## 選ばれた最適な変数を組み込んで再度ロジスティック回帰
propensityScoreModel = glm(formula = swang1 ~ age + edu + ninsclas + cat1 + dnr1 + ca + 
                             surv2md1 + aps1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + 
                             pafi1 + paco21 + ph1 + hema1 + sod1 + pot1 + crea1 + alb1 + 
                             resp + card + neuro + gastr + renal + hema + seps + trauma + 
                             dementhx + psychhx + renalhx + transhx, family = binomial(link = "logit"), 
                           data = dataSet)

summary(propensityScoreModel)


## 各患者の傾向スコアを取得
propensityScores = propensityScoreModel$fitted.values

propensityScores

## Matchライブラリを使って傾向スコアマッチングを行う
propensityScoreMatching0.1 = Match(Y = as.integer(dataSet$death)-1 , Tr = (dataSet$swang1==1), X = propensityScores, M=1,caliper = 0.1, ties=FALSE, replace = FALSE)
summary(propensityScoreMatching0.1)



## IPWを実践
### 変数長いのでまとめる
Y  <- as.integer(dataSet$death)-1
z1 <- dataSet$swang1
ipwe1 <- sum((z1*Y/propensityScores)/sum(z1/propensityScores))
ipwe0 <- sum(((1-z1)*Y)/(1-propensityScores))/sum((1-z1)/(1-propensityScores))

ipwe1 - ipwe0
