#Rを使う下準備
#packageの準備

install.packages("ggplot2")
install.packages("AER")
install.packages("MASS")
install.packages("pscl")
install.packages("modelsummary")

library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(modelsummary)
library(AER)
library(MASS)
library(pscl)

#全部の変数を消す

rm(list=ls())

# データ読み込み
data <- read_csv("multi_reg5/docvis.csv")
psych::describe(data,skew=FALSE)
hist(data$docvis)
table(data$docvis)

# 通院回数を従属変数として、重回帰分析
result1 <- lm(docvis~private+chronic+female+income+black+hispanic,data=data)
summary(result1)

#0が多いので、ゼロ膨張問題が発生している可能性がある。
c(obs=sum(data$docvis==0), poi=sum(dpois(0,exp(predict(result1)))))


# Poisson回帰
result2 <- glm(docvis~private+chronic+female+income+black+hispanic,data=data,family = poisson(link="log"))
summary(result2)

#0が多いので、ゼロ膨張問題が発生している可能性がある。
c(obs=sum(data$docvis==0), poi=sum(dpois(0,exp(predict(result2)))))


# ゼロ過剰Poisson回帰
result3 <- zeroinfl(docvis~private+chronic+female+income+black+hispanic|private+chronic+female+income+black+hispanic,dist="poisson",data=data)
summary(result3)

#0が多いので、ゼロ膨張問題が発生している可能性がある。
c(obs=sum(data$docvis==0), poi=sum(dpois(0,exp(predict(result3)))))

#予測値と実績値を比較
# 実績値

rbind(actual=table(data$docvis)[1:10], 
      pred_pois=round(colSums(predict(result2,type="prob")[,1:10])),
      pred_nb=round(colSums(predict(result3,type="prob")[,1:10])))

AIC(result1, result2, result3)
