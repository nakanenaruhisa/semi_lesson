#Rを使う下準備
#作業ディレクトリの確認と固定
getwd()
setwd("/Users/naruhisa/Library/CloudStorage/OneDrive-学校法人立命館/lecture/semi/R/semi_lesson")
#　↑ここは自分のディレクト入りにあわせて変える

#全部の変数を消す
rm(list=ls())
#packageの準備

install.packages("ggplot2")
install.packages("AER")
install.packages("MASS")
install.packages("pscl")
install.packages("modelsummary")
install.packages("broom")
install.packages("parameters")

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
library(broom)
library(parameters)

#データセットを読み込む
data("NMES1988")

data <- NMES1988

view(data)

#データの基本統計量を確認する
summary(data)

#病院への通院回数を従属変数として、その影響する変数を調べる

#従属変数の分布を確認する（正規分布か、ポアソン分布か？）
ggplot(data = data) +
  aes(x =visits ) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")

#ポアソン分布で重回帰分析する
poason_reg <- glm(visits~hospital+health+chronic+adl+region+age+afam+gender+married+school+income+insurance, 
                data=data, 
                family = poisson(link = "log"))
summary(poason_reg)

#0が多いので、ゼロ膨張問題が発生している可能性がある。ゼロの確率を確認する。
c(obs=sum(data$visits==0), poi=sum(dpois(0,exp(predict(poason_reg)))))

#ゼロ膨張問題が発生しているのでゼロ過剰ポアソン回帰を行う

zeroinfl_reg <- zeroinfl_reg <- zeroinfl(visits~hospital+health+chronic+adl+region+age+afam+gender+married+school+income+insurance, 
                     data=data,
                     dist="poisson")
summary(zeroinfl_reg)

#0が多いので、ゼロ膨張問題が発生している可能性がある。ゼロの確率を確認する。
c(obs=sum(data$visits==0), zip=sum(predict(zeroinfl_reg,type="prob")[,1]))

#ゼロ膨張問題を抑えることができた。

#coefplotで結果を可視化する

gmlresult <- list()
#モデル1（ポアソン分布）
gmlresult[['model_1']] <- glm(visits ~ hospital+health+chronic+adl+region+age+afam+gender+married+school+income+insurance, 
                             data=data, 
                             family = poisson(link = "log"))

#モデル2（ゼロ過剰ポアソン分布）
gmlresult[['model_2']] <- zeroinfl(visits~hospital+health+chronic+adl+region+age+afam+gender+married+school+income+insurance, 
                                   data=data,
                                   dist="poisson",
                                   model=FALSE)
#回帰表テーブル
modelsummary::modelsummary(gmlresult,
                             statistic = "std.error",
                             stars = TRUE,
                             gof_omit = "AIC|BIC|Log.Lik|Deviance|Num.obs")

#coefpoot
library(dotwhisker)
library(jtools)

dwplot(gmlresult, 
       show_intercept = TRUE,
       model_order = c("model_1", "model_2")) +
  geom_vline(xintercept = 0,
             colour = "grey",
             linetype = 3) +
  scale_color_manual(values = c("model_1" = "blue", "model_2" = "red")) +
  theme_bw(base_size = 15) + 
  xlab("Coefficient Estimate") + ylab("") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.007, 0.01),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12))
