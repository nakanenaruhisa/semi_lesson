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

#データセットを読み込む
data("NMES1988")

data <- NMES1988

view(data)

#データをざっと見る
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
#gmlresult[['model_1']] <- glm(visits ~ hospital+health+chronic+adl+region+age+afam+gender+married+school+income+insurance, 
                             #data=data, 
                             #family = poisson(link = "log"))
#モデル2（ゼロ過剰ポアソン分布）
gmlresult[['model_1']] <- zeroinfl(visits~hospital+health+chronic+adl+region+age+afam+gender+married+school+income+insurance, 
                                                   data=data,
                                                   dist="poisson")
#回帰表テーブル
modelsummary (gmlresult, statistic = 'conf.int', conf_level = .99, gof_omit = "AIC|BIC",stars = TRUE) # 99% 信頼区間)

#回帰表テキスト
texreg::screenreg(gmlresult)

#coefpoot
library(dotwhisker)
library(jtools)

dwplot(gmlresult) +
  geom_vline(xintercept = 0,
             colour = "grey",
             linetype = 3) +
  theme_bw(base_size = 15) + 
  
  xlab("Coefficient Estimate") + ylab("") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.007, 0.01),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank())
