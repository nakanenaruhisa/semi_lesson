#Rを使う準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(fixest)

#フォルダの固定
here::here()

#全部の変数を消す
rm(list=ls())
