#Rを使う下準備

#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(rpart)
library(car)
library(modelsummary)
library(readr)
library(readxl)

#全部の変数を消す
rm(list=ls())



### 回帰木の使い方

multureg2_dataset <- read_excel("decision_tree/multureg2_dataset.xlsx")

#dataをインポート
data <- multureg2_dataset



#変数にラベルをつけるよ
data <- data %>% 
  mutate(oral_care_c = factor(oral_care,levels = 0:1,labels = c("未実施","実施"))) %>%
  mutate(gender_c = factor(gender,levels = 1:2,labels = c("男性","女性"))) %>% 
  mutate(cohabiting_family_c = factor(cohabiting_family,levels = 0:1,labels = c("同居無","同居有")))%>%
  mutate(housing_ownership_c = factor(housing_ownership,levels = 0:1,labels = c("賃貸","所有")))%>%
  mutate(municipality_scale_c = factor(municipality_scale,levels = 1:3,labels = c("一般市","中核市","政令市")))%>%
  mutate(care_level_c = factor(care_level,levels = 1:5,labels = c("要介護度1","要介護度2","要介護度3","要介護度4","要介護度5")))


tbl_summary(data)


#重回帰分析
gmlresult <- lm(discharge_days ~ gender_c + care_level_c + oral_care_c+cohabiting_family_c + housing_ownership_c, data = data)

# 多重共線性のチェック
vif(gmlresult)
summary(gmlresult)#県民所得は有意でない可能性があるので削除

gmlresult <- list()
gmlresult[['model_1']] <- lm(discharge_days ~ care_level_c, data =data)
gmlresult[['model_2']] <- lm(discharge_days ~ care_level_c +oral_care_c , data =data)
gmlresult[["model_3"]] <- lm(discharge_days ~ care_level_c +oral_care_c+ cohabiting_family_c, data =data)
gmlresult[["model_4"]] <- lm(discharge_days ~ care_level_c +oral_care_c+ cohabiting_family_c + gender_c, data =data)

modelsummary (gmlresult)

#学習データを指定
data <- data %>% 
  select(discharge_days,gender_c,care_level_c,oral_care_c,cohabiting_family_c,housing_ownership_c)

model = rpart(discharge_days ~ .,data = data)

model

#可視化packageの準備
install.packages("rpart.plot")
library(rpart.plot)

#可視化するよ
rpart.plot(model)

#ggpaertyで可視化するよ
library(partykit)
library(ggparty)

pct <- as.party(model)

g <- ggparty(pct, terminal_space = 0.5)
g <- g + geom_edge(size = 1.5)
g <- g + geom_edge_label(colour = "grey", size = 3)
g <- g + geom_node_plot(
  gglist = list(geom_boxplot(aes(x = "", y = discharge_days, fill = gender_c)), theme_bw(base_size = 12)),
  scales = "fixed",
  id = "terminal",
  shared_axis_labels = TRUE,
  shared_legend = TRUE,
  legend_separator = TRUE,)
g <- g + geom_node_label(
  aes(col = splitvar),
  line_list = list(aes(label = paste("Node", id)),
                   aes(label = splitvar)),
  line_gpar = list(list(
    size = 10,
    col = "black",
    fontface = "bold"
  ),
  list(size = 12)),
  ids = "inner"
)
g <- g + geom_node_label(
  aes(label = paste0("Node ", id, ", N = ", nodesize)),
  fontface = "bold",
  ids = "terminal",
  size = 3,
  nudge_y = 0.01
)
g <- g + theme(legend.position = "none")
plot(g)

