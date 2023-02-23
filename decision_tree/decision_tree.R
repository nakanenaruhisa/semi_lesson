#Rを使う下準備

#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(rpart)

#全部の変数を消す
rm(list=ls())

#dataをインポート
data <- logsticreg_sample

#変数にラベルをつけるよ
data <- data %>% 
  mutate(regidencial_status_c = factor(regidencial_status,levels = 0:1,labels = c("在宅","入所")))%>%
  mutate(gender_c = factor(gender,levels = 1:2,labels = c("女性","男性")))%>%
  mutate(nintei_grade_c = factor(nintei_grade,levels = 1:7,labels = c("要支援1","要支援2","要介護1","要介護2","要介護3","要介護4","要介護5")))%>%
  mutate(chronic_disease_c = factor(chronic_disease,levels = 0:1,labels = c("慢性疾患なし","慢性疾患あり")))%>%
  mutate(acute_disease_c = factor(acute_disease,levels = 0:1,labels = c("急性疾患なし","急性疾患あり")))

#学習データを指定
data <- data %>% 
  select(regidencial_status_c,age,gender_c,chronic_disease_c,acute_disease_c)

model = rpart(regidencial_status_c ~ .,data = data)

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
g <- g + geom_edge_label(colour = "grey", size = 6)
g <- g + geom_node_plot(
  gglist = list(geom_bar(aes(x = "", fill = regidencial_status_c), position = "fill"), theme_bw(base_size = 15)),
  scales = "fixed",
  id = "terminal",
  shared_axis_labels = TRUE,
  shared_legend = TRUE,
  legend_separator = TRUE,
)
g <- g + geom_node_label(
  aes(col = splitvar),
  line_list = list(aes(label = paste("Node", id)),
                   aes(label = splitvar)),
  line_gpar = list(list(
    size = 12,
    col = "black",
    fontface = "bold"
  ),
  list(size = 20)),
  ids = "inner"
)
g <- g + geom_node_label(
  aes(label = paste0("Node ", id, ", N = ", nodesize)),
  fontface = "bold",
  ids = "terminal",
  size = 5,
  nudge_y = 0.01
)
g <- g + theme(legend.position = "none")
plot(g)
