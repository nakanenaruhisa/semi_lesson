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

#全部の変数を消す
rm(list=ls())


### 分類木の使い方

#dataをインポート

library(readxl)
logsticreg_sample <- read_excel("logisticreg/logsticreg_sample.xlsx")
data <- logsticreg_sample

#変数にラベルをつけるよ
data <- data %>% 
  mutate(regidencial_status_c = factor(regidencial_status,levels = 0:1,labels = c("在宅","入所")))%>%
  mutate(gender_c = factor(gender,levels = 1:2,labels = c("女性","男性")))%>%
  mutate(nintei_grade_c = factor(nintei_grade,levels = 1:7,labels = c("要支援1","要支援2","要介護1","要介護2","要介護3","要介護4","要介護5")))%>%
  mutate(family_status_c = factor(family_status,levels = 0:1,labels = c("同居なし","同居あり")))%>%
  mutate(public_assistance_c = factor(public_assistance,levels = 0:1,labels = c("介護保険利用なし","介護保険利用あり")))%>%
  mutate(public_pension_c = factor(public_pension,levels = 0:1,labels = c("公的年金なし","公的年金あり")))


#居住状態を従属変数としてロジスティック回帰分析
logireg <- list()
logireg[['Model 1']] <- glm(data = data, formula = regidencial_status ~ nintei_grade)
logireg[['Model 2']] <- glm(data = data, formula = regidencial_status ~ nintei_grade + family_status_c)
logireg[['Model 3']] <- glm(data = data, formula = regidencial_status ~ nintei_grade + family_status_c + public_assistance_c)
logireg[['Model 4']] <- glm(data = data, formula = regidencial_status ~ nintei_grade + family_status_c + public_assistance_c +public_pension_c + age)


modelsummary (logireg)

#学習データを指定
data <- data %>% 
  select(regidencial_status_c,age,gender_c,nintei_grade_c,family_status_c,public_assistance_c,public_pension_c)

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
  list(size = 12)),
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

