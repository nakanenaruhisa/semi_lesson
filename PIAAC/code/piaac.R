#Rを使う準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggthemes)
library(gtsummary)
library(readr)
library(modelsummary)

#全部の変数を消す
rm(list=ls())

piaac <- read_csv("PIAAC/data/prgjpnp1.csv") %>% 
  mutate_if(is.character, as.numeric) %>% #すべての文字列変数を小文字に変換
  rename_all(.funs = tolower) #すべての大文字を小文字に変換

piaac <- piaac %>% 
  mutate(id = 1:n()) %>% 
  mutate(wage = earnhrbonus) %>% 
  mutate(logwage = log(wage)) %>% 
  mutate(gender = factor(gender_r, level = 2:1, label = c("女性","男性"))) %>% 
  mutate(age = age_r) %>% 
  mutate(work = 2 - c_q01a) %>% 
  mutate(workhour = d_q10 / 5) %>% 
  mutate(workhour_fct = case_when(
    d_q10 < 35 ~ 1,
    d_q10 >= 35 & d_q10 <= 49 ~ 2,
    d_q10 >= 50 & d_q10 <= 59 ~ 3,
    d_q10 >= 60 ~ 4
  )) %>% 
  mutate(workhour_fct = factor(workhour_fct,
                               levels = 1:4,
                               labels = c("短時間（35時間未満）", "標準（35-49時間）", "長時間（50-59時間）", "過労死ライン（60時間以上）"))) %>% 
  mutate(educ = case_when(
    edcat8 == 1 | edcat8 == 2 ~ 1,
    edcat8 == 3 ~ 2,
    edcat8 == 4 | edcat8 == 5 ~ 3,
    edcat8 == 6 | edcat8 == 7 | edcat8 == 8 ~ 4)) %>% 
  mutate(educ = factor(educ, levels = 1:4,
                       labels = c("中学", "高校", "短大高専", "大学大学院"))) %>% 
  mutate(mothereduc = factor(j_q06b,
                             levels = 1:3,
                             labels = c("初等教育", "中等教育", "高等教育"))) %>% 
  mutate(fathereduc = factor(j_q07b,
                             levels = 1:3,
                             labels = c("初等教育", "中等教育", "高等教育"))) %>% 
  mutate(parenteduc = case_when(
    j_q07b >= j_q06b ~ j_q07b,
    j_q07b < j_q06b ~ j_q06b,
    is.na(j_q07b) == TRUE & is.na(j_q06b) == FALSE ~ j_q06b,
    is.na(j_q06b) == TRUE & is.na(j_q07b) == FALSE ~ j_q07b
  )) %>% 
  mutate(parenteduc = factor(parenteduc,
                             levels = 1:3,
                             labels = c("初等教育", "中等教育", "高等教育"))) %>% 
  mutate(occupation = factor(isco1c,
                             levels = 1:9,
                             labels = c("管理職","専門職","技術職・准専門職","事務補助","サービス・販売","農林漁業","技能工","設備・機械運転・組立","単純作業")
  )) %>% 
  mutate(numeracy = pvnum1) %>% 
  mutate(ojt = 2 - b_q12c) %>% 
  mutate(health = 6 - i_q08) %>% 
  mutate(learning = i_q04d) %>% 
  mutate(readwork = readwork) %>% 
  mutate(numwork = numwork)


piaac %>% 
  ggplot(aes(x = numeracy, y = wage)) + 
  geom_point(shape = 1)+
  ylim(0, 10000)

piaac %>% 
  ggplot(aes(x = numeracy, y = wage)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0, 10000)

lm(data = piaac, formula = wage ~ numeracy)

piaac %>% 
  ggplot(aes(x = gender, y = wage)) + 
  geom_point(shape = 1, position = position_jitter(w = 0.3, h = 0)) + 
  ylim(0, 10000)

piaac %>% 
  filter(is.na(educ) == FALSE) %>% 
  ggplot(aes(x = educ, y = wage)) + 
  geom_point(shape = 1, position = position_jitter(w = 0.3, h = 0))+ 
  ylim(0, 10000)

piaac %>% 
  with(table(gender, ojt)) 

piaac %>% 
  group_by(age, ojt) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = age, y = ojt)) + 
  geom_point(aes(size = n), shape = 1) + 
  scale_size(range = c(1, 10)) + 
  geom_smooth(aes(weight = n), method = "lm", se = FALSE) +
  theme(legend.position = "none")

piaac <- piaac %>% 
  mutate(logwage = log(wage))

p1 <- piaac %>% 
  ggplot(aes(x = wage)) + 
  geom_histogram()+ 
  xlim(0, 10000)

p2 <- piaac %>% 
  ggplot(aes(x = logwage)) + 
  geom_histogram() +
  xlim(5, 10)

piaac %>% 
  ggplot(aes(x = numeracy, y = logwage)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", se = FALSE)

reg_res <- lm(data = piaac, logwage ~ numeracy)
summary(reg_res)

piaac <- piaac %>% 
  mutate(age_sq = age^2)

reg_res <- lm(data = piaac, logwage ~ age + age_sq)
summary(reg_res)

piaac %>% 
  ggplot(aes(x = age, y = logwage)) + 
  geom_point(shape = 1) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  ylim(5, 10)

reg_res <- lm(data = piaac, log(wage) ~ age)
modelsummary(list(reg_res),
             stars = TRUE, # 有意水準を示す印をつける
             coef_map = c("(Intercept)" = "切片",
                          "age" = "年齢"), # 各変数に名前をつける
             gof_map = c("nobs", "r.squared"))  # サンプルサイズと決定係数のみ記載する
