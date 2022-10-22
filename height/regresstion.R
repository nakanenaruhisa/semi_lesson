#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(modelsummary)

#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#全部の変数を消す
rm(list=ls())

#datasetを読み込む
ten_kukan100 <- read_csv("height_weight/ten_kukan100.csv")

#変数を箱に入れるよ
height <- (ten_kukan100$height)
weight <- (ten_kukan100$weight)
sex <- (ten_kukan100$sex)

#性別をカテゴリ変数に変換
ten_kukan100 <- ten_kukan100 %>% 
  mutate(sex_c = factor(sex,
                        levels = 1:2,
                        labels = c("女性","男性")))

#性別がカテゴリ変数に変換できていることを確認
ten_kukan100 %>% with(table(sex_c))
sex_c <- (ten_kukan100$sex_c)

#身長と体重の関係の回帰式を書くよ
lm(data = ten_kukan100, formula = height ~ weight)

#男性の身長と体重の関係の回帰式を書くよ
reg_male <- ten_kukan100 %>% 
  filter(sex_c == "男性") %>% 
  lm(formula = height ~ weight) 

#女性の身長と体重の関係の回帰式を書くよ
reg_female <- ten_kukan100 %>% 
  filter(sex_c == "女性") %>% 
  lm(formula = height ~ weight) 


#男性の回帰式を表にする
reg_male %>% 
  tbl_regression(intercept = TRUE) %>% 
  modify_header(label ~ "男性の身長の回帰式") # ""の部分には好きな文字列を入れられる

#女性の回帰式を表にする
reg_female %>% 
  tbl_regression(intercept = TRUE) %>% 
  modify_header(label ~ "女性の身長の回帰式") # ""の部分には好きな文字列を入れられる

#身長と体重のプロットを回帰直線を入れて書いてみて
ggplot(data = ten_kukan100) +     # ten_kukan100データでキャンバス準備
  aes(x = height, y = weight)+ # height,weight列をx,y軸にmapping
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#身長と体重のプロットを性別ごとに回帰直線を入れて書いてみて
ggplot(data = ten_kukan100) +     # tenkukan20データでキャンバス準備
  aes(x = height, y = weight, colour = sex_c)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#coefpoot
install.packages("coefplot")
library(coefplot)
coefplot(reg_male, intercept = FALSE)

#ggplot
tidy(reg_male, conf.int = TRUE) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  scale_colour_tableau()+
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

#ggcoef
ggcoef(reg_male,
       mapping = aes_string(y = "term", x = "estimate"),
       conf.int = TRUE,
       conf.level = 0.95,
       exponentiate = FALSE,
       exclude_intercept = TRUE,
       vline = TRUE)
