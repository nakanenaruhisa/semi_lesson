#Rを使う下準備
#packageの準備
install.packages("stringr")
library(stringr)
library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(plotrix)
library(modelsummary)
library(car)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraKakuProN-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#datasetを読み込む
multureg2_dataset <- read_csv("multireg_2/multureg2_dataset.csv")

as_tibble(multureg2_dataset)

#カテゴリ変数の作成

dataset <- multureg2_dataset %>% 
  mutate(oral_care_c = factor(oral_care,levels = 0:1,labels = c("未実施","実施"))) %>% 
  mutate(cohabiting_family_c = factor(cohabiting_family,levels = 0:1,labels = c("同居無","同居有")))%>%
  mutate(housing_ownership_c = factor(housing_ownership,levels = 0:1,labels = c("賃貸","所有")))%>%
  mutate(municipality_scale_c = factor(municipality_scale,levels = 1:3,labels = c("一般市","中核市","政令市")))

#統合したデータセットを表にする
dataset <- dataset %>% 
  select(id,discharge_days,oral_care_c,care_level,cohabiting_family_c,housing_ownership_c,household_income,municipality_scale_c)

tbl_summary(dataset)

#退院日数を従属変数として単回帰（要介護度）
reg_care_lebel <- lm(data = dataset, formula = discharge_days ~ care_level)

reg_care_lebel

#プロットを回帰直線を入れて書いてみて
ggplot(data = dataset) +     
  aes(x = care_level, y = discharge_days, label = care_level)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  #geom_text(aes(y = discharge_days, label = care_level_c), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_grey(base_size = 15)+#grayテーマで
  theme_gray(base_family="HiraginoSans-W3") #文字化けしないおまじない


#退院日数を従属変数として単回帰（口腔ケア）
reg_oral_care_c <- lm(data = dataset, formula = discharge_days ~ oral_care_c)

#プロットを回帰直線を入れて書いてみて
ggplot(data = dataset) +     
  aes(x = oral_care_c, y = discharge_days, label = oral_care_c)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  #geom_text(aes(y = discharge_days, label = oral_care_c), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

##退院日数を従属変数として単回帰（世帯収入）
reg_household_income <- lm(data = dataset, formula = discharge_days ~ household_income)

#プロットを回帰直線を入れて書いてみて
ggplot(data = dataset) +     
  aes(x = discharge_days, y = household_income)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  #geom_text(aes(y = 県民所得, label = 都道府県), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#重回帰分析
gmlresult <- lm(data = dataset, discharge_days ~ oral_care_c + care_level + household_income)

# 多重共線性のチェック
vif(gmlresult)
summary(gmlresult)

gmlresult <- list()
#モデル1（口腔ケアのみ）
gmlresult[['model_1']] <- lm(discharge_days ~ oral_care_c, data =dataset)
#モデル2（口腔ケア＋世帯収入）
gmlresult[['model_2']] <- lm(discharge_days ~ oral_care_c+ household_income, data =dataset)
#モデル3（口腔ケア＋世帯収入+要介護度）
gmlresult[["model_3"]] <- lm(discharge_days ~ oral_care_c+ household_income  + care_level , data =dataset)

#回帰表テーブル
modelsummary (gmlresult, statistic = 'conf.int', conf_level = .99, gof_omit = "AIC|BIC",stars = TRUE) # 99% 信頼区間)

#回帰表テキスト
texreg::screenreg(gmlresult)

#coefpoot
install.packages("coefplot")
library(coefplot)
library(broom)
coefplot(gmlresult[["model_3"]], intercept = FALSE)

#ggplot
tidy(gmlresult[["model_3"]],conf.int = TRUE,
     exclude_intercept = TRUE) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "")

#ggcoef
ggcoef(gmlresult[["model_3"]],
       mapping = aes_string(y = "term", x = "estimate"),
       conf.int = TRUE,
       exclude_intercept = TRUE,
       conf.level = 0.95,
       exponentiate = FALSE,
       vline = TRUE,
       vline_color = "red",
       vline_linetype =  "solid",
       errorbar_color = "black",
       errorbar_height = .15)

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

