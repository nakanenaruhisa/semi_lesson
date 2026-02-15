#Rを使う下準備
#packageの準備
library(stringr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(plotrix)
library(modelsummary)
library(car)
library(gt)

getwd()
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
if (grepl("^Lesson[0-9]", basename(getwd())) || basename(getwd()) %in% c("folder_format", "applied")) setwd("..")


#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#データセットの読み込み
multureg2_dataset <- read_excel("Lesson15_logistic_regression2/multureg2_dataset.xlsx")


#カテゴリ変数の作成
dataset <- multureg2_dataset %>%
  mutate(event_c = factor(event,levels = 0:1,labels = c("入院","退院")))%>%
  mutate(gender_c = factor(gender,levels = 1:2,labels = c("男性","女性")))%>%
  mutate(oral_care_c = factor(oral_care,levels = 0:1,labels = c("未実施","実施"))) %>% 
  mutate(cohabiting_family_c = factor(cohabiting_family,levels = 0:1,labels = c("同居無","同居有")))%>%
  mutate(housing_ownership_c = factor(housing_ownership,levels = 0:1,labels = c("賃貸","所有")))%>%
  mutate(municipality_scale_c = factor(municipality_scale,levels = 1:3,labels = c("一般市","中核市","政令市")))%>%
  mutate(care_level_c = factor(care_level,levels = 1:5,labels = c("要介護度1","要介護度2","要介護度3","要介護度4","要介護度5")))

view(dataset)

#datasetの要約表その1
tbl_summary(dataset)

df <- dataset %>% 
  group_by(口腔ケア　= oral_care_c) %>% 
  summarise(n = n(),
            退院日数平均 = mean(discharge_days),
            退院日数標準偏差 = sd(discharge_days))

tbl_1 <- gt(df)
print(tbl_1)

logireg <- glm(data = dataset, event ~ oral_care_c + gender_c + care_level_c + cohabiting_family,family=binomial(link="logit"),control = glm.control(maxit = 100))

# 多重共線性のチェック
vif(logireg)
summary(logireg)

#退院の有無と口腔ケアのプロット書いてみて
ggplot(data = dataset) +
  aes(x = oral_care, y = event)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  geom_jitter()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#要介護度と退院の有無のプロット書いてみて
ggplot(data = dataset) +
  aes(x = care_level, y = event)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_jitter(height=0.1, width =0.1) +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#オッズ比の計算
# exp(logireg$coefficients) でもオッズ比は計算できますが、confintと組み合わせて
# 信頼区間も同時に出すのが推奨されます。

# 例：1つのモデルの場合
logireg_single <- glm(data = dataset, event ~ oral_care_c + gender_c + care_level_c + cohabiting_family, family=binomial(link="logit"))
# オッズ比
exp(coef(logireg))


# 複数モデルの場合、オッズ比や信頼区間付きで一括で見るなら
logireg_list <- list()
logireg_list[['Model 1']] <- glm(data = dataset, event ~ oral_care_c + gender_c +care_level, family=binomial(link="logit"))
logireg_list[['Model 2']] <- glm(data = dataset, event ~ oral_care_c + gender_c +care_level + cohabiting_family, family=binomial(link="logit"))
logireg_list[['Model 3']] <- glm(data = dataset, event ~ oral_care_c + gender_c +care_level+cohabiting_family+housing_ownership_c, family=binomial(link="logit"))

# modelsummaryでオッズ比（exp=TRUE）と信頼区間を表示
modelsummary(
  logireg_list,
  exponentiate = TRUE,                  # オッズ比で表示
  statistic = "conf.int",               # 信頼区間を表示
  stars = TRUE,
  gof_map = c("n", "r2", "aic", "bic"),
  title = "退院の有無に関するロジスティック回帰分析（オッズ比, 信頼区間付き）"
)

library(coefplot)

#ggcoef
logireg_moderl_3 <- glm(data = dataset, event ~ oral_care_c + gender_c +care_level+cohabiting_family+housing_ownership_c, family=binomial(link="logit"))

ggcoef(logireg_moderl_3,
       mapping = aes_string(y = "term", x = "estimate"),
       conf.int = TRUE,
       conf.level = 0.95,
       exponentiate = FALSE,
       exclude_intercept = TRUE,
       vline = TRUE,
       vline_color = "red",
       vline_linetype =  "solid",
       errorbar_color = "black",
       errorbar_height = .25
       )

