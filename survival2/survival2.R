#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(haven)
library(survminer)
library(survival)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#theme_set(theme_gray(base_size = 10, base_family = "Meiryo"))        # Windows用
#theme_set(theme_gray(base_size = 10, base_family = "IPAGothic"))     # Ubuntu用
#showtext::showtext_auto()                                            # Cloud用
#theme_set(theme_gray(base_size = 10, base_family = "noto"))          # Cloud用

#カテゴリ変数の作成

dataset <- multureg2_dataset %>%
  mutate(event_c = factor(event,levels = 0:1,labels = c("入院中","退院")))%>%
  mutate(oral_care_c = factor(oral_care,levels = 0:1,labels = c("未実施","実施"))) %>% 
  mutate(cohabiting_family_c = factor(cohabiting_family,levels = 0:1,labels = c("同居無","同居有")))%>%
  mutate(housing_ownership_c = factor(housing_ownership,levels = 0:1,labels = c("賃貸","所有")))%>%
  mutate(municipality_scale_c = factor(municipality_scale,levels = 1:3,labels = c("一般市","中核市","政令市")))%>%
  mutate(care_level_c = factor(care_level,levels = 1:5,labels = c("要介護度1","要介護度2","要介護度3","要介護度4","要介護度5")))

view(dataset)
#KM曲線を書く
kmfit_oral_care <- survfit( Surv(discharge_days, event) ~ oral_care_c, data = dataset )

#表を出す
print(kmfit_oral_care)

#単純描画
ggsurvplot(kmfit_oral_care)

#信頼区間をつける
ggsurvplot(kmfit_oral_care,conf.int=TRUE)

#日本語の軸と凡例を追加
ggsurvplot(kmfit_oral_care,conf.int=TRUE,
           title= "退院日数×口腔ケア",
           xlab="在院日数",
           ylab="入院割合",
           legend=c(0.8,0.8), #"top"/"bottom"/"left"/"right"/"none"またはc(x,y)でx,y=0-1を指定
           legend.title="口腔ケア",
           legend.labs=c("口腔ケアなし","口腔ケアあり"))

#KM曲線を書く（要介護度）
kmfit_care_level <- survfit( Surv(discharge_days, event) ~ care_level, data = dataset )

#表を出す
print(kmfit_care_level)

#単純描画
ggsurvplot(kmfit_care_level)

#信頼区間をつける
ggsurvplot(kmfit_care_level,conf.int=TRUE)

#日本語の軸と凡例を追加
ggsurvplot(kmfit_care_level,conf.int=TRUE,
           title= "退院日数×要介護度",
           xlab="在院日数",
           ylab="入院割合",
           legend=c(0.8,0.8), #"top"/"bottom"/"left"/"right"/"none"またはc(x,y)でx,y=0-1を指定
           legend.title="要介護度",
           legend.labs=c("要介護度1","要介護度2","要介護度3","要介護度4","要介護度5"))

