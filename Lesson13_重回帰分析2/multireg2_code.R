#Rを使う下準備
#packageの準備
install.packages("stringr")
library(stringr)
library(readr)
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

#作業ディレクトリの確認と固定
getwd()

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
data <- read_excel("Lesson13_重回帰分析2/multireg2_dataset.xlsx")
as_tibble(data)

#カテゴリ変数の作成
data <- data %>% 
  mutate(oral_care_c = factor(oral_care,levels = 0:1,labels = c("未実施","実施"))) %>% 
  mutate(gender_c = factor(gender,levels = 1:2,labels = c("男性","女性"))) %>% 
  mutate(cohabiting_family_c = factor(cohabiting_family,levels = 0:1,labels = c("同居無","同居有")))%>%
  mutate(housing_ownership_c = factor(housing_ownership,levels = 0:1,labels = c("賃貸","所有")))%>%
  mutate(municipality_scale_c = factor(municipality_scale,levels = 1:3,labels = c("一般市","中核市","政令市")))%>%
  mutate(care_level_c = factor(care_level,levels = 1:5,labels = c("要介護度1","要介護度2","要介護度3","要介護度4","要介護度5")))


#統合したデータセットを表にする
data <- data %>% 
  select(discharge_days,gender_c,oral_care_c,care_level_c,cohabiting_family_c,housing_ownership_c,household_income,municipality_scale_c)

tbl_summary(data)

#従属変数の分布を確認する（正規分布か、ポアソン分布か？）
ggplot(data = data) +
  aes(x = discharge_days) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")

#退院日数を従属変数として単回帰（要介護度）
reg_care_lebel <- lm(data = data, formula = discharge_days ~ care_level_c)

reg_care_lebel

#プロットを回帰直線を入れて書いてみて
ggplot(data = data) +     
  aes(x = discharge_days, y = care_level_c, label = care_level_c)+
  geom_point(color = "steelblue", alpha = 0.6) +                  # 散布図を描く
  geom_smooth(method = "lm", color = "darkred")+  #回帰直線を描く
  #geom_text(aes(y = discharge_days, label = care_level_c), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_grey(base_size = 15)+#grayテーマで
  theme_gray(base_family="HiraginoSans-W3") + #文字化けしないおまじない
  coord_flip() # グラフを横にする


#退院日数を従属変数として単回帰（口腔ケア）
reg_oral_care_c <- lm(data = data, formula = discharge_days ~ oral_care_c)

#プロットを書いてみて
ggplot(data = data) +     
  aes(x =discharge_days , y = oral_care_c, label = oral_care_c)+
  geom_point() +                  # 散布図を描く
  #geom_smooth(method = "lm")+  #回帰直線を描く
  #geom_text(aes(y = discharge_days, label = oral_care_c), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

##退院日数を従属変数として単回帰（世帯収入）
reg_household_income <- lm(data = data, formula = discharge_days ~ household_income)

#プロットを回帰直線を入れて書いてみて
ggplot(data = data) +     
  aes(x = discharge_days, y = household_income)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  #geom_text(aes(y = 県民所得, label = 都道府県), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#重回帰分析
gmlresult <- lm(data = data, discharge_days ~ gender_c + oral_care_c + care_level_c + household_income)

# 多重共線性のチェック
vif(gmlresult)
summary(gmlresult)

## AIC基準で最適な変数を選ぶ。モデルに影響を与えない変数を削る提案をしてくれます。
# step()関数は、AIC（赤池情報量基準）を使って最適なモデルを選択する関数です。
# 引数として重回帰分析の結果（gmlresult）を渡すことで、
# どの変数を削除するとAICが改善されるかを示してくれます。
# 出力では、各ステップでのAICの値と、削除を提案する変数が表示されます。
step(gmlresult)

#外すことを提案された変数を削除(今回はなし)
gmlresult <- lm(formula = discharge_days ~ gender_c + oral_care_c + care_level_c + household_income, data = data)

gmlresult <- list()
#モデル1（口腔ケアのみ）
gmlresult[['model_1']] <- lm(discharge_days ~ oral_care_c, data =data)
#モデル2（口腔ケア＋世帯収入）
gmlresult[['model_2']] <- lm(discharge_days ~ oral_care_c+ household_income, data =data)
#モデル3（口腔ケア＋世帯収入+要介護度）
gmlresult[["model_3"]] <- lm(discharge_days ~ oral_care_c+ household_income  + care_level_c , data =data)
#モデル4（口腔ケア＋世帯収入+要介護度+gender）
gmlresult[["model_4"]] <- lm(discharge_days ~ oral_care_c + household_income  + care_level_c+ gender_c,data =data)

#回帰表テーブル
modelsummary (gmlresult, statistic = 'conf.int', conf_level = .99, gof_omit = "AIC|BIC",stars = TRUE) # 99% 信頼区間)

#回帰表テキスト
texreg::screenreg(gmlresult)

#coefpoot
install.packages("coefplot")
library(coefplot)
library(broom)
coefplot(gmlresult[["model_4"]], intercept = FALSE)

#ggplot
tidy(gmlresult[["model_4"]],conf.int = TRUE,
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
ggcoef(gmlresult[["model_4"]],
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

