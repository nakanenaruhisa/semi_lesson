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
library(readr)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraKakuProN-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#datasetを4つ読み込む
agedper <- read_csv("semi_lesson/multireg/agedper.csv")
having_job_rate <- read_csv("semi_lesson/multireg/having_job_rate.csv")
nintei_sum <- read_csv("semi_lesson/multireg/nintei_sum.csv")
income_person <- read_csv("semi_lesson/multireg/income_person.csv")

agedper %>% as_tibble
having_job_rate %>% as_tibble()
nintei_sum %>% as_tibble()
income_person %>% as_tibble()

#都道府県別完全失業率

#都道府県名に入った数字を削る
agedper$pref <- str_sub(agedper$pref,start = 4,end = 6)

#4つのデータセットを統合する
ltc_pref <- full_join(agedper,having_job_rate, by = "prefid")
ltc_pref <- full_join(ltc_pref,income_person,by = "prefid")
ltc_pref <- full_join(ltc_pref,nintei_sum,by = "prefid")

as_tibble(ltc_pref)

#認定率変数の作成
ltc_pref_all <- ltc_pref %>%
  mutate("nintei_per" = nintei_sum/population*100) %>% 
  select(prefid,都道府県 = pref.x,認定率 = nintei_per,高齢化率 = aged_rate, 高齢者有業率 = having_job_rate,県民所得 = income_person) 

as_tibble(ltc_pref_all)
tbl_summary(ltc_pref_all)

#統合したデータセットを表にする
ltc_pref_all %>% 
  select(認定率,高齢化率, 高齢者有業率,県民所得)  %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))#~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る

#要介護認定数を従属変数として単回帰分析（高齢化率）
reg_aged_rate <- lm(data = ltc_pref_all, formula = 認定率 ~ 高齢化率)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = 認定率, y = 高齢化率, label = 都道府県)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(aes(y = 高齢化率, label = 都道府県), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_grey(base_size = 15)+#grayテーマで
  theme_gray(base_family="HiraginoSans-W3") #文字化けしないおまじない


#要介護認定数を従属変数として単回帰分析（高齢者有業率）
reg_having_job_rate <- lm(data = ltc_pref_all, formula = 認定率 ~ 高齢者有業率)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = 認定率, y = 高齢者有業率, label = 都道府県)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(aes(y = 高齢者有業率, label = 都道府県), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#要介護認定数を従属変数として単回帰分析（県民所得）
reg_income_person <- lm(data = ltc_pref_all, formula = 認定率 ~ 県民所得)


#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = 認定率, y = 県民所得)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(aes(y = 県民所得, label = 都道府県), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#相関行列
ltc_pref_all %>% 
  select(認定率,高齢化率, 高齢者有業率,県民所得) %>% 
  ggpairs(ltc_pref_all,colors = "都道府県")

#重回帰分析
gmlresult <- lm(認定率~高齢化率+県民所得+高齢者有業率, ltc_pref_all)

# 多重共線性のチェック
vif(gmlresult)
summary(gmlresult)#県民所得は有意でない可能性があるので削除

#モデル1（高齢者有業率のみ）
#モデル2（高齢者有業率+高齢化率）

gmlresult <- list()
gmlresult[['Model 1']] <- lm(認定率~高齢化率, ltc_pref_all)
gmlresult[['Model 2']] <- lm(認定率~高齢化率+高齢者有業率, ltc_pref_all)
modelsummary (gmlresult)

#coefpoot
install.packages("coefplot")
library(coefplot)
coefplot(gmlresult, intercept = FALSE)

#ggplot
tidy(gmlresult, conf.int = TRUE) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

#ggcoef
ggcoef(gmlresult,
  mapping = aes_string(y = "term", x = "estimate"),
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  exclude_intercept = FALSE,
  vline = TRUE)
