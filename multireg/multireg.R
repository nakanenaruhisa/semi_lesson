#このファイルは、重回帰分析を学習するためのRスクリプトです。
#このスクリプトは、4つのデータセットを読み込み、それらを統合します
#次に、要介護認定率を従属変数として、高齢化率、高齢者有業率、県民所得、平均歩数を独立変数として単回帰分析を行います。
#次に、要介護認定率を従属変数として、高齢化率、高齢者有業率、県民所得、平均歩数を独立変数として重回帰分析を行います。
#最後に、AIC基準で最適な変数を選び、多重共線性のチェックを行います。

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

#datasetを4つ読み込む
agedper <- read_csv("multireg/agedper.csv")
having_job_rate <- read_csv("multireg/having_job_rate.csv")
nintei_sum <- read_csv("multireg/nintei_sum.csv")
income_person <- read_csv("multireg/income_person.csv")
walk_point <- read_csv("multireg/walking_point.csv")

agedper %>% as_tibble
having_job_rate %>% as_tibble()
nintei_sum %>% as_tibble()
income_person %>% as_tibble()
walk_point %>% as_tibble()

#walk_pointを都道府県ごとに平均を取る
walk_point <- walk_point %>%
  group_by(prefid) %>%
  summarise(walk_point = mean(walk_point))

#datasetを統合する 

#高齢化率データの都道府県名に入った数字を削る
agedper$pref <- str_sub(agedper$pref,start = 4,end = 6)

#4つのデータセットを統合する
ltc_pref <- full_join(agedper,having_job_rate, by = "prefid")
ltc_pref <- full_join(ltc_pref,income_person,by = "prefid")
ltc_pref <- full_join(ltc_pref,nintei_sum,by = "prefid")
ltc_pref <- full_join(ltc_pref,walk_point,by = "prefid")

as_tibble(ltc_pref)

#認定率変数の作成
ltc_pref <- ltc_pref %>%
  mutate("nintei_per" = nintei_sum/population*100) 

ltc_pref_all <- ltc_pref %>%
  select(prefid,pref.x,nintei_per,aged_rate,having_job_rate,income_person,walk_point) 


as_tibble(ltc_pref_all)
tbl_summary(ltc_pref_all)

#統合したデータセットを表にする
ltc_pref_all %>%
  select(nintei_per,aged_rate,having_job_rate,income_person,walk_point) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))#~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る

#従属変数の分布を確認する（正規分布か、ポアソン分布か？）
ggplot(data = ltc_pref_all) +
  aes(x = nintei_per) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")

#ここからは変数ごとの単回帰分析をしていきます。
#要介護認定数従属変数として単回帰分析（高齢化率）

reg_aged_rate <- lm(data = ltc_pref_all, formula = nintei_per ~ aged_rate)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = nintei_per, y = aged_rate, label = pref.x)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(aes(y = aged_rate, label = pref.x), size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_grey(base_size = 15)+#grayテーマで
  theme_gray(base_family="HiraginoSans-W3") #文字化けしないおまじない


#要介護認定率を従属変数として単回帰分析（高齢者有業率）
reg_having_job_rate <- lm(data = ltc_pref_all, formula = nintei_per ~ having_job_rate)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = nintei_per, y = having_job_rate, label = pref.x)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(size = 2, vjust = 4)+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#要介護認定率を従属変数として単回帰分析（県民所得）
reg_income_person <- lm(data = ltc_pref_all, formula = nintei_per ~ income_person)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = nintei_per, y = income_person, label = pref.x) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm") +    # 回帰直線を描く
  geom_text(size = 2, vjust = 4)+
  scale_colour_tableau() +
  theme_gray(base_size = 15) +    # grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") # 文字化けしないおまじない

#要介護認定率を従属変数として単回帰分析（平均歩数）
reg_walk_point <- lm(data = ltc_pref_all, formula = nintei_per ~ walk_point)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = nintei_per, y = walk_point, label = pref.x) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm") +    # 回帰直線を描く
  geom_text(size = 2, vjust = 4)+
  scale_colour_tableau() +
  theme_gray(base_size = 15) +    # grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") # 文字化けしないおまじない


#ここからは変数を同時に投入する重回帰分析をしていきます。
#ただし、全部の変数を入れてから、有意なもの選んで考察するのは邪道です。
#回帰分析は仮説を立ててから、その仮説を検証するために行います。

#重回帰分析 
gmlresult <- lm(nintei_per ~ aged_rate + having_job_rate + income_person + walk_point, data = ltc_pref_all)



## AIC基準で最適な変数を選ぶ。モデルに影響を与えない変数を削る提案をしてくれます。
step(gmlresult)

#外すことを提案された変数を削除
gmlresult <- lm(nintei_per ~ aged_rate + having_job_rate + income_person, data = ltc_pref_all)


# 多重共線性のチェック
vif(gmlresult)
summary(gmlresult)

gmlresult <- list()
#モデル1（高齢化率のみ）
gmlresult[['model_1']] <- lm(nintei_per ~ aged_rate, data =ltc_pref_all)
#モデル2（高齢化率+高齢者有業率）
gmlresult[['model_2']] <- lm(nintei_per ~ aged_rate + having_job_rate, data =ltc_pref_all)
#モデル3（高齢化率＋高齢者有業率+県民所得）
gmlresult[["model_3"]] <- lm(nintei_per ~ aged_rate + having_job_rate　+ income_person , data =ltc_pref_all)

#回帰表テーブル
modelsummary (gmlresult)

#回帰表テキスト

install.packages("texreg")
library(texreg)
texreg::screenreg(gmlresult)

#coefpoot
install.packages("coefplot")
library(coefplot)
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