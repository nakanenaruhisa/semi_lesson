#このファイルは、重回帰分析を学習するためのRスクリプトです。
#このスクリプトは、4つのデータセットを読み込み、それらを統合します
#次に、要介護認定率を従属変数として、高齢化率、高齢者有業率、県民所得、平均歩数を独立変数として単回帰分析を行います。
#最後に、AIC基準で最適な変数を選び、多重共線性のチェックを行います。

#Rを使う下準備
#作業ディレクトリの確認と固定
getwd()
setwd("/Users/naruhisa/Library/CloudStorage/OneDrive-学校法人立命館/lecture/semi/R/semi_lesson")
#　↑ここは自分のディレクト入りにあわせて変える

#全部の変数を消す
rm(list=ls())


#packageの準備
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


#テーマのセット
theme_set(theme_gray(
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#フォントの固定
jp_font <- "Hiragino Sans"
theme_set(theme_gray(base_family = jp_font, base_size = 11))

#datasetを4つ読み込む
agedper <- read_csv("Lesson12_重回帰分析1/agedper.csv")
having_job_rate <- read_csv("Lesson12_重回帰分析1/having_job_rate.csv")
nintei_sum <- read_csv("Lesson12_重回帰分析1/nintei_sum.csv")
income_person <- read_csv("Lesson12_重回帰分析1/income_person.csv")
walking_point <- read_csv("Lesson12_重回帰分析1/walking_point.csv")

agedper %>% as_tibble
having_job_rate %>% as_tibble()
nintei_sum %>% as_tibble()
income_person %>% as_tibble()
walking_point %>% as_tibble()

#walk_pointを都道府県ごとに平均を取る
walking_point <- walking_point %>%
  group_by(prefid) %>%
  summarise(walk_point = mean(walk_point))


#datasetを統合する 

view(agedper)

#高齢化率データの都道府県名に入った数字を削る（4文字名から7文字目まで残す）
agedper$pref <- str_sub(agedper$pref,start = 4,end = 7)

view(agedper)

#4つのデータセットを統合する
ltc_pref <- full_join(agedper,having_job_rate, by = "prefid")
ltc_pref <- full_join(ltc_pref,income_person,by = "prefid")
ltc_pref <- full_join(ltc_pref,nintei_sum,by = "prefid")
ltc_pref <- full_join(ltc_pref,walking_point,by = "prefid")

as_tibble(ltc_pref)

#認定率変数の作成
ltc_pref <- ltc_pref %>%
  mutate("nintei_per" = nintei_sum/population*100) 

view(ltc_pref)

# 必要な変数だけ抜き出す
ltc_pref_all <- ltc_pref %>%
  select(prefID=prefid,pref=pref.x.x,nintei_per,aged_rate,having_job_rate,income_person,walk_point) 

#　分析にかけられるcsv
as_tibble(ltc_pref_all)

#table1
tbl_summary(ltc_pref_all)

#統合したデータセットを表にする
ltc_pref_all %>%
  select(nintei_per,aged_rate,having_job_rate,income_person,walk_point) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} (±{sd})"),
    label = list(
      nintei_per ~ "要介護認定率",
      aged_rate ~ "高齢化率",
      having_job_rate ~ "高齢者有業率",
      income_person ~ "県民所得",
      walk_point ~ "平均歩数"
    )
  )

#従属変数の分布を確認する（正規分布か、ポアソン分布か？）
ggplot(data = ltc_pref_all) +
  aes(x = nintei_per) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  theme_gray(base_size = 15) 

#ここからは変数ごとの単回帰分析をしていきます。
#要介護認定数従属変数として単回帰分析（高齢化率）

reg_aged_rate <- lm(data = ltc_pref_all, formula = nintei_per ~ aged_rate)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = aged_rate, y = nintei_per)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(aes(label = pref), size = 2, vjust = 4, family = jp_font) +
  scale_colour_tableau()+
  theme_gray(base_size = 15)#grayテーマで



#要介護認定率を従属変数として単回帰分析（高齢者有業率）
reg_having_job_rate <- lm(data = ltc_pref_all, formula = nintei_per ~ having_job_rate)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = having_job_rate, y = nintei_per)+
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_text(aes(label = pref), size = 2, vjust = 4, family = jp_font) +
  scale_colour_tableau()+
  theme_gray(base_size = 15)  #grayテーマで

#要介護認定率を従属変数として単回帰分析（県民所得）
reg_income_person <- lm(data = ltc_pref_all, formula = nintei_per ~ income_person)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = income_person, y = nintei_per) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm") +    # 回帰直線を描く
  geom_text(aes(label = pref), size = 2, vjust = 4, family = jp_font) +
  scale_colour_tableau() +
  theme_gray(base_size = 15)     # grayテーマで

#要介護認定率を従属変数として単回帰分析（平均歩数）
reg_walk_point <- lm(data = ltc_pref_all, formula = nintei_per ~ walk_point)

#プロットを回帰直線を入れて書いてみて
ggplot(data = ltc_pref_all) +     
  aes(x = walk_point, y = nintei_per) +
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm") +    # 回帰直線を描く
  geom_text(aes(label = pref), size = 2, vjust = 4, family = jp_font) +
  scale_colour_tableau() +
  theme_gray(base_size = 15)     # grayテーマで


#ここからは変数を同時に投入する重回帰分析をしていきます。
#ただし、全部の変数を入れてから、有意なもの選んで考察するのは邪道です。
#回帰分析は仮説を立ててから、その仮説を検証するために行います。


#重回帰分析の前に、変数を標準化します
#===========================================
# 標準化により、異なるスケールの変数間で係数を比較しやすくなります。
# 標準化係数（β係数）は、独立変数が1標準偏差変化したときの
# 従属変数の標準偏差単位での変化を示します。

# 標準化された変数を作成(scaleで行います)
ltc_pref_all_std <- ltc_pref_all %>%
  mutate(
    nintei_per_std = scale(nintei_per)[,1],
    aged_rate_std = scale(aged_rate)[,1],
    having_job_rate_std = scale(having_job_rate)[,1],
    income_person_std = scale(income_person)[,1],
    walk_point_std = scale(walk_point)[,1]
  )

# 標準化された重回帰分析（全変数）
gml_result_std <- lm(data = ltc_pref_all_std,
                     nintei_per_std ~ aged_rate_std + having_job_rate_std + 
                       income_person_std + walk_point_std)

# AIC基準で最適な変数を選ぶ（標準化版）
step(gml_result_std)

# 標準化されたモデルリスト
gml_result_std <- list()
# モデル1（高齢化率のみ、標準化）
gml_result_std[['model_1']] <- lm(nintei_per_std ~ aged_rate_std, 
                                  data = ltc_pref_all_std)
# モデル2（高齢化率+高齢者有業率、標準化）
gml_result_std[['model_2']] <- lm(nintei_per_std ~ aged_rate_std + having_job_rate_std, 
                                  data = ltc_pref_all_std)
# モデル3（高齢化率＋高齢者有業率+県民所得、標準化）
gml_result_std[["model_3"]] <- lm(nintei_per_std ~ aged_rate_std + having_job_rate_std + 
                                    income_person_std, data = ltc_pref_all_std)

# 標準化された回帰表テーブル
modelsummary(gml_result_std,
             statistic = "conf.int",
             conf_level = 0.95,
             title = "標準化された重回帰分析結果")

# 標準化された回帰表テキスト
texreg::screenreg(gml_result_std)

# 標準化係数の可視化（切片あり）
tidy(gml_result_std[["model_3"]], conf.int = TRUE,
     exclude_intercept = TRUE) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_gray(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "標準化係数（β）", 
       title = "標準化された重回帰分析の係数")

# 標準化係数の可視化（ggcoef）
ggcoef(
  gml_result_std[["model_3"]],
  mapping = aes_string(y = "term", x = "estimate"),
  conf.int = TRUE,
  exclude_intercept = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  vline = TRUE,
  vline_color = "red",
  vline_linetype = "solid",
  errorbar_color = "black",
  errorbar_height = .15
)
