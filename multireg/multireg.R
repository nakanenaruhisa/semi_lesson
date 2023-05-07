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
agedper <- read_csv("multireg/agedper.csv")
having_job_rate <- read_csv("multireg/having_job_rate.csv")
nintei_sum <- read_csv("multireg/nintei_sum.csv")
income_person <- read_csv("multireg/income_person.csv")

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
gmlresult <- lm(認定率 ~ 高齢化率+県民所得+高齢者有業率, ltc_pref_all)

# 多重共線性のチェック
vif(gmlresult)
summary(gmlresult)#県民所得は有意でない可能性があるので削除

gmlresult <- list()
#モデル1（高齢化率のみ）
gmlresult[['model_1']] <- lm(認定率 ~ 高齢化率, data =ltc_pref_all)
#モデル2（高齢化率＋県民所得）
gmlresult[['model_2']] <- lm(認定率 ~ 高齢化率+県民所得, data =ltc_pref_all)
#モデル3（高齢化率＋県民所得+高齢者有業率）
gmlresult[["model_3"]] <- lm(認定率 ~ 高齢化率+県民所得+高齢者有業率, data =ltc_pref_all)

#回帰表テーブル
modelsummary (gmlresult)

#回帰表テキスト
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

#モリモリのcoefplot
term_order <- rev(c("(Intercept)","高齢化率","県民所得","高齢者有業率"))
model_order <- rev(c("model_1","model_2","model_3"))

result_data <- gmlresult %>%
  map(tidy,conf.level = 0.95,conf.int=TRUE) %>%
  bind_rows(.id = "model") %>%
  mutate(term = factor(term,levels = term_order), #項とモデルの順序付け
         model = factor(model,levels = model_order)) %>%
  filter(term != "(Intercept)") %>% #今回は簡単のため切片を除外します
  mutate(stars = case_when( #有意水準を設定する
    p.value <= 0.001 ~ "***",
    (p.value > 0.001 & p.value <= 0.01) ~ "**",
    (p.value > 0.01 & p.value <= 0.05) ~ "*",
    (p.value > 0.05 & p.value <= 0.1) ~ ".",
    TRUE ~ "")) %>%
  mutate(coef_label = paste0(round(estimate,3)," (",round(std.error,3),")",stars))

g <- ggplot(result_data, aes(x = term,
                             y = estimate,
                             col = model))
g <- g + geom_hline(yintercept = 0) #係数=0の直線
g <- g + geom_point(position = position_dodge(1)) #係数の点プロット
g <- g + geom_text(position = position_dodge(1), #係数の数値を文字列でプロット
                   vjust = -1,　#点と重ならないようにちょっと上に出します
                   aes(label = coef_label),
                   show.legend = FALSE) #これを設定しないと凡例に文字列も反映されてしまいます
g <- g + geom_pointrange(position = position_dodge(1),
                         aes(ymin = conf.low, ymax = conf.high), #エラーバー用
                         show.legend = FALSE)
g <- g + guides(col = guide_legend(reverse=T))
g <- g + coord_flip()
g <- g + ggtitle("Estimated Coefficients of Regression Model (95%CI)")
g

#決定木
install.packages("rpart.plot")
library(rpart.plot)

