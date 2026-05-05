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
setwd("..")


#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = if (interactive()) "HiraginoSans-W3" else "sans",# macOS用
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

print(dataset)

# ---------------------------------------------------------------------------
# 【本レッスン（Lesson15）で連続変数を標準化する理由：Lesson12・13との使い分け】
# Lesson12・13の重回帰（OLS）では非標準化係数（B）を採用した。
# Lesson14・15のロジスティック回帰では、教材として連続変数を標準化する。
# 以下は重回帰・一般化線形モデルにおける係数の標準化の使い分け指針の要約である。
#
# ■ 基本的な違い（非標準化 B と標準化に相当する読み方）
#   - B：元の変数の単位をもつ。「X が 1 単位増えると…」実務予測・政策判断に向く。
#   - 標準化後：単位なし（SD 単位）。変数間の相対的影響力の比較に向く。
#
# ■ 標準化をこのレッスンで採用する場面（該当するもの）
#   ① 口腔ケア・性別・同居などと並べて、要介護度（連続として投入する場合）など
#      スケールの異なる要因の相対的影響を比較する教材目的。
#   ② 探索的に「どの変数が相対的に効いているか」を見る練習。
#   ※カテゴリ（factor）は下記のとおり標準化せず factor のまま（ダミーは B で解釈しやすい）。
#
# ■ 非標準化（B）を優先する場面（Lesson12・13と同じ考え方）
#   ① 単位が政策的に意味があるとき。② 研究間比較では β は SD に依存しやすい。
#   ③ ダミーは B が明快なことが多い。④ 予測・シミュレーションには B が必要。
#
# ■ 社会福祉研究の実践イメージ
#   「どの要因が相対的に強いか」→ 標準化寄りの報告も有効。
#   「介入・政策の効果をどう言うか」→ 非標準化（B）を主要に。
#   論文・学会では B・SE・（必要に応じ β）・p の併記が一般的な例も多い。
#
# ■ よくある誤解
#   ・β が大きい＝重要、ではない（多重共線性で歪む）。
#   ・標準化＝理論的重要性の公平化ではない。
# ---------------------------------------------------------------------------

#ロジスティック回帰の前に標準化
#===========================================
# 連続変数として扱うcare_levelを標準化します。
# カテゴリ変数はfactorのまま扱います。
dataset_std <- dataset %>%
  mutate(
    care_level_std = scale(care_level)[, 1]
  )

#datasetの要約表その1
tbl_summary(dataset)

df <- dataset %>% 
  group_by(口腔ケア　= oral_care_c) %>% 
  summarise(n = n(),
            退院日数平均 = mean(discharge_days),
            退院日数標準偏差 = sd(discharge_days))

tbl_1 <- gt(df)
print(tbl_1)

logireg <- glm(data = dataset_std, event ~ oral_care_c + gender_c + care_level_c + cohabiting_family,family=binomial(link="logit"),control = glm.control(maxit = 100))

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
  theme_gray(base_size = 15) #grayテーマで

#要介護度と退院の有無のプロット書いてみて
ggplot(data = dataset) +
  aes(x = care_level, y = event)+ 
  geom_point() +                  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  geom_jitter(height=0.1, width =0.1) +
  scale_colour_tableau()+
  theme_gray(base_size = 15) #grayテーマで

#オッズ比の計算
# exp(logireg$coefficients) でもオッズ比は計算できますが、confintと組み合わせて
# 信頼区間も同時に出すのが推奨されます。

# 例：1つのモデルの場合
logireg_single <- glm(data = dataset_std, event ~ oral_care_c + gender_c + care_level_c + cohabiting_family, family=binomial(link="logit"))
# オッズ比
exp(coef(logireg))


# 複数モデルの場合、オッズ比や信頼区間付きで一括で見るなら
logireg_list <- list()
logireg_list[['Model 1']] <- glm(data = dataset_std, event ~ oral_care_c + gender_c + care_level_std, family=binomial(link="logit"))
logireg_list[['Model 2']] <- glm(data = dataset_std, event ~ oral_care_c + gender_c + care_level_std + cohabiting_family, family=binomial(link="logit"))
logireg_list[['Model 3']] <- glm(data = dataset_std, event ~ oral_care_c + gender_c + care_level_std + cohabiting_family + housing_ownership_c, family=binomial(link="logit"))

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
logireg_moderl_3 <- glm(data = dataset_std, event ~ oral_care_c + gender_c + care_level_std + cohabiting_family + housing_ownership_c, family=binomial(link="logit"))

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

