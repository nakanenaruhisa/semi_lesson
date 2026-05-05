#Rを使う下準備
#全部の変数を消す
rm(list = ls())

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

# 作業ディレクトリの確認
getwd()
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
setwd("OneDrive-学校法人立命館/lecture/semi/R/semi_lesson")

#テーマのセット
theme_set(theme_gray(
  base_family = if (interactive()) "HiraginoSans-W3" else "sans", # macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#dataをインポート
logsticreg_sample <- read_csv(
  "Lesson14_logistic_regression1/logsticreg_sample_fixed.csv"
)
data <- logsticreg_sample

print(data)

#dataにラベルをつける
data <- data %>%
  mutate(
    regidencial_status = factor(regidencial_status, labels = c("在宅", "施設")),
    gender = factor(gender, labels = c("男性", "女性")),
    nintei_grade = as.numeric(nintei_grade), # 連続変数として扱う
    family_status = factor(
      family_status,
      labels = c("同居家族なし", "同居家族あり")
    ),
    public_assistance = factor(
      public_assistance,
      labels = c("介護保険利用なし", "介護保険利用あり")
    ),
    public_pension = factor(
      public_pension,
      labels = c("公的年金なし", "公的年金あり")
    )
  )

#datasetの要約表
tbl_summary(data)

#regidencial_statusをfactor型に変換
data$regidencial_status <- as.factor(data$regidencial_status)

# ---------------------------------------------------------------------------
# 【本レッスン（Lesson14）で連続変数を標準化する理由：Lesson12・13との使い分け】
# Lesson12・13の重回帰（OLS）では非標準化係数（B）を採用した。
# 本レッスンおよび Lesson15 のロジスティック回帰では、教材として連続変数を
# 標準化する。以下は重回帰における係数の標準化の使い分け指針の要約である。
#
# ■ 基本的な違い（非標準化 B と標準化に相当する読み方）
#   - B：元の変数の単位をもつ。「X が 1 単位増えると Y が B 単位変化」。
#     実務的予測・政策判断に向く。
#   - 標準化後：単位なし（標準偏差単位）。「X が 1SD 増えると Y が βSD 相当で
#     変化」に読み替えやすい。変数間の相対的影響力の比較に向く。
#
# ■ 標準化（β に近い解釈）を使う場面（本コードが該当しうるもの）
#   ① 異なる単位・スケールの変数を同じ土俵で比べたいとき
#      例：年齢（歳）と要介護認定レベル等が同居・施設入所などに与える影響の
#      相対的な強さを比較する教材目的。
#   ② 測定単位の 1 単位が直感しにくい連続尺度のとき。
#   ③ 探索的研究・変数選択の段階で、相対的重要性をスクリーニングするとき。
#
# ■ 非標準化（B）を使う場面（Lesson12・13で採用した理由と対になる）
#   ① 単位が実質的に意味を持つとき（政策解釈）。
#   ② 異なるサンプル・研究間の比較では、β は各標本の SD に依存しやすい。
#   ③ ダミー変数は B のままの方が明快なことが多い（本コードは factor を標準化しない）。
#   ④ 予測・シミュレーションには非標準化係数が必要。
#
# ■ 社会福祉研究の実践イメージ
#   「どの要因が最も強く影響しているか」→ 標準化寄りの報告も有効。
#   「介入・政策の効果をどう解釈するか」→ 非標準化（B）を主要に。
#   論文・学会では B・SE・（必要に応じ β）・p を併記するのが一般的な例も多い。
#
# ■ よくある誤解
#   ・β が大きい＝重要、は絶対ではない（多重共線性で歪む）。
#   ・標準化＝理論的重要性の公平化ではない。
#   ・ダミー変数の標準化は流派が分かれる（本教材は factor は非標準化）。
# ---------------------------------------------------------------------------

#ロジスティック回帰の前に標準化
#===========================================
# 連続変数（age, nintei_grade）を標準化します。
# カテゴリ変数はfactorのまま扱います。
data_std <- data %>%
  mutate(
    age_std = scale(age)[, 1],
    nintei_grade_std = scale(nintei_grade)[, 1]
  )

#居住状態と年齢の単回帰のプロット書いてみて
ggplot(data = data) +
  aes(x = age, y = regidencial_status) +
  geom_point() + # 散布図を描く
  geom_smooth(method = "lm") + #回帰直線を描く
  scale_colour_tableau() +
  theme_gray(base_size = 15) #grayテーマで

#居住状態と同居家族の単回帰のプロット書いてみて
ggplot(data = data) +
  aes(x = family_status, y = regidencial_status) +
  geom_point() + # 散布図を描く
  geom_smooth(method = "lm") + #回帰直線を描く
  geom_jitter(height = 0.1, width = 0.1) +
  scale_colour_tableau() +
  theme_gray(base_size = 15) #grayテーマで


#居住状態を従属変数としてロジスティック回帰分析
logireg <- glm(
  data = data_std,
  regidencial_status ~ age_std +
    gender +
    nintei_grade_std +
    family_status +
    public_assistance +
    public_pension,
  family = binomial(link = "logit")
)

# 多重共線性のチェック
vif(logireg)
#オッズ比の計算
exp(logireg$coefficients)
summary(logireg)

#居住状態を従属変数としてロジスティック回帰分析
logireg <- list()
logireg[['Model 1']] <- glm(
  data = data_std,
  regidencial_status ~ age_std + gender,
  family = binomial(link = "logit")
)
logireg[['Model 2']] <- glm(
  data = data_std,
  regidencial_status ~ age_std + gender + nintei_grade_std,
  family = binomial(link = "logit")
)
logireg[['Model 3']] <- glm(
  data = data_std,
  regidencial_status ~ age_std + gender + nintei_grade_std + family_status,
  family = binomial(link = "logit")
)
logireg[['Model 4']] <- glm(
  data = data_std,
  regidencial_status ~ age_std +
    gender +
    nintei_grade_std +
    family_status +
    public_assistance +
    public_pension,
  family = binomial(link = "logit")
)

# 決定係数を含むモデル要約表を表示
modelsummary(
  logireg,
  statistic = "conf.int",
  stars = TRUE,
  gof_map = c("n", "r2", "aic", "bic"),
  title = "ロジスティック回帰分析の結果"
)


install.packages("coefplot")
library(coefplot)

#ggcoef
logireg <- glm(
  data = data_std,
  regidencial_status ~ age_std +
    gender +
    nintei_grade_std +
    family_status +
    public_assistance +
    public_pension,
  family = binomial(link = "logit")
)

#ggcoefでプロットを書いてみて
ggcoef(
  logireg,
  mapping = aes_string(y = "term", x = "estimate"),
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  exclude_intercept = TRUE,
  vline = TRUE,
  vline_color = "red",
  vline_linetype = "solid",
  errorbar_color = "black",
  errorbar_height = .25
)
