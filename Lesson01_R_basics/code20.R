#=============================================================
# Rの超基本操作 ─ まずはRに慣れよう！
#=============================================================

#Rは電卓として使える！四則演算をやってみよう
1 + 1        # 足し算
10 - 3       # 引き算
4 * 5        # 掛け算
10 / 3       # 割り算
2 ^ 3        # べき乗（2の3乗 = 8）
10 %% 3      # 余り（10÷3の余り = 1）

#変数に値を入れてみよう（<- が代入の記号）
x <- 5
y <- 3
x + y        # 変数同士の計算もできる
x * y

#文字列も変数に入れられるよ
name <- "Ritsumeikan"
name         # 中身を確認

#ベクトル = 数字や文字の並び。c() で作る
numbers <- c(10, 20, 30, 40, 50)
numbers      # 中身を確認
numbers[3]   # 3番目の要素を取り出す（30）
length(numbers) # ベクトルの長さ（要素数）

#データ型を確認する関数 class()
class(x)       # "numeric"（数値）
class(name)    # "character"（文字列）
class(TRUE)    # "logical"（論理値：TRUEかFALSE）

#=============================================================
# Rを使う下準備
#=============================================================

#全部の変数を消す
rm(list=ls())

#今の作業フォルダを確認するコマンド
getwd() 
#今の作業フォルダを設定するコマンド
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
if (grepl("^Lesson[0-9]", basename(getwd())) || basename(getwd()) %in% c("folder_format", "applied")) setwd("..")

# パッケージの準備
library(tidyverse)
library(ggplot2)
library(dbplyr)
library(GGally)
library(ggthemes)
library(gtsummary)
library(summarytools)
# Rscript 等で X11 が使えない環境では dfSummary 等の表示を無効化
if (!interactive()) st_options(use.x11 = FALSE)
# ggpmisc は Matrix 1.6+ が必要。読めない場合は回帰式表示のみスキップ
has_ggpmisc <- suppressWarnings(requireNamespace("ggpmisc", quietly = TRUE))
if (has_ggpmisc) library(ggpmisc)
library(psych)

#テーマのセット（Rscript 等では Hiragino が使えないため sans にフォールバック）
base_fam <- if (interactive()) "HiraginoSans-W3" else "sans"
theme_set(theme_grey(base_family = base_fam))
theme_set(theme_gray(
  base_family = base_fam,
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#datasetを読み込む
ten_kukan20 <- read_csv("Lesson01_R_basics/ten_kukan20.csv")

#=============================================================
# データの構造を確認しよう ─ 読み込んだデータを調べる
#=============================================================

#データの構造を確認する（列の型や中身がわかる）
str(ten_kukan20)

#データの最初の6行を見る
head(ten_kukan20)   # 最初の6行を表示

# データ全体をExcelっぽい画面で見たいときに使う（対話環境でのみ開く）
if (interactive()) View(ten_kukan20)

#変数を箱に入れるよ
height <- (ten_kukan20$height)
weight <- (ten_kukan20$weight)
sex <- (ten_kukan20$sex)

#性別をカテゴリ変数に変換
ten_kukan20 <- ten_kukan20 %>% 
  mutate(sex_c = factor(sex,levels = 1:2,labels = c("女性","男性")))

head(ten_kukan20)  # 変換後のデータを確認

#性別がカテゴリ変数に変換できていることを確認
ten_kukan20 %>% with(table(sex_c))
sex_c <- (ten_kukan20$sex_c)

#身長の平均値を出してみて
mean(height)

#体重の平均値を出してみて
mean(weight)

#男性の身長の平均値を出してみて
male_height_mean <- subset(height,sex_c == "男性")
male_height_mean <- mean(male_height_mean) 
print(male_height_mean)

#女性の身長の平均値を出してみて
female_height_mean <- subset(height,sex_c == "女性")
female_height_mean <- mean(female_height_mean)
print(female_height_mean)

#身長の標準偏差出してみて
sd(height)
#体重の標準偏差出してみて
sd(weight)

#男性の身長の標準偏差（サンプルの分散）出してみて
male_height_sd <- subset(height,sex_c == "男性")
male_height_sd <- sd(male_height_sd)
print(male_height_sd)

#女性の身長の標準偏差（サンプルの分散）出してみて
female_height_sd <- subset(height,sex_c == "女性") 
female_height_sd <- sd(female_height_sd)
print(female_height_sd)

#=============================================================
# 平均と標準偏差以外の基本統計量も出してみよう
#=============================================================

#中央値（データを並べたときの真ん中の値）
median(height)
median(weight)

#最大値と最小値
max(height)    # 一番身長が高い人
min(height)    # 一番身長が低い人
max(weight)    # 一番体重が重い人
min(weight)    # 一番体重が軽い人

#範囲（最小値と最大値をセットで表示）
range(height)

#四分位数（データを4等分する値）
quantile(height)              # 0%, 25%, 50%, 75%, 100% を表示
quantile(height, probs = 0.25) # 第1四分位数だけ取り出す
quantile(height, probs = 0.75) # 第3四分位数だけ取り出す

#四分位範囲（IQR = Q3 - Q1、箱ひげ図の箱の高さに相当）
IQR(height)

# めんどくさいデータは全部一気に出す
summary(ten_kukan20)
tryCatch(describe(ten_kukan20), error = function(e) message("describe (psych): ", e$message))

#=============================================================
# tidyverseの基礎操作 ─ データを自在に操ろう
#=============================================================
# tidyverseでは %>%（パイプ演算子）を使ってコードをつなげる
# 「データを受け取って → 処理して → 次に渡す」というイメージ

#--- filter(): 条件に合う行だけ取り出す ---
#男性だけのデータを取り出す
ten_kukan20 %>% 
  filter(sex_c == "男性")

#身長170cm以上のデータを取り出す
ten_kukan20 %>% 
  filter(height >= 170)

#男性かつ身長170cm以上（複数条件はカンマで区切る）
ten_kukan20 %>% 
  filter(sex_c == "男性", height >= 170)

#--- select(): 必要な列だけ選ぶ ---
#身長と体重の列だけ取り出す
ten_kukan20 %>% 
  select(height, weight)

#ID以外の列を取り出す（-で除外できる）
ten_kukan20 %>% 
  select(-ID)

#--- arrange(): データを並び替える ---
#身長の低い順に並び替え（昇順）
ten_kukan20 %>% 
  arrange(height)

#身長の高い順に並び替え（降順 = desc）
ten_kukan20 %>% 
  arrange(desc(height))

#--- group_by() + summarise(): グループごとに集計する ---
#男女別に身長の平均と標準偏差を計算
ten_kukan20 %>% 
  group_by(sex_c) %>% 
  summarise(
    人数 = n(),                    # 人数をカウント
    平均身長 = mean(height),       # 平均
    SD身長 = sd(height),           # 標準偏差
    平均体重 = mean(weight),
    SD体重 = sd(weight)
  )

#--- mutate(): 新しい列を追加する ---
#BMIを計算して新しい列として追加してみよう
ten_kukan20 <- ten_kukan20 %>% 
  mutate(BMI = weight / (height / 100) ^ 2)

#BMIが追加されたか確認
head(ten_kukan20)

#データセット全体のテーブルを自動でつくる
ten_kukan20 %>% 
  tbl_summary()

#↑上と同じ意味＝データセット全体のテーブルを自動でつくる
tbl_summary(ten_kukan20)

# 記述統計量にラベルを付けてテーブルで出力する
# （gtsummary のバージョンによっては label に因子を含めるとエラーになる場合あり）
ten_kukan20 %>%
  select(height, weight, sex_c) %>%
  tbl_summary(
    label = list(height ~ "身長", weight ~ "体重"),
    statistic = list(all_continuous() ~ "{mean} ±{sd}"),
    digits = all_continuous() ~ 1
  )

# せっかくだから男女別に書いてみる（by= 使用時は label に by 変数を含めない）
ten_kukan20 %>%
  select(height, weight, sex_c) %>%
  tbl_summary(
    label = list(height ~ "身長", weight ~ "体重"),
    statistic = list(all_continuous() ~ "{mean} ±{sd}"),
    by = sex_c,
    digits = all_continuous() ~ 1
  )

#=============================================================
# 相関分析 ─ 2つの変数の関係を数値で見てみよう
#=============================================================

#身長と体重の相関係数を計算する（-1～1の値。1に近いほど正の相関が強い）
cor(height, weight)

#相関係数の検定（p値で「偶然ではない」と言えるか確認する）
cor.test(height, weight)
# p-value < 0.05 なら「身長と体重には統計的に有意な相関がある」と言える

#男性だけの相関係数
male_data <- ten_kukan20 %>% filter(sex_c == "男性")
cor(male_data$height, male_data$weight)

#女性だけの相関係数
female_data <- ten_kukan20 %>% filter(sex_c == "女性")
cor(female_data$height, female_data$weight)

#相関行列（複数の変数の相関を一覧で見る）
ten_kukan20 %>% 
  select(height, weight) %>% 
  cor()

#男女の平均身長に有意差はある？
t.test(height ~ sex, var.equal = TRUE)

#身長と体重のプロット書いてみて
ggplot(data = ten_kukan20) +     # tenkukan20データでキャンバス準備
  aes(x = height, y = weight)+ # height,weight列をx,y軸にmapping
  geom_point() +                  # 散布図を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans") #文字化けしないおまじない

# 男女別に色分けしたプロット
p_cor <- ggplot(data = ten_kukan20, aes(x = height, y = weight, colour = sex_c)) +
  geom_point(size = 2) +
  scale_colour_tableau() +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans") +
  geom_smooth(method = "lm", formula = y ~ x)
if (has_ggpmisc) p_cor <- p_cor + ggpmisc::stat_poly_eq(formula = y ~ x, aes(label = paste(stat(eq.label))), parse = TRUE)
p_cor  # 回帰式は ggpmisc 利用時のみ表示

#=============================================================
# ヒストグラム ─ データの分布を見てみよう
#=============================================================

#身長のヒストグラム（どのあたりの身長の人が多いかがわかる）
ggplot(data = ten_kukan20) +
  aes(x = height) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white") + # binsで棒の本数を調整
  labs(title = "身長の分布", x = "身長 (cm)", y = "人数") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#体重のヒストグラム
ggplot(data = ten_kukan20) +
  aes(x = weight) +
  geom_histogram(bins = 8, fill = "coral", color = "white") +
  labs(title = "体重の分布", x = "体重 (kg)", y = "人数") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#男女別に色を分けたヒストグラム（重ねて表示）
ggplot(data = ten_kukan20) +
  aes(x = height, fill = sex_c) +
  geom_histogram(bins = 8, alpha = 0.6, position = "identity") + # alpha=透明度、position="identity"で重ねる
  scale_fill_tableau() +
  labs(title = "男女別・身長の分布", x = "身長 (cm)", y = "人数", fill = "性別") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#=============================================================
# 密度プロット ─ ヒストグラムをなめらかにした分布の形
#=============================================================

#男女別の身長の密度プロット
ggplot(data = ten_kukan20) +
  aes(x = height, fill = sex_c, color = sex_c) +
  geom_density(alpha = 0.3) +  # alpha=透明度（0～1）
  scale_fill_tableau() +
  scale_color_tableau() +
  labs(title = "男女別・身長の密度プロット", x = "身長 (cm)", y = "密度", fill = "性別", color = "性別") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#=============================================================
# 棒グラフ ─ カテゴリごとの個数を数えて表示
#=============================================================

#性別ごとの人数を棒グラフで表示
ggplot(data = ten_kukan20) +
  aes(x = sex_c, fill = sex_c) +
  geom_bar() +                    # geom_bar()は自動でカウントしてくれる
  scale_fill_tableau() +
  labs(title = "性別ごとの人数", x = "性別", y = "人数", fill = "性別") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

# 男性と女性の身長の箱ひげ図
ggplot(data = ten_kukan20) +     # tenkukan20データでキャンバス準備
  aes(x = sex_c, y = height, fill = sex_c) + # height, weight列をx, y軸にmapping, sexごとに色分け
  geom_boxplot() +                  # 箱ひげ図を描く
  xlab("sex") + ylab("height") +
  scale_fill_tableau() +
  theme_gray(base_size = 15) + # grayテーマで
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans") # 文字化けしないおまじない

#男性と女性の身長の箱ひげ図にプロットもしてみよう
library(cowplot)

ggplot(data = ten_kukan20) +
  aes(x = sex_c, y = height, fill = sex_c) +
  geom_boxplot() +
  geom_jitter(aes(x = sex_c, y = height, color = sex_c), width = 0.1, shape = 21, colour = "BLACK", size = 3) + # shapeパラメータで輪郭線のある円を指定
  xlab("sex") + ylab("height") +
  scale_colour_tableau()+
  theme_cowplot( 26 ) +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#箱ひげ図にt検定の結果を書いてみる

library("ggsignif")

ggplot(data = ten_kukan20) +
  aes(x = sex_c, y = height, fill = sex_c) +
  geom_boxplot() +
  geom_signif(comparisons = list(c("男性", "女性")), # `sex_c`のカテゴリー名
              test = "t.test",
              na.rm = FALSE,
              map_signif_level = TRUE,
              col = "red") + 
  theme_minimal() +
  geom_jitter(aes(x = sex_c, y = height, color = sex_c), width = 0.1, shape = 21, colour = "BLACK", size = 3) + 
  xlab("sex") + ylab("height") +
  scale_colour_tableau() +
  theme_cowplot(26) +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#男性と女性の身長の平均の比較をseエラーバーをつけて書く（ggplot）
ggplot(data = ten_kukan20)+
  aes(x = sex_c, y = height, color = sex_c)+
  stat_summary(aes(x = sex_c), fun = "mean", geom = "point", size = 3) +  # 平均値のプロット
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("sex") + ylab("height") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans") #文字化けしないおまじない

#=============================================================
# グラフのカスタマイズ ─ タイトルや軸ラベルを整えよう
#=============================================================

# labs() を使えばタイトル、サブタイトル、軸ラベル、キャプションを一括で設定できる
ggplot(data = ten_kukan20, aes(x = height, y = weight, colour = sex_c)) +
  geom_point(size = 3) +
  scale_colour_tableau() +
  labs(
    title = "身長と体重の関係",              # グラフのタイトル
    subtitle = "20人分のサンプルデータ",      # サブタイトル
    x = "身長 (cm)",                         # x軸のラベル
    y = "体重 (kg)",                         # y軸のラベル
    colour = "性別",                         # 凡例のタイトル
    caption = "出典：サンプルデータ"          # 右下のキャプション
  ) +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

#=============================================================
# グラフの保存 ─ ggsave() で画像ファイルに書き出そう
#=============================================================

# まずグラフを変数に入れる
my_plot <- ggplot(data = ten_kukan20, aes(x = height, y = weight, colour = sex_c)) +
  geom_point(size = 3) +
  scale_colour_tableau() +
  labs(title = "身長と体重の関係", x = "身長 (cm)", y = "体重 (kg)", colour = "性別") +
  theme_gray(base_size = 15) +
  theme_gray(base_family = if (interactive()) "HiraKakuPro-W3" else "sans")

# グラフを表示
my_plot

# PNG形式で保存（ファイル名、幅、高さ、解像度を指定）
dir.create("height", showWarnings = FALSE)
ggsave("height/my_plot.png", plot = my_plot, width = 8, height = 6, dpi = 300)

# PDF形式で保存（論文やレポートに使うときはPDFが便利）
ggsave("height/my_plot.pdf", plot = my_plot, width = 8, height = 6)

# ggsave() のポイント：
# - ファイル名の拡張子（.png, .pdf, .jpg）で保存形式が決まる
# - width, height はインチ単位（デフォルト）
# - dpi は解像度（300あれば十分きれい）

