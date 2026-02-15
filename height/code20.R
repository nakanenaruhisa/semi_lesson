#このコードは身長・体重データの基本的な分析を行うためのものです
#--------------------データの準備--------------------

#環境のクリーンアップ
rm(list=ls()) #既存の変数をすべて削除し、クリーンな状態で始める

#必要なパッケージの読み込み
library(tidyverse)#tidyverse: データ操作・可視化のための統合パッケージ
library(ggplot2)#ggplot2: グラフ作成用(tidyverseに含まれる)
library(dbplyr)#dbplyr: データベース操作用
library(GGally)#GGally: ペアプロット等の高度なプロット用
library(ggthemes)#ggthemes: ggplot2の追加テーマ
library(gtsummary)#gtsummary: 要約統計量の表作成用
library(summarytools)#summarytools: データ要約用
library(ggpmisc)#ggpmisc: ggplot2の拡張機能
library(psych)#psych: 心理統計用


#--------------------グラフの設定--------------------
#システムフォントを使用した基本テーマの設定
theme_set(theme_gray(
  base_size = 11,        #基本の文字サイズ
  base_line_size = 0.2,  #グラフの罫線の太さ
  base_rect_size = 0.2   #グラフの外枠の太さ
))

#日本語フォント設定（システムに応じて適切なフォントを使用）
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  font_family <- "HiraginoSans-W3"
} else if (Sys.info()["sysname"] == "Windows") {  # Windows
  font_family <- "Yu Gothic"
} else {  # その他のOS
  font_family <- ""
}

#フォント設定の適用
update_geom_defaults("text", list(family = font_family))
theme_update(text = element_text(family = font_family))

#--------------------データの読み込みと前処理--------------------

#CSVファイルからデータを読み込む
ten_kukan20 <- read_csv("height/ten_kukan20.csv")

#簡略化提案: 以下の変数代入は不要かもしれません
#tidyverseを使用する場合、データフレームのまま処理する方が効率的です
height <- (ten_kukan20$height)
weight <- (ten_kukan20$weight)
sex <- (ten_kukan20$sex)

#性別を数値から意味のあるカテゴリ（女性/男性）に変換
#mutate()を使用してデータフレーム内で直接変換
ten_kukan20 <- ten_kukan20 %>% 
  mutate(sex_c = factor(sex, levels = 1:2, labels = c("女性","男性")))

#データの先頭部分を確認（データ構造の確認）
head(ten_kukan20)

#性別の分布をテーブルを作成して確認
ten_kukan20 %>% with(table(sex_c))

#基本統計量の算出
#身長の平均値
mean(height)

#体重の平均値
mean(weight)

#男性の身長の平均値を出してみて
male_height_mean <- subset(height,ten_kukan20$sex_c == "男性")
male_height_mean <- mean(male_height_mean) 
print(male_height_mean)

#女性の身長の平均値を出してみて
female_height_mean <- subset(height,ten_kukan20$sex_c == "女性")
female_height_mean <- mean(female_height_mean)
print(female_height_mean)

#身長の標準偏差(出してみて
sd(height)
#体重の標準偏差出してみて
sd(weight)

#男性の身長の標準偏差（サンプルの分散）出してみて
male_height_sd <- subset(height,ten_kukan20$sex_c == "男性")
male_height_sd <- sd(male_height_sd)
print(male_height_sd)

#女性の身長の標準偏差（サンプルの分散）出してみて
female_height_sd <- subset(height,ten_kukan20$sex_c == "女性") 
female_height_sd <- sd(female_height_sd)
print(female_height_sd)

#データを全部一気に出す
summary(ten_kukan20)
describe(ten_kukan20)


#データセット全体のテーブルを自動でつくる
ten_kukan20 %>% 
  tbl_summary()

#↑上と同じ意味＝データセット全体のテーブルを自動でつくる
tbl_summary(ten_kukan20)

#記述統計量にラベルを付けてテーブルで出力する
ten_kukan20 %>% 
  select(height,weight,sex_c) %>% 
  tbl_summary(label = list(sex_c ~ "性別",
                           height ~ "身長",
                           weight ~ "体重"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ±{sd}"), 
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "身長と体重テーブル") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#せっかくだから男女別に書いてみる
ten_kukan20 %>% 
  select(height,weight,sex_c) %>% 
  tbl_summary(label = list(sex_c ~ "性別",
                           height ~ "身長",
                           weight ~ "体重"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ±{sd}"),
              by = sex_c,
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "男女別身長体重") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#男女の平均身長に有意差はある？
t.test(height,sex,var.equal = T)

#プロットサイズの設定
options(repr.plot.width=5, repr.plot.height=5)  # グラフのサイズを設定

#身長と体重のプロット
ggplot(data = ten_kukan20) +     
  aes(x = height, y = weight) +  
  geom_point() +                 
  labs(x = "身長", y = "体重") +
  theme(text = element_text(size = 15))

# 男女別に色分けしたプロット
ggplot(data = ten_kukan20, aes(x = height, y = weight, colour = sex_c)) + # ten_kukan20データでキャンバス準備
  geom_point(size = 2) +                  # 散布図を描く
  scale_colour_tableau() +
  theme_gray(base_size = 15) + # grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") + # 文字化けしないおまじない
  geom_smooth(method = "lm", formula = y ~ x) + 
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(stat(eq.label))),
               parse = TRUE)

# 男性と女性の身長の箱ひげ図
ggplot(data = ten_kukan20) +     # tenkukan20データでキャンバス準備
  aes(x = sex_c, y = height, fill = sex_c) + # height, weight列をx, y軸にmapping, sexごとに色分け
  geom_boxplot() +                  # 箱ひげ図を描く
  xlab("sex") + ylab("height") +
  scale_fill_tableau() +
  theme_gray(base_size = 15) + # grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") # 文字化けしないおまじない

#男性と女性の身長の箱ひげ図にプロットもしてみよう
library(cowplot)

ggplot(data = ten_kukan20) +
  aes(x = sex_c, y = height, fill = sex_c) +
  geom_boxplot() +
  geom_jitter(aes(x = sex_c, y = height, color = sex_c), width = 0.1, shape = 21, colour = "BLACK", size = 3) + # shapeパラメータで輪郭線のある円を指定
  xlab("sex") + ylab("height") +
  scale_fill_tableau() +
  theme_cowplot( 26 ) +
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")

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
  scale_fill_tableau() +
  theme_cowplot(26) +
  theme_gray(base_size = 15) +
  theme_gray(base_family = "HiraKakuPro-W3")

#男性と女性の身長の平均の比較をseエラーバーをつけて書く（ggplot）
ggplot(data = ten_kukan20)+
  aes(x = sex_c, y = height, color = sex_c)+
  stat_summary(aes(x = sex_c),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("sex") + ylab("height") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#参考新しいパッケージtidyplotsを使ってみる
install.packages("tidyplots")
library(tidyplots)

ten_kukan20 %>% 
  tidyplot(x = sex_c, y = height, color = sex_c) %>% 
  add_boxplot() %>% 
  add_test_pvalue(ref.group = 1)%>%
  adjust_size(width = 120, height = 120) %>%
  adjust_font(fontsize = 15, family = NULL, face = NULL, color = "black")
