#=============================================================
# ggplot2 入門 ─ グラフの「文法」を学ぼう
#=============================================================
# ggplot2は「グラフの文法（Grammar of Graphics）」という考え方に基づいている。
# グラフを「データ」「見た目（aes）」「図の種類（geom）」の3つの部品に分けて、
# それを + で組み合わせて作っていく。
#
# 基本の形：
#   ggplot(data = データ) +
#     aes(x = X軸, y = Y軸) +
#     geom_xxx()   ← 図の種類
#
# Rの基本操作（Lesson1）は理解できている前提で進めるよ！
#=============================================================


#=============================================================
# 【準備】パッケージとデータ
#=============================================================

rm(list=ls())

library(tidyverse)  # ggplot2 も含まれている
library(ggthemes)   # きれいなテーマや配色を追加

#テーマのセット（日本語が文字化けしないようにする）
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 12
))

# Rに最初から入っている「iris（アヤメ）」データを使う
# 3種類のアヤメの花びら(Petal)とがく片(Sepal)の長さ・幅を記録したデータ
str(iris)
head(iris)
# Species = 品種（setosa, versicolor, virginica の3種）


#=============================================================
# 【STEP 1】 ggplotの3つの部品を理解しよう
#=============================================================
# ggplot2のグラフは3つの部品でできている：
#   1. data   = どのデータを使う？
#   2. aes    = どの列をx軸・y軸・色などに割り当てる？（aesthetic mapping）
#   3. geom   = どんな形のグラフにする？（geometry）

# --- まず data と aes だけ指定してみよう ---
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length)
# → 軸だけのキャンバスが表示される。geomがないので何も描かれない！

# --- geom_point() を追加すると散布図になる ---
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point()
# → がく片の長さ(x) と 花びらの長さ(y) の関係がわかる

# ※ aes() は ggplot() の中に書いてもOK（どちらの書き方もよく使う）
ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()


#=============================================================
# 【STEP 2】 aes（エステティクス）を使いこなそう
#=============================================================
# aes() の中では、データの列をグラフの「見た目」に紐づける（mapping）。
# x, y 以外にも color, fill, size, shape, alpha などがある。

# --- color: 色で品種を区別する ---
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length, color = Species) +
  geom_point()

# --- size: 点の大きさで幅（Width）を表現する ---
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length, color = Species, size = Petal.Width) +
  geom_point()

# --- shape: 点の形で品種を区別する ---
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length, shape = Species) +
  geom_point(size = 3)

# --- alpha: 透明度を変える（点が重なるとき便利） ---
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length, color = Species) +
  geom_point(size = 3, alpha = 0.6)

# ★ ポイント：aes() の中に書く → データの値に連動して変わる
#            aes() の外に書く → 全部同じ値になる（固定）
# 例：全部赤い点にしたいとき
ggplot(data = iris) +
  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point(color = "red", size = 2)  # aesの外 → 全部赤


#=============================================================
# 【課題1】 aes のマッピングを試してみよう
#=============================================================

# 課題1-1: Sepal.Width(x軸) と Sepal.Length(y軸) の散布図を描こう


# 課題1-2: 上のグラフに Species で色分けを追加しよう


# 課題1-3: 点のサイズを Petal.Width に連動させてみよう


# 課題1-4: 全部の点を size = 4, alpha = 0.5 の固定値にしてみよう
#          （aesの外に書くこと！）



#=============================================================
# 【STEP 3】 いろいろな geom を学ぼう
#=============================================================
# geom = グラフの種類。目的に合った geom を選ぶのが大事！

# ----- 3-1: geom_point() ─ 散布図 -----
# 2つの連続変数の関係を見るとき
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2)

# ----- 3-2: geom_smooth() ─ トレンドライン -----
# 散布図に傾向線を追加する
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")  # lm = 直線（線形回帰）

# method = "loess" にすると曲線になる
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  geom_smooth(method = "loess")

# ----- 3-3: geom_histogram() ─ ヒストグラム -----
# 1つの連続変数の分布を見るとき
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white")

# 品種別に色を分ける
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity")

# ----- 3-4: geom_density() ─ 密度プロット -----
# ヒストグラムをなめらかにした分布
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.4)

# ----- 3-5: geom_boxplot() ─ 箱ひげ図 -----
# グループ間の分布を比較するとき
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot()

# ----- 3-6: geom_violin() ─ バイオリンプロット -----
# 箱ひげ図 + 密度プロットを合わせたような図
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8)  # 中に小さい箱ひげ図を入れる

# ----- 3-7: geom_bar() ─ 棒グラフ（カウント） -----
# カテゴリの個数を数えて表示
ggplot(iris, aes(x = Species, fill = Species)) +
  geom_bar()

# ----- 3-8: geom_col() ─ 棒グラフ（集計済みの値） -----
# 自分で集計した値を棒グラフにするときは geom_col()
iris_summary <- iris %>%
  group_by(Species) %>%
  summarise(mean_sl = mean(Sepal.Length))

ggplot(iris_summary, aes(x = Species, y = mean_sl, fill = Species)) +
  geom_col()

# ----- 3-9: geom_jitter() ─ ジッター（ばらけた点） -----
# 箱ひげ図に個別のデータ点を重ねるとき便利
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.6)

# ----- 3-10: geom_line() ─ 折れ線グラフ -----
# 時系列データなどに使う。iris にはないので、簡単なデータを作って試す
sample_data <- data.frame(
  month = 1:12,
  sales = c(100, 120, 115, 130, 150, 180, 200, 190, 170, 160, 140, 155)
)

ggplot(sample_data, aes(x = month, y = sales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3)


#=============================================================
# 【課題2】 geom を使い分けてグラフを描こう
#=============================================================

# 課題2-1: Petal.Length のヒストグラムを描こう（binsは自分で調整してみて）


# 課題2-2: Petal.Length の密度プロットを品種別（Species）に描こう


# 課題2-3: 品種別の Petal.Width の箱ひげ図を描こう


# 課題2-4: 上の箱ひげ図に geom_jitter() で点を重ねてみよう


# 課題2-5: 品種別の Petal.Width のバイオリンプロットを描こう



#=============================================================
# 【STEP 4】 レイヤーの重ね方を理解しよう
#=============================================================
# ggplot2 では + で層（レイヤー）を重ねていく。
# 後から追加したレイヤーが上に描かれる。

# 例：散布図 → トレンドライン → ラベルの順で重なる
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2) +             # レイヤー1: 点
  geom_smooth(method = "lm", se = FALSE) +  # レイヤー2: 直線（se=FALSE で信頼区間なし）
  labs(title = "レイヤーを重ねた例")  # レイヤー3: ラベル

# 順番を変えると見え方が変わる（直線が点の下になる）
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_smooth(method = "lm", se = FALSE) +  # 先に直線
  geom_point(size = 2) +                     # 後から点（上に描かれる）
  labs(title = "レイヤーの順番を変えた例")


#=============================================================
# 【STEP 5】 labs() でグラフを整えよう
#=============================================================
# labs() でタイトル、軸ラベル、凡例タイトル、キャプションを設定できる

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2) +
  labs(
    title = "アヤメのがく片と花びらの関係",     # タイトル
    subtitle = "3品種の比較",                    # サブタイトル
    x = "がく片の長さ (cm)",                     # x軸ラベル
    y = "花びらの長さ (cm)",                     # y軸ラベル
    color = "品種",                              # 凡例タイトル
    caption = "データ出典: iris dataset (Fisher, 1936)"  # キャプション
  )


#=============================================================
# 【STEP 6】 テーマを変えてみよう
#=============================================================
# theme_xxx() でグラフ全体のデザインを変更できる

p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2) +
  labs(title = "テーマの比較", x = "がく片の長さ", y = "花びらの長さ")

# デフォルトのテーマ
p + theme_gray()

# 白背景（論文でよく使う）
p + theme_bw()

# 最小限のデザイン
p + theme_minimal()

# クラシック（罫線なし）
p + theme_classic()

# ggthemes パッケージのテーマ
p + theme_economist()   # Economist誌風
p + theme_fivethirtyeight()  # FiveThirtyEight風
p + theme_wsj()         # ウォールストリートジャーナル風


#=============================================================
# 【STEP 7】 色を変えてみよう（scale）
#=============================================================
# scale_xxx() で色や軸の範囲などを細かく制御できる

p_base <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2)

# ggthemesの配色（Tableau風）
p_base + scale_color_tableau()

# 手動で色を指定
p_base + scale_color_manual(values = c("setosa" = "#E63946",
                                       "versicolor" = "#457B9D",
                                       "virginica" = "#2A9D8F"))

# グラデーション配色（連続値のとき）
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Petal.Width)) +
  geom_point(size = 2) +
  scale_color_gradient(low = "yellow", high = "red")

# fill（塗りつぶし）の色を変えるときは scale_fill_xxx()
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1")  # RColorBrewer のパレット


#=============================================================
# 【課題3】 グラフの見た目を整えよう
#=============================================================

# 課題3-1: 品種別の Sepal.Width と Petal.Width の散布図を描いて、
#          labs() でタイトル・軸ラベル・凡例タイトルを日本語にしよう


# 課題3-2: 上のグラフに theme_minimal() を適用しよう


# 課題3-3: 色を scale_color_manual() で自分の好きな3色に変えてみよう
#          ヒント: 色の名前は "red", "blue", "green" や
#                  カラーコード "#FF6347" のような形式で指定できる



#=============================================================
# 【STEP 8】 facet ─ グラフを品種ごとに分割する
#=============================================================
# facet_wrap() でカテゴリごとに小さなグラフを並べて表示できる

# 品種ごとに分割
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Species)  # Species ごとに分割

# 列数を指定
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  facet_wrap(~ Species, ncol = 1)  # 縦に1列で並べる

# facet_wrap のパネルごとに独立した軸にする
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  facet_wrap(~ Species, scales = "free")  # 軸の範囲を品種ごとに変える


#=============================================================
# 【課題4】 facet を使ってみよう
#=============================================================

# 課題4-1: Petal.Width のヒストグラムを品種ごとに分割して描こう
#          ヒント: facet_wrap(~ Species)


# 課題4-2: 品種ごとの散布図（Sepal.Width vs Petal.Width）を
#          縦1列（ncol = 1）で並べてみよう



#=============================================================
# 【STEP 9】 軸の調整
#=============================================================

# 軸の範囲を変更する
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  xlim(4, 8) +     # x軸の範囲を 4〜8 に
  ylim(0, 8)       # y軸の範囲を 0〜8 に

# 軸の目盛りを細かく設定する（scale_x_continuous / scale_y_continuous）
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  scale_x_continuous(breaks = seq(4, 8, by = 0.5)) +  # 0.5刻みの目盛り
  scale_y_continuous(breaks = seq(0, 7, by = 1))       # 1刻みの目盛り

# 軸を反転する
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  coord_flip()  # x軸とy軸を入れ替える


#=============================================================
# 【STEP 10】 グラフを保存しよう（ggsave）
#=============================================================

# まずグラフを変数に入れる
final_plot <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_tableau() +
  labs(
    title = "アヤメのがく片と花びらの関係",
    x = "がく片の長さ (cm)",
    y = "花びらの長さ (cm)",
    color = "品種"
  ) +
  theme_minimal()

# 画面に表示
final_plot

# PNG形式で保存
ggsave("ggplot/my_iris_plot.png", plot = final_plot, width = 8, height = 6, dpi = 300)

# PDF形式で保存（レポートや論文に便利）
ggsave("ggplot/my_iris_plot.pdf", plot = final_plot, width = 8, height = 6)


#=============================================================
# 【総合課題】 学んだことを組み合わせてグラフを作ろう
#=============================================================

# 総合課題1: 以下の条件を満たす散布図を作ろう
#   - x軸: Petal.Length, y軸: Petal.Width
#   - 品種ごとに色分け
#   - geom_smooth(method = "lm") でトレンドラインを追加
#   - labs() でタイトル「花びらの長さと幅の関係」をつける
#   - 好きなテーマを適用する


# 総合課題2: 以下の条件を満たす箱ひげ図を作ろう
#   - 品種別の Petal.Length の比較
#   - geom_jitter() でデータ点を重ねる
#   - labs() でタイトルと軸ラベルを日本語にする
#   - scale_fill_manual() で好きな3色に変える
#   - ggsave() でPNG形式で保存する


# 総合課題3: 以下の条件を満たすグラフを自由に作ろう
#   - facet_wrap() で品種ごとにパネルを分ける
#   - 1つのパネルの中に geom を2つ以上重ねる
#   - labs() でタイトル・軸ラベルをつける


#=============================================================
# 【おまけ】 ggplot2 の geom 早見表
#=============================================================
# geom_point()     : 散布図（2つの連続変数の関係）
# geom_smooth()    : トレンドライン（散布図と組み合わせる）
# geom_line()      : 折れ線グラフ（時系列データなど）
# geom_histogram() : ヒストグラム（1つの連続変数の分布）
# geom_density()   : 密度プロット（なめらかな分布）
# geom_bar()       : 棒グラフ（カテゴリの個数を自動カウント）
# geom_col()       : 棒グラフ（集計済みの値を使う）
# geom_boxplot()   : 箱ひげ図（分布の比較）
# geom_violin()    : バイオリンプロット（分布の形が見える）
# geom_jitter()    : ジッター（ばらけた点、箱ひげ図に重ねる）
# geom_tile()      : タイル図（ヒートマップに使う）
# geom_text()      : テキスト（グラフに文字を描く）
# geom_label()     : ラベル（背景付きの文字を描く）
# geom_errorbar()  : エラーバー（平均値の不確かさを示す）
# geom_hline()     : 水平線
# geom_vline()     : 垂直線
