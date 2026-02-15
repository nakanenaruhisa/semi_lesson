#=============================================================
# Lesson 4: 折れ線グラフ ─ 時間の流れでデータを見よう
#=============================================================
# 折れ線グラフは「時間の経過にともなう変化」を見るのに最適なグラフ。
# 売上の推移、気温の変化、人口の増減など、「いつ・どう変わったか」を伝えられる。
#
# 使う geom:
#   geom_line()  ─ 線を引く
#   geom_point() ─ 線の上にデータ点を打つ（組み合わせることが多い）
#
# Lesson1〜3 の内容（R基本、ggplotの仕組み）は理解できている前提で進めるよ！
#=============================================================


#=============================================================
# 【準備】
#=============================================================

rm(list = ls())
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
if (grepl("^Lesson[0-9]", basename(getwd())) || basename(getwd()) %in% c("folder_format", "applied")) setwd("..")

library(tidyverse)
library(ggthemes)

# 日本語テーマの設定
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 12
))


#=============================================================
# 【STEP 1】 まずデータを作ろう ─ data.frame の復習
#=============================================================
# 折れ線グラフ用のデータは「横軸が時間、縦軸が数値」のセット。
# data.frame() で自分で作ってみよう。

# ある喫茶店の月別売上データ（架空）
cafe <- data.frame(
  month  = 1:12,
  sales  = c(80, 75, 90, 110, 130, 160, 180, 175, 150, 120, 95, 100)
  # 単位：万円
)

# 中身を確認
cafe
str(cafe)


#=============================================================
# 【STEP 2】 一番シンプルな折れ線グラフ
#=============================================================

# geom_line() で線を引く
ggplot(data = cafe, aes(x = month, y = sales)) +
  geom_line()

# これだけでも折れ線グラフになる！
# でも、データの点がどこにあるかわかりにくい…


#=============================================================
# 【STEP 3】 geom_point() を重ねてデータ点を見せよう
#=============================================================

# geom_line() と geom_point() を + で重ねる
ggplot(cafe, aes(x = month, y = sales)) +
  geom_line(color = "steelblue", linewidth = 1) +   # 線
  geom_point(color = "steelblue", size = 3)          # 点

# linewidth = 線の太さ、size = 点の大きさ
# 同じ色にそろえると統一感が出る


#=============================================================
# 【STEP 4】 labs() でグラフを整えよう
#=============================================================

ggplot(cafe, aes(x = month, y = sales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "喫茶店の月別売上",
    x = "月",
    y = "売上（万円）"
  ) +
  scale_x_continuous(breaks = 1:12)  # x軸の目盛りを1〜12に固定
# → breaks で「ここに目盛りを打って」と指定できる


#=============================================================
# 【課題1】 シンプルな折れ線グラフを描いてみよう
#=============================================================

# 課題1-1: 下のデータで折れ線グラフを描こう（geom_line + geom_point）
# ある学生の月別読書冊数
reading <- data.frame(
  month = 1:12,
  books = c(2, 3, 1, 4, 5, 3, 6, 7, 4, 3, 2, 5)
)


# 課題1-2: 上のグラフに labs() でタイトル「月別読書冊数」、
#          x軸「月」、y軸「冊数」をつけよう


# 課題1-3: 線の色を "coral"、点の色を "coral" に変えてみよう



#=============================================================
# 【STEP 5】 2本の線を重ねよう ─ 複数系列の折れ線グラフ
#=============================================================
# 「売上とコストを同じグラフで比較したい」というケースは多い。
# やり方は2通りある。

# --- 方法1: geom_line() を2回書く（シンプルだけど長い） ---

# まずデータを作る
company <- data.frame(
  year  = 2018:2025,
  sales = c(120, 135, 150, 140, 160, 180, 200, 220),  # 売上（百万円）
  costs = c(80, 85, 90, 95, 100, 105, 110, 115)        # コスト（百万円）
)

ggplot(company, aes(x = year)) +
  geom_line(aes(y = sales, color = "売上"), linewidth = 1) +
  geom_point(aes(y = sales, color = "売上"), size = 3) +
  geom_line(aes(y = costs, color = "コスト"), linewidth = 1) +
  geom_point(aes(y = costs, color = "コスト"), size = 3) +
  scale_color_manual(values = c("売上" = "#2196F3", "コスト" = "#F44336")) +
  labs(title = "売上とコストの推移", x = "年", y = "金額（百万円）", color = "指標") +
  scale_x_continuous(breaks = 2018:2025)

# ★ ポイント: aes(y = ..., color = "ラベル名") と書くと
#   自動的に凡例（色の説明）が作られる！


# --- 方法2: データを「縦長」に変換してから描く（おすすめ） ---

# pivot_longer() でデータの形を変える
company_long <- company %>%
  pivot_longer(
    cols = c(sales, costs),     # 縦長にしたい列
    names_to = "category",      # 列名が入る新しい列の名前
    values_to = "amount"        # 値が入る新しい列の名前
  )

# 変換前と変換後を比較してみよう
company       # 横長: year, sales, costs
company_long  # 縦長: year, category, amount

# 縦長データなら1行でスッキリ書ける
ggplot(company_long, aes(x = year, y = amount, color = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("sales" = "#2196F3", "costs" = "#F44336"),
    labels = c("sales" = "売上", "costs" = "コスト")
  ) +
  labs(title = "売上とコストの推移", x = "年", y = "金額（百万円）", color = "指標") +
  scale_x_continuous(breaks = 2018:2025)


#=============================================================
# 【課題2】 2本の線を描いてみよう
#=============================================================

# 下のデータは、ある2科目のテスト点数の推移
scores <- data.frame(
  exam   = 1:5,          # 第1回〜第5回
  math   = c(60, 65, 70, 75, 85),
  english = c(70, 68, 72, 80, 78)
)

# 課題2-1: 方法1（geom_line を2回書く方法）で
#          数学（青）と英語（赤）の折れ線グラフを描こう


# 課題2-2: pivot_longer() で scores を縦長に変換しよう
# ヒント: cols = c(math, english), names_to = "subject", values_to = "score"


# 課題2-3: 縦長データを使って同じグラフを描こう（方法2）


# 課題2-4: labs() でタイトル「テスト点数の推移」、x軸「試験回」、y軸「点数」をつけよう



#=============================================================
# 【STEP 6】 線のスタイルを変えよう
#=============================================================

# linetype で線の種類を変えられる
# "solid"(実線), "dashed"(破線), "dotted"(点線),
# "dotdash"(点破線), "longdash"(長破線), "twodash"(2重破線)

ggplot(company_long, aes(x = year, y = amount, color = category, linetype = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "線の種類を変えた例", x = "年", y = "金額（百万円）")

# 白黒印刷でも区別できるようにするとき、linetype は便利！

# shape でデータ点の形も変えられる
ggplot(company_long, aes(x = year, y = amount, color = category, shape = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "点の形を変えた例", x = "年", y = "金額（百万円）")


#=============================================================
# 【STEP 7】 基準線を引こう ─ geom_hline / geom_vline
#=============================================================

# geom_hline(): 水平の線を引く（目標値や基準値を示すとき便利）
# geom_vline(): 垂直の線を引く（特定の時点を示すとき便利）

ggplot(cafe, aes(x = month, y = sales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 120, linetype = "dashed", color = "red") +  # 目標ライン
  geom_vline(xintercept = 7, linetype = "dotted", color = "gray40") + # 7月を強調
  annotate("text", x = 7.5, y = 125, label = "目標: 120万円", color = "red") +
  labs(title = "喫茶店の月別売上（目標ライン付き）", x = "月", y = "売上（万円）") +
  scale_x_continuous(breaks = 1:12)

# annotate("text", ...) でグラフ上に好きな文字を置ける


#=============================================================
# 【STEP 8】 成長率（変化率）を計算して描こう
#=============================================================
# 「前年比で何%増えた？」を計算するには lag() を使う。
# lag() = 1つ前の値を取得する関数

company <- company %>%
  mutate(
    sales_growth = (sales - lag(sales)) / lag(sales) * 100,
    costs_growth = (costs - lag(costs)) / lag(costs) * 100
  )

# 計算結果を確認（最初の年はNAになる ← 「1つ前」がないから）
company

# 成長率の折れ線グラフ（NA の行は除く）
company_growth <- company %>%
  filter(!is.na(sales_growth)) %>%  # NAの行を除く
  pivot_longer(
    cols = c(sales_growth, costs_growth),
    names_to = "category",
    values_to = "growth"
  )

ggplot(company_growth, aes(x = year, y = growth, color = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # 0%ライン
  scale_color_manual(
    values = c("sales_growth" = "#2196F3", "costs_growth" = "#F44336"),
    labels = c("sales_growth" = "売上成長率", "costs_growth" = "コスト成長率")
  ) +
  labs(title = "売上・コストの成長率推移", x = "年", y = "成長率（%）", color = "指標") +
  scale_x_continuous(breaks = 2019:2025)


#=============================================================
# 【課題3】 成長率を計算して描こう
#=============================================================

# 課題3-1: 下のデータに「前月比成長率」の列を追加しよう
# ヒント: mutate(growth = (visitors - lag(visitors)) / lag(visitors) * 100)
shop <- data.frame(
  month    = 1:12,
  visitors = c(500, 480, 520, 600, 700, 850, 900, 870, 750, 650, 550, 580)
)


# 課題3-2: 成長率の折れ線グラフを描こう（NAの月は除く）
# ヒント: filter(!is.na(growth)) してから ggplot


# 課題3-3: geom_hline() で 0% の基準線を追加しよう



#=============================================================
# 【STEP 9】 面グラフ ─ 折れ線の下を塗りつぶす
#=============================================================
# geom_area() を使うと、折れ線の下の面積を塗りつぶせる。
# 「量の大きさ」を視覚的に強調したいときに使う。

ggplot(cafe, aes(x = month, y = sales)) +
  geom_area(fill = "steelblue", alpha = 0.3) +  # 塗りつぶし
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "喫茶店の月別売上（面グラフ）", x = "月", y = "売上（万円）") +
  scale_x_continuous(breaks = 1:12)


#=============================================================
# 【STEP 10】 テーマと仕上げ
#=============================================================
# 最終的なグラフは見やすくカスタマイズしよう

final_plot <- ggplot(company_long, aes(x = year, y = amount, color = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("sales" = "#2196F3", "costs" = "#F44336"),
    labels = c("sales" = "売上", "costs" = "コスト")
  ) +
  labs(
    title = "売上とコストの年次推移",
    subtitle = "2018年〜2025年",
    x = "年",
    y = "金額（百万円）",
    color = "指標",
    caption = "データ：架空データ"
  ) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(breaks = seq(0, 250, by = 50)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",                     # 凡例を下に
    plot.title = element_text(hjust = 0.5, size = 14), # タイトル中央揃え
    plot.subtitle = element_text(hjust = 0.5)       # サブタイトル中央揃え
  )

final_plot

# 保存（保存先フォルダがなければ作成）
dir.create("Lesson05_line_plot", showWarnings = FALSE)
ggsave("Lesson05_line_plot/line_plot_result.png",
       plot = final_plot, width = 8, height = 5, dpi = 300)


#=============================================================
# 【総合課題】
#=============================================================

# 総合課題1: 下のデータで折れ線グラフを作ろう
#   - geom_line() + geom_point() を使う
#   - labs() でタイトル・軸ラベルをつける
#   - scale_x_continuous(breaks = ...) で年の目盛りを全部表示する

population <- data.frame(
  year = 2015:2025,
  pop  = c(1270, 1268, 1265, 1262, 1260, 1257, 1253, 1250, 1246, 1242, 1238)
  # 日本の人口（百万人・架空データ）
)


# 総合課題2: 下のデータで2本の線（東京と大阪）の折れ線グラフを作ろう
#   - pivot_longer() で縦長に変換してから描く
#   - scale_color_manual() で好きな色を設定する
#   - geom_hline() で「平均気温20度」のラインを追加する

temperature <- data.frame(
  month = 1:12,
  tokyo = c(5.4, 6.1, 9.4, 14.3, 19.0, 22.7, 26.2, 27.5, 24.2, 18.4, 12.7, 7.7),
  osaka = c(6.0, 6.4, 9.8, 15.2, 19.8, 23.6, 27.4, 28.6, 25.0, 19.2, 13.5, 8.2)
)


# 総合課題3: 総合課題2のグラフに以下を追加しよう
#   - theme_minimal() を適用
#   - legend.position = "bottom" で凡例を下に
#   - ggsave() でPNG形式で保存


#=============================================================
# 【おまけ】 折れ線グラフ関連の geom 早見表
#=============================================================
# geom_line()    : 折れ線（点と点を直線でつなぐ）
# geom_point()   : 点（折れ線と組み合わせることが多い）
# geom_area()    : 面グラフ（折れ線の下を塗りつぶす）
# geom_step()    : 階段グラフ（段階的に変化するデータ向け）
# geom_smooth()  : トレンドライン（傾向を滑らかに表現）
# geom_hline()   : 水平基準線（目標値・平均値に使う）
# geom_vline()   : 垂直基準線（特定時点を強調する）
# annotate()     : グラフ上にテキストや図形を追加
