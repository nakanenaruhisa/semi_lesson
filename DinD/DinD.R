# 必要なパッケージをロード - データ操作と可視化に必要なライブラリをインポート
library(tidyverse) # データ操作のための総合パッケージ
library(ggplot2)   # グラフ作成用パッケージ
library(plm)       # パネルデータ分析用パッケージ
library(lfe)       # 固定効果モデル推定用パッケージ
library(ggthemes)  # ggplot2の追加テーマパッケージ

# 作業環境をクリーンにするため、既存の変数をすべて削除
rm(list=ls())

# CSVファイルからデータを読み込む
DinD <- read_csv("semi_lesson/DinD/DinD.csv")

# データの内容を確認
view(DinD)

# 来院頻度ごとの満足度の平均値を計算して比較
aggregate(satis ~ frequency, data = DinD, FUN = mean)

# データの内容を再確認
view(DinD)

# 来院頻度と満足度の関係を箱ひげ図で可視化
ggplot(DinD, aes(x = factor(frequency), y = satis, fill = factor(frequency))) +
  geom_boxplot() +
  scale_fill_tableau() +
  theme_gray(base_size = 15) + # グラフの基本フォントサイズを設定
  theme_gray(base_family = "HiraKakuPro-W3") + # 日本語フォントを設定
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + # アスペクト比を1:1に固定
  theme(plot.background = element_rect(fill = "white")) # グラフエリアの背景色を指定

# 差の差分析の初期モデル
# 病院IDと介入の効果を考慮したモデルを推定
did_model <- felm(satis ~ hospital + procedure,data = DinD)

# モデルの推定結果を表示
summary(did_model)

# データ型の変換 - 適切な分析のための前処理
DinD$satis <- as.numeric(as.character(DinD$satis))      # 満足度を数値型に変換
DinD$procedure <- factor(DinD$procedure)                 # 介入をファクター型に変換
DinD$month <- as.numeric(DinD$month)                    # 月を数値型に変換

# 本格的な差の差回帰分析
# 介入と時期の交互作用、および病院の固定効果を含むモデルを推定
did_model <- lm(satis ~ procedure * as.factor(month) + as.factor(hospital), data = DinD)

# 時期と介入の組み合わせごとの記述統計量を計算
df <- DinD %>%
  group_by(month,procedure) %>%
  summarise(Mean    = mean(satis),    # 平均値
            SD      = sd(satis),      # 標準偏差
            N       = n(),            # サンプルサイズ
            .groups = "drop") %>%
  arrange(Mean)

# 集計結果の確認
view(df)

# 差の差分析の結果を視覚化
ggplot(data = df) +
  aes(x = month, y = Mean, colour = procedure, shape = procedure) +
  geom_point(size = 3) +                                  # 各時点の平均値をプロット
  geom_line(size = 0.6) +                                # 時系列の推移を線で表示
  geom_vline(xintercept = 4 ,colour="black")+           # 介入時点を示す垂直線
  xlab("month") + ylab("satis") +                        # 軸ラベルの設定
  theme_gray(base_size = 15) +                           # グラフの基本フォントサイズを設定
  theme_gray(base_family = "HiraKakuPro-W3")            # 日本語フォントを設定
