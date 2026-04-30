# 下準備
# 全ての変数を消す
rm(list = ls())

# 作業ディレクトリの確認
getwd()
# 作業フォルダを semi_lesson に合わせる（Lesson 等のサブフォルダから実行した場合のみ1つ上へ）
setwd(
  "/Users/naruhisa/Library/CloudStorage/OneDrive-学校法人立命館/lecture/semi/R/semi_lesson"
)

# 必要なライブラリの読み込み
library(tidyverse)
library(ggplot2)

# T検定のサンプルコード

# サンプルデータの作成（条件を満たすよう生成）

set.seed(123) # 再現性保持

n_single <- 100 # ひとり親世帯のサンプル数
n_double <- 100 # 二人親世帯のサンプル数

# ひとり親世帯収入（平均値303, 中央値271, 標準偏差45）
# 対数正規分布で中央値と平均を区別して制御
mu_single <- log(271) # 中央値=exp(mu)
sigma_single <- sqrt(log((45^2 + 303^2) / 271^2)) # 分散調整
income_single <- rlnorm(n_single, meanlog = mu_single, sdlog = sigma_single)
# 平均を調整
income_single <- income_single * (303 / mean(income_single))

# 二人親世帯収入（平均値812, 中央値731, 標準偏差32）
mu_double <- log(731) # 中央値=exp(mu)
sigma_double <- sqrt(log((32^2 + 812^2) / 731^2)) # 分散調整
income_double <- rlnorm(n_double, meanlog = mu_double, sdlog = sigma_double)
# 平均を調整
income_double <- income_double * (812 / mean(income_double))

# データフレームの作成
family_type <- factor(
  c(rep(1, n_single), rep(2, n_double)),
  levels = c(1, 2),
  labels = c("ひとり親世帯", "二人親世帯")
)

df <- tibble(
  family_type = family_type,
  income = c(income_single, income_double)
)


# データの可視化（ggplot2で統一）
ggplot(df, aes(x = income, fill = family_type, color = family_type)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20, alpha = 0.35, position = "identity") +
  geom_density(linewidth = 1) +
  facet_wrap(~family_type, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("ひとり親世帯" = "skyblue3", "二人親世帯" = "tomato3")) +
  scale_color_manual(values = c("ひとり親世帯" = "blue", "二人親世帯" = "red")) +
  labs(title = "世帯タイプ別の収入分布", x = "収入(万円)", y = "密度") +
  theme_minimal(base_family = if (interactive()) "HiraginoSans-W3" else "sans") +
  theme(legend.position = "none")

#　データの可視化、箱ひげ図をtidyplotsで書いてみて
# 追加のライブラリの読み込み
library(ggpubr) # for stat_compare_means

# ggplot2の日本語フォント設定（macOS用）。非対話時は sans にフォールバック
theme_set(theme_gray(
  base_family = if (interactive()) "HiraginoSans-W3" else "sans"
))

# 必要なパッケージの読み込み
library(ggsignif)

# ggplot2を使用した箱ひげ図の作成（t検定の結果をggsignifで表示）
ggplot(df, aes(x = family_type, y = income, fill = family_type)) +
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("ひとり親世帯", "二人親世帯")),
    map_signif_level = TRUE,
    test = "t.test"
  ) +
  theme_minimal(
    base_family = if (interactive()) "HiraginoSans-W3" else "sans"
  ) +
  labs(x = "世帯タイプ", y = "収入 (万円)", fill = "世帯タイプ")

# tidyplotsを使ったシンプルな箱ひげ図＋t検定p値の表示例
df %>%
  tidyplots::tidyplot(x = family_type, y = income, color = family_type) %>%
  tidyplots::add_boxplot() %>%
  tidyplots::add_test_pvalue() %>%
  tidyplots::adjust_size(width = 100, height = 100)
