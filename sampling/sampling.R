#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#ggforceパッケージを追加
install.packages("ggforce")
library(ggforce)


#全部の変数を消す
rm(list=ls())

#大数の法則
set.seed(19761130) # 乱数のseedを指定

X1 <- rnorm(5, 0, 100) # N(0, 100)から乱数を5個抽出する
print(X1) # 抽出された乱数を出力する

mean(X1)

X2 <- rnorm(100, 0, 100) # N(0, 100)から乱数を100個抽出する
print(X2) # 抽出された乱数を出力する

mean(X2)

X3 <- rnorm(10000, 0, 100) # N(0, 100)から乱数を1万個抽出する
mean(X3) # 抽出された乱数の平均値を出力する

X4 <- rnorm(20000929, 0, 100) # N(0, 100)から乱数を1万個抽出する
mean(X4) # 抽出された乱数の平均値を出力する

X_bar_vec <- rep(NA, 1000)     # 長さ1000の空ベクトルを作成
for (i in 1:1000) {            # iを1から1000へ増やしながら反復
  temp_vec <- rnorm(i, 0, 100) # N(0, 100)からi個の乱数を抽出し、temp_vecに格納
  # temp_vecの平均値をX_bar_vecのi番目要素として格納
  X_bar_vec[i] <- mean(temp_vec)
}

# 可視化
ggplot() +
  geom_line(aes(x = 1:1000, y = X_bar_vec,color = "green")) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "n", y = expression(hat(theta)))+
  scale_colour_tableau()+
  theme_test(base_size = 15) + #grayテーマで
  theme_test(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#モンテカルロ法
set.seed(19861009)
pi_df <- tibble(x = runif(800, -1, 1),
                y = runif(800, -1, 1))

pi_df

#1×1の正方形の中に入れる
pi_df %>%
  ggplot() +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1),
            fill = "white", color = "black") +
  geom_point(aes(x = x, y = y)) +
  coord_fixed(ratio = 1) +
  theme_minimal()

#1×1の正方形に円を書く

pi_df %>%
  ggplot() +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1),
            fill = "white", color = "black") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_point(aes(x = x, y = y)) +
  coord_fixed(ratio = 1) +
  theme_minimal()

#点に「円内」「円外」という変数をもたせる
pi_df <- pi_df %>%
  mutate(in_circle = if_else(x^2 + y^2 < 1^2, "円内", "円外"))

#変数「円内」「円外」で色分け
pi_df %>%
  ggplot() +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1),
            fill = "white", color = "black") +
  # 円の内側か外側かで色分け
  geom_point(aes(x = x, y = y, color = in_circle), size = 2) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  labs(x = "X", y = "Y", color = "") +
  coord_fixed(ratio = 1) +
  theme_void(base_size = 12)

#変数「円内」「円外」をカウント
pi_df %>%
  group_by(in_circle) %>%
  summarise(N = n())

#計算
623 / 800 * 4
