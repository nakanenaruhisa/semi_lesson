# 必要なパッケージをロード

library(readr)
library(tidyverse)
library(ggplot2)
library(rstan)


#全部の変数を消す
rm(list=ls())

# データを読み込む
data<- read_csv("height/ten_kukan100.csv")

# データの確認
head(data)

# データの準備
female_height <- data$height[data$sex == 1]
male_height <- data$height[data$sex == 2]

# データの確認
print(paste("女性のデータ数:", length(female_height)))
print(paste("男性のデータ数:", length(male_height)))
print("女性の身長:")
print(female_height)
print("男性の身長:")
print(male_height)

# データリストの作成
data_list <- list(
  N1 = length(female_height),
  N2 = length(male_height),
  y1 = as.vector(female_height),
  y2 = as.vector(male_height)
)

# データリストの確認
print(data_list)

# Stanモデルの定義
stan_model_code <- "
data {
  int<lower=0> N1;
  int<lower=0> N2;
  real y1[N1];
  real y2[N2];
}

parameters {
  real mu1;
  real mu2;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
}

model {
  mu1 ~ normal(0, 10);
  mu2 ~ normal(0, 10);
  sigma1 ~ cauchy(0, 5);
  sigma2 ~ cauchy(0, 5);
  y1 ~ normal(mu1, sigma1);
  y2 ~ normal(mu2, sigma2);
}
"

# モデルのコンパイル
stan_model <- stan_model(model_code = stan_model_code)

# サンプリングの実行
fit <- sampling(stan_model, data = data_list, iter = 2000, warmup = 500, chains = 4, seed = 123)


# サンプリング結果の表示
print(fit)

# サンプリング結果の要約を表示
summary(fit)

# 平均値の差の計算とプロット
if (class(fit) == "stanfit") {
  samples <- extract(fit)
  mu_diff <- samples$mu1 - samples$mu2
  
  # ヒストグラムのプロット
  qplot(mu_diff, geom = "histogram", binwidth = 0.1, main = "Difference in means (mu1 - mu2)", xlab = "Difference", ylab = "Frequency")
} else {
  print("サンプリングに失敗しました。'fit'オブジェクトが'stanfit'クラスではありません。")
}
