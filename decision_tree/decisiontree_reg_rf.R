#フォルダの固定
here::here()

#全部の変数を消す
rm(list=ls())

# パッケージを読み込む
install.packages(c("readxl", "dplyr", "caTools", "randomForest", "Metrics"))

# ライブラリを読み込む
library(tidyverse)
library(readxl)
library(dplyr)
library(caTools)
library(randomForest)
library(Metrics)

# データセットを読み込む
data <- multureg2_dataset

# 変数を定義する
y <- data$discharge_days
x <- data %>% select(-id, -discharge_days,-event)

# 学習用データとテスト用データを分割する（75%と25%）
set.seed(42)
split <- sample.split(y, SplitRatio = 0.75)  # use 'y' instead of 'X' in sample.split
x_train <- subset(x, split == TRUE)
y_train <- y[split == TRUE]
x_test <- subset(x, split == FALSE)
y_test <- y[split == FALSE]

# ランダムフォレストで学習データを作成する
model <- randomForest(x_train, y_train, importance=TRUE)

# テストデータを使って目的変数を予測する
y_pred <- predict(model, x_test)

# 学習データの目的変数とテストデータの目的変数のズレを比較する
mae <- mae(y_test, y_pred)
mse <- mse(y_test, y_pred)
rmse <- rmse(y_test, y_pred)
r2 <- cor(y_test, y_pred)^2

print(paste("MAE: ", mae))
print(paste("RMSE: ", rmse))
print(paste("R^2 Score: ", r2))

# ランダムフォレストのエラーを可視化する
plot(model)

# ランダムフォレストの重要変数を可視化する
importance <- importance(model)
importance_df <- data.frame(Feature = row.names(importance), Importance = importance[,1])

# Order the data frame by importance
importance_df <- importance_df[order(-importance_df$Importance),]

# Create the plot
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_light() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")

# 新しい患者のデータを入力する
new_patient <- data.frame(
  gender = 2,
  oral_care = 1,
  care_level = 1,
  cohabiting_family = 1,
  household_income = 700,
  housing_ownership = 1,
  municipality_scale = 2
)

# 新しい患者データから退院日数を予測する
predicted_discharge_days <- predict(model, new_patient)
print(paste("Predicted discharge days: ", predicted_discharge_days))

# ブートストラップの数を指定する
n_bootstrap <- 100

# Empty vector to store the predictions
bootstrap_preds <- numeric(n_bootstrap)

# For each bootstrap sample
for (i in 1:n_bootstrap) {
  # Sample indices with replacement
  bootstrap_indices <- sample(1:nrow(x_train), replace = TRUE)
  
  # Select the rows from X_train and y_train using the bootstrap indices
  bootstrap_sample <- x_train[bootstrap_indices,]
  bootstrap_target <- y_train[bootstrap_indices]
  
  # Train the model on the bootstrap sample
  bootstrap_model <- randomForest(bootstrap_sample, bootstrap_target)
  
  # Make a prediction for the new patient
  bootstrap_preds[i] <- predict(bootstrap_model, new_patient)
}

# Calculate the mean prediction and the 95% confidence interval
mean_pred <- mean(bootstrap_preds)
ci_lower_95 <- quantile(bootstrap_preds, 0.025)
ci_upper_95 <- quantile(bootstrap_preds, 0.975)
ci_lower_68 <- quantile(bootstrap_preds, 0.16)
ci_upper_68 <- quantile(bootstrap_preds, 0.84)

print(paste("この患者の退院期間の予測は", mean_pred))
print(paste("70%の確率で[", ci_lower_68, ", ", ci_upper_68, "]に収まる"))
print(paste("95%の確率で[", ci_lower_95, ", ", ci_upper_95, "]に収まる"))

