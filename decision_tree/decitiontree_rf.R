#全部の変数を消す
rm(list=ls())

library(tidyverse)
library(xgboost)
library(caret)
library(Matrix)
library(kernlab)
library(randomForest)
library(e1071)
library(nnet)
library(ggplot2)
library(class)

#data
logsticreg_sample <- read_excel("logisticreg/logsticreg_sample.xlsx")
data <- logsticreg_sample

data <- data %>%
  mutate(regidencial_status = as.factor(regidencial_status))%>%
  select(regidencial_status,age,nintei_grade,family_status,public_assistance,public_pension)

#️ランダムフォレスト

install.packages("randomForest") # 必要に応じてインストール
library(randomForest)

data.rf <- randomForest(regidencial_status ~ ., data = data) # ランダムフォレストでモデル作成

data.rf

#重要度

importance(data.rf)

#重要度のプロット
varImpPlot(data.rf)

# 訓練データとテストデータに分割
set.seed(100)
df.rows = nrow(data) # 150
train.rate = 0.7 # 訓練データの比率
train.index <- sample(df.rows, df.rows * train.rate)
df.train = data[train.index,] # 訓練データ
df.test = data[-train.index,] # テストデータ
cat("train=", nrow(df.train), " test=", nrow(df.test))

model.rf <- randomForest(regidencial_status ~ ., data = df.train) # ランダムフォレスト、データは訓練用を指定

# テストデータの予測
prediction = predict(model.rf, df.test)

(result <- table(prediction, df.test$regidencial_status)) # ()で括って内容表示

# 精度の計算（行列の対角合計 / 行列の合計）
(accuracy_prediction = sum(diag(result)) / sum(result))

# ランダムフォレストのエラーを可視化する
plot(model.rf)

# モデル予測結果を図示
#ggplot2で可視化

ggplot(df.test, aes(x = nintei_grade, y = age, shape = factor(prediction), colour = factor(prediction))) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(1,2)) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_continuous(limits = c(0,7)) +
  scale_y_continuous(limits = c(65,100)) +
  labs(title = "地域在宅予測 (random forest)",
       x = "nintei_grade",
       y = "age",
       shape = "Prediction Level",
       colour = "Prediction Level") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text = element_text(size = 18),
        legend.position = "top", 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))

#新しい利用者のデータを入力する
new_patient <- data.frame(
  age = 72,
  nintei_grade = 4,
  family_status = 0,
  public_assistance = 1,
  public_pension = 1)

# 新しい患者データから退院日数を予測する
predicted_regidencial_status <- predict(model.rf, new_patient, type = "response")

# 予測結果と正解の確率を表示
print(paste("Predicted regidencial_statuss: ", predicted_regidencial_status))
probabilities <- predict(model.rf, new_patient, type = "prob")
print(probabilities)




