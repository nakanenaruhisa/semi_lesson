# 必要なパッケージをインストールおよび読み込み
install.packages("ggplot2")
install.packages("caret")
install.packages("rpart")

library(ggplot2)
library(caret)
library(rpart)

# サンプルデータの作成
set.seed(123)
x <- matrix(rnorm(200), ncol = 2)
t <- sample(0:1, 100, replace = TRUE)

# データフレームを作成
data <- data.frame(x1 = x[,1], x2 = x[,2], class = factor(t))

# 決定木モデルの作成
tree_2 <- rpart(class ~ x1 + x2, data = data, method = "class")

# 決定境界のプロット関数
plot_decision_boundary <- function(model, x, t) {

  # データフレームを作成
data <- data.frame(x1 = x[,1], x2 = x[,2], class = factor(t))
  
  # 決定境界の描画範囲の設定
  x1_min <- min(x[,1]) - 1
  x1_max <- max(x[,1]) + 1
  x2_min <- min(x[,2]) - 1
  x2_max <- max(x[,2]) + 1
  
# グリッドの作成
grid <- expand.grid(x1 = seq(x1_min, x1_max, by = 0.01), 
                      x2 = seq(x2_min, x2_max, by = 0.01))
  
  # 予測結果を計算
grid$class <- predict(model, newdata = grid, type = "class")
  
# プロットの作成
  ggplot(data = data, aes(x = x1, y = x2)) +
    geom_point(aes(color = class), size = 2) +
    scale_color_manual(values = c("mediumblue", "orangered")) +
    geom_tile(data = grid, aes(fill = class), alpha = 0.2) +
    scale_fill_manual(values = c("mediumblue", "orangered")) +
    theme_minimal() +
    labs(x = 'x1', y = 'x2')
}

# プロットの描画
plot_decision_boundary(tree_2, x, t)
