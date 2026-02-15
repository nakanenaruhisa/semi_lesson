# T検定のサンプルコード

# サンプルデータの作成
set.seed(123) # 再現性のため
group1 <- rnorm(30, mean = 10, sd = 2) # 平均10、標準偏差2の正規分布から30個のデータ
group2 <- rnorm(30, mean = 12, sd = 2) # 平均12、標準偏差2の正規分布から30個のデータ

# データの可視化
par(mfrow=c(1,2))
hist(group1, main="グループ1のヒストグラム", xlab="値", ylab="頻度")
hist(group2, main="グループ2のヒストグラム", xlab="値", ylab="頻度")


# 基本統計量の計算
cat("\nグループ1の平均値:", mean(group1))
cat("\nグループ1の標準偏差:", sd(group1))
cat("\nグループ2の平均値:", mean(group2))
cat("\nグループ2の標準偏差:", sd(group2))

# 対応のない2群のt検定の実行
t_test_result <- t.test(group1, group2)

# 結果の表示
cat("\n\nt検定の結果:")
cat("\nt値:", t_test_result$statistic)
cat("\n自由度:", t_test_result$parameter)
cat("\np値:", t_test_result$p.value)
cat("\n95%信頼区間:", t_test_result$conf.int)

# 結果の解釈
if(t_test_result$p.value < 0.05) {
  cat("\n\n結果: 2つのグループ間に統計的に有意な差があります（p < 0.05）")
} else {
  cat("\n\n結果: 2つのグループ間に統計的に有意な差はありません（p >= 0.05）")
}
