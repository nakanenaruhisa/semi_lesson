#packageの準備
install.packages("mice")
install.packages("missForest")
install.packages("VIM")
library(tidyverse)
library(gtsummary)
library(here)
library(mice)
library(missForest)
library(VIM)

#フォルダの固定
here::here()

#全部の変数を消す
rm(list=ls())

#データセットのインポート
data <- BostonHousing

#欠損データを作る
set.seed(123)
data.mis <- missForest::prodNA(data, noNA = 0.2)
summary(data.mis)

#欠損データを可視化する
library(VIM)
vim.aggr <- aggr(data.mis, col = c('white','red'), 
                 numbers = TRUE,
                 sortVars = FALSE,
                 prop = TRUE,
                 labels = names(data.mis),
                 cex.axis = .8,
                 gap = 3)

#中央値を代入する（よくない）方法
install.packages("imputeMissings")
library(imputeMissings)

data.comp.median <- impute(data.mis, method = "median/mode")
model.median <- lm(medv ~ ., data = data.comp.median)
summary(model.median)

#データフレームの結合
combined_df <- bind_rows(
  BostonHousing %>% select(age) %>% mutate(dataset = "BostonHousing"),
  data.mis %>% select(age) %>% mutate(dataset = "data.mis"),
  data.comp.median %>% select(age) %>% mutate(dataset = "data.comp.median")
)

#ggplotを使ったヒストグラムの作成（歪んだ分布を確認！）
combined_df %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dataset, scales = "free") +
  theme_minimal() +
  xlab("Age") +
  ylab("Density")

#多重代入法
library(mice)
imp <- mice(data.mis,m = 10,
            maxit = 50,
            method = "pmm",
            printFlag = FALSE,
            seed = 12345)

data.comp.mice <- complete(imp, "long")

#データフレームの結合
combined_df <- bind_rows(
  BostonHousing %>% select(age) %>% mutate(dataset = "BostonHousing"),
  data.mis %>% select(age) %>% mutate(dataset = "data.mis"),
  data.comp.mice %>% select(age) %>% mutate(dataset = "data.comp.mice")
)


#ggplotを使ったヒストグラムの作成
combined_df %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ dataset, scales = "free") +
  theme_minimal() +
  xlab("Age") +
  ylab("Density")

#パッケージを使った各変数の欠損値データと保管データの比較
mice::densityplot(imp)


# create a tbl_summary for each dataset
tbl1 <- tbl_summary(data.comp.mice)
tbl2 <- tbl_summary(BostonHousing)
tbl_merged <- tbl_merge(list(tbl1, tbl2))

tbl_merged

view(data.mis)
