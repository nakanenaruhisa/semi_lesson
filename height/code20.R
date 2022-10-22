#Rを使う下準備
#全部の変数を消す
rm(list=ls())
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#テーマのセット
theme_set(theme_grey(base_family = "HiraginoSans-W3"))
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))



#datasetを読み込む
ten_kukan20 <- read_csv("height_weight/ten_kukan20.csv")

#変数を箱に入れるよ
height <- (ten_kukan20$height)
weight <- (ten_kukan20$weight)
sex <- (ten_kukan20$sex)

#性別をカテゴリ変数に変換
ten_kukan20 <- ten_kukan20 %>% 
  mutate(sex_c = factor(sex,levels = 1:2,labels = c("女性","男性")))

#性別がカテゴリ変数に変換できていることを確認
ten_kukan20 %>% with(table(sex_c))
sex_c <- (ten_kukan20$sex_c)

#身長の平均値を出してみて
mean(height)

#体重の平均値を出してみて
mean(weight)

#男性の身長の平均値を出してみて
maleheightmean <- subset(height,sex_c == "男性")
maleheightmean <- mean(maleheightmean) 
print(maleheightmean)

#女性の身長の平均値を出してみて
femaleheightmean <- subset(height,sex_c == "女性")
femaleheightmean <- mean(femaleheightmean)
print(femaleheightmean)

#身長の標準偏差出してみて
sd(height)
#体重の標準偏差出してみて
sd(weight)

#標準誤差を出すためにパッケージを追加する。
install.packages("plotrix")
library(plotrix)

#身長の標準誤差を出す
std.error(height)

#体重の標準誤差を出す
std.error(weight)

#男性の身長の標準偏差出してみて
maleheightsd <- subset(height,sex_c == "男性")
maleheightsd <- sd(maleheightsd)
print(maleheightsd)

#女性の身長の標準偏差出してみて
femaleheightsd <- subset(height,sex_c == "女性") 
femaleheightsd <- sd(femaleheightsd)
print(femaleheightsd)

#データセット全体のテーブルを自動でつくる
ten_kukan20 %>% 
  tbl_summary()

#記述統計量にラベルを付けてテーブルで出力する
ten_kukan20 %>% 
  select(height,weight,sex_c) %>% 
  tbl_summary(label = list(sex_c ~ "性別",
                           height ~ "身長",
                           weight ~ "体重"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ±{sd}"), 
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "身長と体重テーブル") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#せっかくだから男女別に書いてみる
ten_kukan20 %>% 
  select(height,weight,sex_c) %>% 
  tbl_summary(label = list(sex_c ~ "性別",
                           height ~ "身長",
                           weight ~ "体重"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ±{sd}"),
              by = sex_c,
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "男女別身長体重") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#せっかくだから男女別に書いてみる
ten_kukan20 %>% 
  select(height,weight,sex_c) %>% 
  tbl_summary(label = list(sex_c ~ "性別",
                           height ~ "身長",
                           weight ~ "体重"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ±{sd}"),
              by = sex_c,
              digits = all_continuous() ~ 1) %>%
  add_n() %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "男女別身長体重") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#身長と体重のプロット書いてみて
ggplot(data = ten_kukan20) +     # tenkukan20データでキャンバス準備
  aes(x = height, y = weight)+ # height,weight列をx,y軸にmapping
  geom_point() +                  # 散布図を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#身長と体重の性別ごとのプロット書いてみて
ggplot(data = ten_kukan20) +     # tenkukan20データでキャンバス準備
  aes(x = height, y = weight, colour = sex_c)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_point() +                  # 散布図を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#男性と女性の身長の箱ひげ図を書いて比較してみて
ggplot(data = ten_kukan20) +     # tenkukan20データでキャンバス準備
  aes(x = sex_c, y = height, fill = sex_c)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_boxplot() +                  # はこひげ図を描く
  xlab("sex") + ylab("height") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#男性と女性の身長の平均の比較をseエラーバーをつけて書く（ggplot）
ggplot(data = ten_kukan20)+
  aes(x = sex_c, y = height, color = sex_c)+
  stat_summary(aes(x = sex_c),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("sex") + ylab("height") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
