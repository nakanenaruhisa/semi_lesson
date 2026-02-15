#生存分析のパッケージインストール
install.packages("survival")
install.packages("devtools")
install.packages("ggsignif")
install.packages("car")


#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(haven)
library(survminer)
library(survival)
library(car)
#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#theme_set(theme_gray(base_size = 10, base_family = "Meiryo"))        # Windows用
#theme_set(theme_gray(base_size = 10, base_family = "IPAGothic"))     # Ubuntu用
#showtext::showtext_auto()                                            # Cloud用
#theme_set(theme_gray(base_size = 10, base_family = "noto"))          # Cloud用

#パッケージの中のデータセットインポート
COX <- read_dta("survival/COX.dta")



#drugをカテゴリ変数に変換
COX_c <- COX %>% 
  mutate(drug_c = factor(drug,
                        levels = 0:1,
                        labels = c("プラセボ","投与")))

#studytime、イベント、介入要因（drug）を箱に入れる
studytime <- COX_c$studytime
deied <- COX_c$died

#データセット全体のテーブルを自動でつくる
COX_c %>% 
  tbl_summary()

#記述統計量にラベルを付けてテーブルで出力する
COX_c %>% 
  select(studytime,drug_c,age) %>% 
  tbl_summary(label = list(studytime ~ "観察期間",
                           drug_c ~ "投与タイプ",
                           age  ~ "年齢"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#KM曲線を書く
kmfit <- survfit( Surv(studytime, deied) ~ drug_c, data = COX_c )

#表を出す
print(kmfit)

#単純描画
ggsurvplot(kmfit)

#信頼区間をつける
ggsurvplot(kmfit,conf.int=TRUE)

#日本語の軸と凡例を追加
ggsurvplot(kmfit,conf.int=TRUE,
           title= "カプランマイヤー曲線",
           xlab="観察月数",
           ylab="生存割合",
           legend=c(0.8,0.8), #"top"/"bottom"/"left"/"right"/"none"またはc(x,y)でx,y=0-1を指定
           legend.title="投薬",
           legend.labs=c("投薬なし","投薬あり"))

#文字化けした場合はPreference → Graphic と進み、 BackendをAGGに変更する。
#raggパッケージをインストールするか聞かれるので、インストールする

