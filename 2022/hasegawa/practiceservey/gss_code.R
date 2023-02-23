#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)

#全部の変数を消す
rm(list=ls())

#practiceservey <- read_csv("OneDrive−個人用/R/semi_lesson/gss_sm/practiceservey.csv")

#複数回答を処理
data <- as.data.frame(practiceservey)
colnames(data) <- program
data

answers <- strsplit(data$program, ",")
answers_list <- unlist(answers)
answers_list <- unique(unlist(answers))
answers_list
ma_df <- NULL
for (i in 1:length(answers)) {
  for (j in 1:length(answers_list)) {
    tempanswer <-  answers_list[j] %in% answers[[i]]
    ma_df <- rbind(ma_df,tempanswer) 
  }
}
ma_dfdm <- ma_df*1
ma_dfdm
slrnum1 <- length(answers_list)
df_ma <- NULL
for (i in 1:slrnum1) {
  tempma <- ma_dfdm[seq(i,nrow(ma_dfdm),slrnum1), ]
  df_ma <- cbind(df_ma,tempma)
}
colnames(df_ma) <- answers_list
df_ma
data_new <- cbind(data,df_ma) 
data_new
rownames(data_new) <- NULL

write_csv(x = data_new2,file = "data_new2.csv")

#datasetの読み込み
data_new2 <- read_excel("OneDrive/R/semi_lesson/practiceservey/data_new2.xlsx")

#分野にラベルを貼る
data_new2 <- data_new2 %>%
  mutate(name_c = factor(name,levels = 1:4,labels = c("高齢","障害","児童","地域")))

#時間にラベルを貼る
data_new2 <- data_new2 %>%
  mutate(time_firstweek_c = factor(time_firstweek,levels = 1:4,labels = c("1時間未満","1~2時間","2~3時間","3時間以上")))%>%
  mutate(time_secondweek_c = factor(time_secondweek,levels = 1:4,labels = c("1時間未満","1~2時間","2~3時間","3時間以上")))%>%
  mutate(time_forthweek_c = factor(time_forthweek,levels = 1:4,labels = c("1時間未満","1~2時間","2~3時間","3時間以上")))

#実施したプログラムにラベルを貼る
data_new2 <- data_new2 %>%
  mutate(communication_c = factor(communication,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(confarence_c = factor(confarence,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(community_c = factor(community,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(meeting_c = factor(meeting,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(record_c = factor(record,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(training_c = factor(training,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(careplan_c = factor(careplan,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(orientation_c = factor(orientation,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(other_c = factor(other,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(super_c = factor(super,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(network_c = factor(network,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(volunteer_c = factor(volunteer,levels = 0:1,labels = c("未実施","実施")))%>%
  mutate(supportplan_c = factor(supportplan,levels = 0:1,labels = c("未実施","実施")))

view(data_new2)

#分野ごとにテーブルを書いてみる
data_new2 %>%
  select(name_c,time_firstweek_c,time_secondweek_c,time_forthweek_c) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ±{sd}"),
    by = name_c,
    digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#分野ごとにテーブルを書いてみる
data_new2 %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ±{sd}"),
    by = name_c,
    digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

library(ggsci)
library(scales)
#利用者とのコミュニケーション
ggplot(data_new2)+
  (aes(x = name_c, y = as.numeric(communication_c), fill = communication_c))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  xlab("分野") + ylab("利用者とのコミュニケーション") +
  scale_fill_nejm()

#カンファレンスの実施
ggplot(data_new2)+
  (aes(x = name_c, y = as.numeric(confarence_c), fill = confarence_c))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = percent)+
  xlab("分野") + ylab("カンファレンスの実施") +
  scale_fill_nejm()

#地域活動
#記録の実施
#ケアプランの作成
#研修への参加
#地域調査
#他機関とのネットワーク
#スーパーバイザーの実施
#ボランティア活動
#オリエンテーション
#会議への参加
#児童自立支援計画の作成

#記録に関するlongdata読み込み
notetime <- read_csv("practiceservey/notetime.csv")

#分野にラベルを貼る
notetime <- notetime %>%
  mutate(name_c = factor(name,levels = 1:4,labels = c("高齢","障害","児童","地域")))

#実習期間にラベルを貼る
notetime <- notetime %>%
  mutate(week_c = factor(week,levels = 1:3,labels = c("1週目","2~3週目","4週目")))

notetime$ID <- as.factor(notetime$ID)
notetime$time <- as.numeric(notetime$time)
notetime$week_c <- as.factor(notetime$week_c)
notetime$meantime <- mean(notetime$time)

#実習記録に関する時間の折れ線グラフ
ggplot(data = notetime)+
  (aes(x = week_c, y = time, colour = name_c, group = ID))+
  geom_point()+
  geom_line()+
  #geom_jitter(height=0.1, width =0.1)+
  scale_colour_tableau()+
  xlab("week_c") + ylab("take_time") +
  labs(color = "")+ #凡例のタイトルを指定
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
