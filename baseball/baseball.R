Rを使う下準備
#packageの準備
install.packages("modelsummary")

renv::init()

#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(modelsummary)

#フォルダの固定
here::here()


#全部の変数を消す
rm(list=ls())

stats_summary <- read_csv("baseball/stats_summary.csv")

data <- stats_summary

#横浜をDeNAに変換する
data <- data %>%
  mutate(team = チーム) %>%
  mutate(team = 
           if_else(team == "横浜", "DeNA", team))

#チームイニシャルをチーム名に変換する
data <- data %>%
  mutate(team = 
           case_when(team =="F" ~ "日本ハム",
                     team =="H" ~ "ソフトバンク",
                     team =="B" ~ "オリックス",
                     team =="M" ~ "ロッテ",
                     team =="L" ~ "西武",
                     team =="E" ~ "楽天",
                     TRUE ~ team)) 

#OPSを作成する
data <- data %>% 
  mutate(OPS = 出塁率+長打率)

data  %>%
  filter(選手名　== "島内　宏明") %>%
  view()

#規定打席数443に達した選手に絞り込み
data <- data %>% 
  filter(打席数　>　443)


view(data)
head(data)

#本塁打を従属変数としてモデル構築
run4gm<- list()
run4gm[['Model 1']]　<- lm(data = data, formula = 本塁打 ~ 安打)
run4gm[['Model 2']]　<- lm(data = data, formula = 本塁打 ~ 安打+四球)
run4gm[['Model 3']]　<- lm(data = data, formula = 本塁打 ~ 安打+三振+四球)
msummary(run4gm)

#OPSを従属変数として重回帰分析
OPS_重回帰分析<- list()
OPS_重回帰分析[['Model 1']]　<- lm(data = data, formula = OPS ~ 本塁打)
OPS_重回帰分析[['Model 2 ']]　<- lm(data = data, formula = OPS ~ 四球)
OPS_重回帰分析[['Model 3']]　<- lm(data = data, formula = OPS ~ 本塁打+四球)
OPS_重回帰分析[['Model 4']]　<- lm(data = data, formula = OPS ~ 本塁打+四球+安打)
OPS_重回帰分析[['Model 5 ']]　<- lm(data = data, formula = OPS ~ 本塁打+四球+安打+得点)

msummary(OPS_重回帰分析)

#OPSと三振のチームごとのプロット書いてみて
ggplot(data = data) +     # tenkukan20データでキャンバス準備
  aes(x = OPS, y = 三振)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_point() +                  # 散布図を描く
  scale_colour_tableau()+
  geom_smooth(method = "lm")+  #回帰直線を描く
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#OPSを従属変数として単回帰分析（三振）
OPS_三振 <- lm(data = data, formula = OPS ~ 三振)
summary(OPS_三振)

#OPSと本塁打のプロット書いてみて
ggplot(data = data) +     # tenkukan20データでキャンバス準備
  aes(x = OPS, y = 本塁打)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_point() + 
  geom_smooth(method = "lm")+
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#チームごとの本塁打ヒストグラム
#ggplot
data %>% 
  filter(team == "中日"|team == "巨人"|team == "横浜"|team == "阪神"|team == "広島"|team == "ヤクルト")　%>%
  ggplot(aes(x = 本塁打,fill = team,colour = team)) +
  geom_histogram(alpha = 0.4,bins = 60) +
  ylim(0, 20) +
  labs(x = "本塁打", y = "出現度数")


#ソフトバンクチームデータ
softbank <- data %>% 
  filter(team == "ソフトバンク") 
view(softbank)

#柳田と山田のデータ作成
yanagitayamada <- data %>% 
  filter(選手名　== "柳田　悠岐"|選手名　== "山田　哲人") %>% 
  select(name = 選手名,ave=打率,walk = 四球,homerun=本塁打,OPS,year,stealing = 盗塁 )

view(yanagitayamada)

#柳田と山田の打率年次推移
ggplot(yanagitayamada, aes(x = year, y = ave))+
  geom_line (aes(x = year, y = ave,colour = name))+
  geom_point() +
  scale_colour_tableau()+
  labs(color = "year")+ #凡例のタイトルを指定
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない


#柳田と山田の本塁打年次推移
ggplot(yanagitayamada, aes(x = year, y = homerun))+
  geom_line (aes(x = year, y = homerun,colour = name))+
  geom_point() +
  scale_colour_tableau()+
  labs(color = "year")+ #凡例のタイトルを指定
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#柳田と山田の盗塁年次推移
ggplot(yanagitayamada, aes(x = year, y = stealing))+
  geom_line (aes(x = year, y = stealing,colour = name))+
  geom_point() +
  scale_colour_tableau()+
  labs(color = "year")+ #凡例のタイトルを指定
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#チームの平均打率・本塁打・盗塁・得点・OPS
data %>% 
  select(team,打率,本塁打,盗塁,得点,OPS) %>% 
  tbl_summary(
    by = team,
    digits = all_continuous() ~ 3)

#セ・リーグだけのデータを作る
cleague <- data %>% 
  filter(team == "巨人"|team == "中日"|team == "阪神"|team == "ヤクルト"|team == "DeNA"|team == "広島")

#パ・リーグだけのデータを作る
pleague <- data %>% 
  filter(team == "楽天"|team == "オリックス"|team == "ロッテ"|team == "西武"|team == "ソフトバンク"|team == "日本ハム")

#チームのOPSの箱ひげ図を書いて比較してみて
ggplot(data = data) +     # tenkukan20データでキャンバス準備
  aes(x = team, y = OPS, fill = team)+ # height,weight列をx,y軸にmapping,sexごとに色分け
  geom_boxplot() +                  # はこひげ図を描く
  xlab("team") + ylab("OPS") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#パ・リーグのチームの平均打率・本塁打・盗塁・得点・OPS
pleague %>% 
  select(team,打率,本塁打,盗塁,得点,OPS) %>% 
  tbl_summary(
    by = team,
    digits = all_continuous() ~ 3)

#パ・リーグのチームの平均OPSをCI95エラーバーをつけて書く（ggplot）
ggplot(data = pleague)+
  aes(x = team, y = OPS, color = team)+
  stat_summary(aes(x = team),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("team") + ylab("OPS") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#パ・リーグのチームの得点をCI95エラーバーをつけて書く（ggplot）
ggplot(data = pleague)+
  aes(x = team, y = 得点, color = team)+
  stat_summary(aes(x = team),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("team") + ylab("得点") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#セ・リーグのチームの平均打率・本塁打・盗塁・得点・OPS
cleague %>% 
  select(team,打率,本塁打,盗塁,得点,OPS) %>% 
  tbl_summary(
    by = team,
    digits = all_continuous() ~ 4)

#セ・リーグのチームのOPSをsCI95エラーバーをつけて書く（ggplot）
ggplot(data = cleague)+
  aes(x = team, y = OPS, color = team)+
  stat_summary(aes(x = team),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("team") + ylab("OPS") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#セ・リーグのチームの得点をCI95エラーバーをつけて書く（ggplot）
ggplot(data = cleague)+
  aes(x = team, y = 得点, color = team)+
  stat_summary(aes(x = team),fun = "mean", geom = "point", size = 3)+　#平均値のプロット
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.1, lwd = 1)+
  xlab("team") + ylab("得点") +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない
