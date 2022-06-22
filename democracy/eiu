#全部の変数を消す
rm(list=ls())

#datasetの読み込み
read.csv("/R/semi_lesson/democracy/EIU2020.csv")#このフォルダ名は個々人違うので修正すること

growth <- read_csv("democracy/wb_gdp_growth.csv")

growth <- wb_gdp_growth

#EIU2000をediという箱に入れる
eiu <- EIU2020

#Eiu2017年のデータのみを作る
eiu2017 <- eiu　%>%
  filter(year == 2017)#yearが2017のやつだけ抜き出して

only <- eiu %>% 
  select(eiu)

#dataの結合
eiu_growth <- full_join(eiu2017,growth, by= "country")

#dataの整理（欠損値がある国名を削除)
eiu_growth_na <- eiu_growth[!(is.na(eiu_growth$eiu) | is.na(eiu_growth$gdpgrowth)),]

#結合したdataの整理（year.yを削除、year.xの名称を変更)
eiu_growth_new <- eiu_growth_na %>% 
  select(country,year = year.x,eiu,gdpgrowth)

eiuscore <- eiu_growth_new$eiu #eiuスコア変数を作成
growthscore <- eiu_growth_new$gdpgrowth　#growthスコア変数を作成

#とりあえず、tableを作ってみる
eiu_growth_new %>% 
  select(eiu,gdpgrowth) %>% #eiuとgdp成長率だけ抜き出して
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))#平均と標準偏差つけてtable作って


#GDP成長率と民主主義スコアの関係の回帰式を書くよ
reg_growth_eiu <- lm(data = eiu_growth_new, formula = eiuscore ~ growthscore)

#GDPと民主主義スコアの回帰式を表にする
reg_growth_eiu %>% tbl_regression(intercept = TRUE)

#eiuとgrowthでプロットを書く
ggplot(data = eiu_growth_new) +
  aes(x = growthscore, y = eiuscore)+
  geom_point() +                  # 散布図を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#やたら外れてるリビアを取り除く
eiu_growth_new2 <- eiu_growth_new %>% 
  filter(country != "Libya")

eiuscore <- eiu_growth_new2$eiu #再度eiuスコア変数を作成
growthscore <- eiu_growth_new2$gdpgrowth　#再度growthスコア変数を作成

#eiuとgrowthで国名をつけてプロットを書く
ggplot(data = eiu_growth_new2) +
  aes(x = growthscore, y = eiuscore, color = "country")+
  geom_point() +  # 散布図を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#eiuとgrowthで国名をつけてプロットを回帰直線をつけて書く
ggplot(data = eiu_growth_new2) +
  aes(x = growthscore, y = eiuscore, color = "country")+
  geom_point() +  # 散布図を描く
  geom_smooth(method = "lm")+  #回帰直線を描く
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

