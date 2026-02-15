#ライブラリの準備
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(readxl)
library(NipponMap)
library(sf)

#カラーパレットの確認
display.brewer.all()

#地図用パッケージのインストール
install.packages("NipponMap")

# エクセルファイルの読み込み
# ヘッダ部分を読み飛ばしている
# sheet=1を変更することで別のシートも読める

birthrate_pref2022 <- read_excel("birthrate_pref2022.xlsx")
sampleDB<-readxl::read_excel("20201028sample.xls",skip=1,sheet=1)


#birthrate_pref2022の2022の列のみ残す
birthrate_pref2022<-birthrate_pref2022　%>% 
  select(pref_year,"2022")

#birthrate_pref2022のpref_yearに入っている半角スペースを除去
birthrate_pref2022$pref_year<-gsub(" ","",birthrate_pref2022$pref_year)

view(birthrate_pref2022)

# 列１と列２の名前を変更している。
names(sampleDB)[1:2]<-c("prefcode","prefnameJ")

#birthrate_pref2022のpref_yearとsampleDBのprefnameJを基準に結合
sampleDB<-left_join(sampleDB,birthrate_pref2022,by=c("prefnameJ"="pref_year"))



view(sampleDB)



# データと地図を結合する際にキーの型が同じ必要があるので
# 数値型を文字型に変更している。
sampleDB$prefcode<-as.character(sampleDB$prefcode)

# 地図の情報はNipponMapから取り出しています。
Nippon_map <- read_sf(system.file("shapes/jpn.shp", package = "NipponMap")[1],
                      crs = "+proj=longlat +datum=WGS84")

# 地図情報に総務省のデータベースを接続
mapDB<-left_join(Nippon_map,sampleDB, by=c("SP_ID"="prefcode"))

view(mapDB)

# 地図にプロット
ggplot(mapDB, aes(fill = `2022`)) + 
  geom_sf() + 
  scale_fill_gradientn(colors=brewer.pal(9,"Greens"), limits = c(0.80, 2.0))+
  theme_gray (base_family = "HiraKakuPro-W3")+
  labs(fill = "")+
  ggtitle("都道府県別合計特殊出生率2022")
