#packageの準備
install.packages("RColorBrewer")
install.packages("ggplot2")

#ライブラリの準備
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

#カラーパレットの確認
display.brewer.all()

#地図用パッケージのインストール
install.packages("NipponMap")

#ライブラリの準備
library(NipponMap)
library(sf)

#ウェブサイトから直接ダウンロードする場合
url1<-"https://yamamoto-masashi.github.io/DSlec/20201028sample.xls"
download.file(url1,destfile="20201028sample.xls")

# エクセルファイルの読み込み
# ヘッダ部分を読み飛ばしている
# sheet=1を変更することで別のシートも読める
sampleDB<-readxl::read_excel("20201028sample.xls",skip=5,sheet=1)

view(sampleDB)

# 変数の対応関係
# A1101_総人口【人】  
# A1301_15歳未満人口【人】
# A1303_65歳以上人口【人】  
# B1101_ 総面積（北方地域及び竹島を除く）【ｈａ】   
# B1103_ 可住地面積【ｈａ】  
# B4107_ 雪日数（年間）【日】 
# B4108_ 日照時間（年間）【時間】   
# D110101_市町村数【‐】   
# E6102_大学数【校】  
# E6302_大学学生数【人】
# F610201_超過実労働時間数（男）【時間】   
# F610202_超過実労働時間数（女）【時間】   
# H110202_空き家数【戸】

# 列１と列２の名前を変更している。
names(sampleDB)[1:2]<-c("prefcode","prefnameJ")

# データと地図を結合する際にキーの型が同じ必要があるので
# 数値型を文字型に変更している。
sampleDB$prefcode<-as.character(sampleDB$prefcode)

# 地図の情報はNipponMapから取り出しています。
Nippon_map <- read_sf(system.file("shapes/jpn.shp", package = "NipponMap")[1],
                      crs = "+proj=longlat +datum=WGS84")

view(Nippon_map)

# 地図情報に総務省のデータベースを接続
mapDB<-left_join(Nippon_map,sampleDB, by=c("SP_ID"="prefcode"))

# 地図にプロット
ggplot(mapDB, aes(fill = B4107)) + 
  geom_sf() + 
  scale_fill_gradientn(colors=brewer.pal(9,"GnBu"))+
  theme_gray (base_family = "HiraKakuPro-W3")+
  labs(fill = "年間雪日数(日)")+
  ggtitle("都道府県別の雪日数 (2018年)")

#京都市北区の地図を読み込み
map_kitaku <- read_sf("map/kyoto/kitaku/r2ka26101.shp",
               crs = "+proj=longlat +datum=WGS84") # 京都府のシェープファイル

view(map)

ggplot(map_kitaku) + geom_sf()

ggplot(map_kitaku) + geom_sf(aes(fill=JINKO))+
  scale_fill_gradientn(colors=brewer.pal(9,"GnBu"))+
  theme_gray (base_family = "HiraKakuPro-W3")+
  labs(fill = "単位：人")+
  ggtitle("国勢調査(2020年)における京都市北区の人口 (町丁・字等別)")

#京都市伏見区の地図を読み込み
map_fushimiku <- read_sf("map/kyoto/fushimiku/r2ka26109.shp",
                      crs = "+proj=longlat +datum=WGS84") # 京都府のシェープファイル

#S_NAMEの最初の2文字を削った変数であるS_S_NAMEを作成してください
map_fushimiku <- map_fushimiku %>%
  mutate(S_S_NAME = substr(S_NAME, 3, nchar(S_NAME)))  # 先頭5文字を削除

view(map_fushimiku)

ggplot(map_fushimiku) + geom_sf()

ggplot(map_fushimiku) + geom_sf(aes(fill=JINKO))+
  scale_fill_gradientn(colors=brewer.pal(9,"GnBu"))+
  #geom_sf_text(aes(label=S_S_NAME), size=2, color="black", check_overlap = TRUE) +  # 町名ラベルを追加
  theme_gray (base_family = "HiraKakuPro-W3")+
  labs(fill = "単位：人")+
  ggtitle("国勢調査(2020年)における京都市伏見区の人口 (町丁・字等別)")

