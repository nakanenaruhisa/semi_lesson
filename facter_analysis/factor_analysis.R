#packageの準備
install.packages( "psych" )
install.packages( "GPArotation" )

#ライブラリの準備
library( psych )
library( GPArotation )
data( bfi ) # これで bfi という変数が利用可能になります

#データを確認する
head(bfi, 3)

#データを抜き出す
dat = bfi[ 1:25 ]

KMO( dat ) # KMO値を計算する

# スクリープロットを描く
fa.parallel( dat, fa = "fa", use = "complete.obs" ) # スクリープロットを表示
abline( h = 0 ) # y = 0 の横線を追加

# 因子負荷の推定
result = fa( dat, nfactors = 5, fm = "minres", rotate = "oblimin", use = "complete.obs" )
print( result, digits = 3, sort = T )

#因子負荷の可視化
result = fa( dat, nfactors = 5, fm = "minres", rotate = "oblimin", use = "complete.obs" )
fa.diagram( result )

