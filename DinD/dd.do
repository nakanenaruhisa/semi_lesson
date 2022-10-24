
*データ呼び出し
webuse hospdd,clear

*ラベル変更
label variable hospital "病院ID"
label variable frequency "病院訪問頻度"
label variable month "月"
label variable procedure "介入"
label variable satis "患者満足スコア"

*来院頻度と満足度比較
mean satis, over(frequency)

**来院頻度と満足度比較箱ひげ
graph box satis, over(frequency) scheme(gg_tableau) scale(1.2)



*差の差回帰分析
didregress (satis)(procedure),group(hospital) time (month)

*グラフ表示
estat trendplots,scheme(gg_tableau) scale(1.2)
