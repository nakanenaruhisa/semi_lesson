#パッケージの読み込み
library("gtsummary")
#tidyverseパッケージがなければインストール
if(!require("tidyverse", quietly = TRUE)){
  install.packages("tidyverse");require("tidyverse")
}

###データ例の作成#####
set.seed(1234)
TestData <- tibble(Group = sample(paste0("Group", 1:2), 100,
                                  replace = TRUE),
                   Data1 = sample(50:200, 100, replace = TRUE),
                   Data2 = sample(0:10, 100, replace = TRUE),
                   Data3 = sample(LETTERS[1:3], 100, replace = TRUE))
#確認
TestData

#データをテーブルに変換:tbl_summaryコマンド
#単純に変換
TestData %>%
  tbl_summary()

#Groupで分割し変換
#分割指標を設定:byオプション
TestData %>%
  tbl_summary(by = Group) %>%
  #列を追加
  add_n() %>% 
  #検定結果を追加:add_pオプション
  #各種検定の検定コマンドに引数を渡す:test.argsオプション
  #例では分散が等しいと仮定して"t.test"を求める
  #各種検定詳細は記事内を確認くしてください
  add_p(test = list(all_continuous() ~ "t.test",
                    Data3 ~ "chisq.test"),
        test.args = all_continuous() ~ list(var.equal = TRUE)) %>%
  #先頭行の内容設定:modify_headerオプション
  #**で囲むと太字になります,以下同様
  modify_header(label = "**指標**", p.value = "**P**") %>%
  #先頭行を分割する  
  modify_spanning_header(list(all_stat_cols() ~ "KARADA GROUP",
                              starts_with("p.value") ~ "**p-value**")) %>%
  #脚注の内容設定:modify_footnoteオプション
  modify_footnote(all_stat_cols() ~ "median (IQR) for KARADANI; N (%) for IIMONO") %>%
  #表題の内容設定:modify_footnoteオプション  
  modify_caption("**KARADA-GOOD** (N = {N})")
