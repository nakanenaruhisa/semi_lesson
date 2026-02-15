library(stringr)
library(readr)
library(tidyverse)
library(GGally)
library(dbplyr)
library(car)

#datasetを4つ読み込む
agedper <- read_csv("multireg/agedper.csv")
having_job_rate <- read_csv("multireg/having_job_rate.csv")
nintei_sum <- read_csv("multireg/nintei_sum.csv")
income_person <- read_csv("multireg/income_person.csv")

agedper %>% as_tibble
having_job_rate %>% as_tibble()
nintei_sum %>% as_tibble()
income_person %>% as_tibble()

#都道府県別完全失業率

#都道府県名に入った数字を削る
agedper$pref <- str_sub(agedper$pref,start = 4,end = 6)

#4つのデータセットを統合する
ltc_pref <- full_join(agedper,having_job_rate, by = "prefid")
ltc_pref <- full_join(ltc_pref,income_person,by = "prefid")
ltc_pref <- full_join(ltc_pref,nintei_sum,by = "prefid")

as_tibble(ltc_pref)

#認定率変数の作成
ltc_pref_all <- ltc_pref %>%
  mutate("nintei_per" = nintei_sum/population*100) 

as_tibble(ltc_pref_all)
tbl_summary(ltc_pref_all)

#統合したデータセットを表にする
ltc_pref_all %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))#~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
