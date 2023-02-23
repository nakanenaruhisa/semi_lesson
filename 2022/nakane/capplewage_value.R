#Rを使う下準備
#packageの準備
library(tidyverse)
library(ggplot2)
library(GGally)
library(dbplyr)
library(ggthemes)
library(gtsummary)
library(ggmosaic)

#全部の変数を消す
rm(list=ls())

#テーマのセット
theme_set(theme_gray(
  base_family = "HiraginoSans-W3",# macOS用
  base_size = 11, #文字の大きさを設定。デフォルトは11
  base_line_size = 0.2, #罫線の線の太さを設定。デフォルトはbase_size/22
  base_rect_size = 0.2 #外枠の線の太さを設定。デフォルトはbase_size/22
))

#夫データセットの読み込み
X1138_hus <- read_excel("semi_lesson/2022/endo/現代核家族調査2008/1138_hus.xlsx")

#妻データセットの読み込み
X1138_wife <- read_excel("semi_lesson/2022/endo/現代核家族調査2008/1138_wife.xlsx")

#夫変数の絞り込み
hus <- X1138_hus %>% 
  select(id,h12,h46a,h46b,h46c,h46d,h46e,h46f) %>% 
  mutate(h12 = if_else(h12 == 99, NA_real_, h12)) %>% 
  mutate(h46a = na_if(h46a, 9)) %>% 
  mutate(h46b = na_if(h46b, 9)) %>%
  mutate(h46c = na_if(h46c, 9)) %>%
  mutate(h46d = na_if(h46d, 9)) %>% 
  mutate(h46e = na_if(h46e, 9)) %>% 
  mutate(h46f = na_if(h46f, 9)) %>%
  drop_na() %>%
  mutate(h12_c = factor(h12,
                 levels = 0:14,
                 labels = c("収入はない","5万円未満","5〜10万円未満",
                                              "10〜15万円未満","15〜20万円未満",
                                              "20〜25万円未満","25〜30万円未満",
                                              "30〜35万円未満","35〜40万円未満",
                                              "40〜45万円未満","45〜50万円未満",
                                              "50〜55万円未満","55〜60万円未満",
                                              "60〜65万円未満","65万円以上" )))

hus <- hus %>%
  mutate(h46a_c= factor(h46a,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(h46b_c= factor(h46b,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>%
  mutate(h46c_c= factor(h46c,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(h46d_c= factor(h46d,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(h46e_c= factor(h46e,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(h46f_c= factor(h46f,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対")))
#夫の性別役割分業スコアを作成
hus <- hus %>% 
  mutate(familyvalue_h= h46a+h46b+h46c+h46d+h46e+h46f)

mean(hus$familyvalue_h)

#妻変数の絞り込み
wife <- X1138_wife %>% 
  select(id,w12_s1,w46a,w46b,w46c,w46d,w46e,w46f) %>%
  mutate(w12_s1 = if_else(w12_s1 == 99, NA_real_, w12_s1)) %>% 
  mutate(w12_s1 = if_else(w12_s1 == 88, NA_real_, w12_s1)) %>% 
  mutate(w46a = na_if(w46a, 9)) %>% 
  mutate(w46b = na_if(w46b, 9)) %>%
  mutate(w46c = na_if(w46c, 9)) %>%
  mutate(w46d = na_if(w46d, 9)) %>% 
  mutate(w46e = na_if(w46e, 9)) %>% 
  mutate(w46f = na_if(w46f, 9)) %>%
  drop_na() %>%
  mutate(w12_c = factor(w12_s1,
                        levels = 1:14,
                        labels = c("5万円未満","5〜10万円未満",
                                   "10〜15万円未満","15〜20万円未満",
                                   "20〜25万円未満","25〜30万円未満",
                                   "30〜35万円未満","35〜40万円未満",
                                   "40〜45万円未満","45〜50万円未満",
                                   "50〜55万円未満","55〜60万円未満",
                                   "60〜65万円未満","65万円以上" )))
#妻の性別役割分業スコアを作成
wife <- wife %>% 
  mutate(familyvalue_w= w46a+w46b+w46c+w46d+w46e+w46f)

mean(wife$familyvalue_w)

wife <- wife %>%
  mutate(w46a_c= factor(w46a,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(w46b_c= factor(w46b,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>%
  mutate(w46c_c= factor(w46c,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(w46d_c= factor(w46d,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(w46e_c= factor(w46e,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対"))) %>% 
  mutate(w46f_c= factor(w46f,
                      levels = 1:4,
                      labels = c("賛成","まあ賛成",
                                 "やや反対","反対")))

#夫妻データのマージ
capple <- full_join(hus,wife, by= "id")



#夫妻データの絞り込み
capple <- capple %>% 
  select(id,h12,h12_c,h46a_c,h46b_c,h46c_c,h46d_c,h46e_c,h46f_c,w12,w12_c,w46a_c,w46b_c,w46c_c,w46d_c,w46e_c,w46f_c,familyvalue_h,familyvalue_w)

#データセットのテーブルを自動でつくる
capple %>%
  select(h12_c,w12_c,familyvalue_h,familyvalue_w) %>%
  drop_na() %>%
  tbl_summary()

#記述統計量にラベルを付けてテーブルで出力する
capple %>% 
  select(h12_c,w12_c) %>%
  drop_na() %>%
  tbl_summary(label = list(h12_c ~ "夫の手取り",
                           w12_c ~ "妻の手取り"), #~の前には列名、後ろにはつけたい名前を""で囲んで入れ、,で一つずつ区切る
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 1) %>% #数値の部分が小数点第y位の部分の値
  modify_header(label ~ "") # ""の部分には好きな文字列を入れられる。何も入れなければ空欄になる

#夫収入と夫家族スコアのプロット
ggplot(data = capple) +
  aes(x = h12, y = familyvalue_h)+ # height,weight列をx,y軸にmapping
  geom_point() +                  # 散布図を描く
  geom_jitter() +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

#妻収入と妻家族スコアのプロット
ggplot(data = capple) +
  aes(x = w12_s1, y = familyvalue_w)+ # height,weight列をx,y軸にmapping
  geom_point() +                  # 散布図を描く
  geom_jitter() +
  scale_colour_tableau()+
  theme_gray(base_size = 15) + #grayテーマで
  theme_gray(base_family = "HiraKakuPro-W3") #文字化けしないおまじない

