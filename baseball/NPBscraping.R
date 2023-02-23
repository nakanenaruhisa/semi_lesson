install.packages("rvest")
library("rvest")
library(tidyverse)

html <- read_html("https://npb.jp/bis/2022/stats/bat_c.html")

view(html)
