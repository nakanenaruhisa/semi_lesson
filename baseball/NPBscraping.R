install.packages("rvest")
library("rvest")
library(tidyverse)

sbatch2022 <- read_html("https://baseballdata.jp/ctop.html")

view(html)
