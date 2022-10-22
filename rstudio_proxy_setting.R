options(RCurlOptions = list(proxy = "proxy.kpu.ac.jp:8080"))
Sys.setenv("http_proxy"="http://proxy.kpu.ac.jp:8080")
options(repos=local({ r <- getOption("repos"); r["CRAN"] <- "http://cran.ism.ac.jp"; r }))
