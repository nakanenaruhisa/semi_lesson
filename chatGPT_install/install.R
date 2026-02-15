install.packages("remotes") 
remotes::install_github("jcrodriguez1989/chatgpt")
library("chatgpt")


library(usethis) 
usethis::edit_r_environ()
# • Modify '/Path/to/.Renviron' 
# • Restart R for changes to take effect 
# .Renvironファイルが開く


# .Renvironは起動時に読み込まれるため、再起動
.rs.restartR()
# Restarting R session... 

#test
chatgpt::chat("How are you?")

