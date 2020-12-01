library(rvest)
library(tidyverse)
library(magrittr)

df <- read.csv("2020urldata.csv")

my_tmp_data <- lapply(1:length(df$name),function(x){
  tmp_num <- c(NA,NA)
  if(df$url[x] != "https://www.realclearpolitics.com/epolls/2020/president/2020_elections_electoral_college_map.html"){
    tmp_data <- read_html(df$url[x])
    tmp_hoge <- html_table(tmp_data,fill = TRUE)
    if(length(tmp_hoge) != 0){
      tmp_fuga <- tmp_hoge[[1]]
      bool <- grep("Average",tmp_fuga$Poll) %>% length()
      my_bool <- tmp_fuga$Poll == "Final Results"
      tmp_fuga <- tmp_fuga[!my_bool,]
      tmp_num[1] <- ifelse(bool != 0,tmp_fuga$`Biden (D)`[1],ave(tmp_fuga$`Biden (D)`))
      tmp_num[2] <- ifelse(bool != 0,tmp_fuga$`Trump (R)`[1],ave(tmp_fuga$`Trump (R)`))
    }
  }
  return(tmp_num)
}) %>% unlist

data_num <- length(df$name) * 2
df2 <- data.frame(State = df$name, Biden = my_tmp_data[1:data_num%%2 == 1], Trump = my_tmp_data[1:data_num%%2 == 0])
write.csv(df2,"2020ave.csv", row.names= FALSE)