library("magrittr")
df <- read.csv("Distance_data.csv",stringsAsFactors = FALSE)
moji <- c()
flag <- 0
#元のデータ名が長いので変更
df_col_name <- df[,1]
df_row_name <- names(df)
df[,1] <- LETTERS[1:length(df_col_name)] #列名変更
colnames(df) <- c("X",LETTERS[1:(length(df_row_name)-1)]) #行名変更

my_df <- data.frame(LETTERS , 0)
my_df$LETTERS <- as.character(my_df$LETTERS)


hoge <- lapply(1:20, function(x){
  # 最小の値を見つける
  min_num <- df[2:length(df)][!is.na(df[2:length(df)])] %>% as.numeric() %>% min()
  # 列番号
  a <- df == min_num
  col_num <- ceiling(grep(TRUE,a) / length(df[,1]))
  df_col <- df[col_num] %>% names
  # 行番号
  row_num <- grep(TRUE,a) %% length(df[,1])
  if(row_num == 0) row_num = length(df[,1])
  df_row <- df[row_num,1] %>% as.character()
  
  #数値を当てはめる
  my_a <- my_df[1] == df_col
  my_a_num <-  which(my_a) %>% my_df[.,2]
  my_b <- my_df[1] == df_row
  my_b_num <-  which(my_b) %>% my_df[.,2]
  
  if (length(df) == 2) { #最後の例外処理
    if (flag == 1) return()
    moji[x] <- paste0("(",names(df[1,][2]),":",as.numeric(df[1,2])/ 2 - as.numeric(my_a_num) ,",",df[1,1],":",as.numeric(df[1,2])/2 - as.numeric(my_b_num),")")
    flag <<- 1
    return(moji[x])
  }
  #文字作成
  moji[x] <- paste0("(",df_col,":",as.numeric(min_num)/ 2 - as.numeric(my_a_num),",",df_row,":",as.numeric(min_num)/2 - as.numeric(my_b_num) ,")")

  my_df <<- rbind(my_df,c(moji[x],as.numeric(min_num)/2))
  
  #追加列生成
  tmp_data <- df
  tmp <- tmp_data[,1] == df_col
  sp <- ifelse(which(tmp) %>% length() != 0,tmp_data <- tmp_data[-which(tmp),] ,0) 
  tmp <- tmp_data[,1] == df_row
  sp <- ifelse(which(tmp) %>% length() != 0,tmp_data <- tmp_data[-which(tmp),] ,0) 
  tmp_df <- tmp_data #後で再利用
  #列データ生成
  col_data <- lapply(1:length(tmp_data$X),function(y){
    btmp <- names(tmp_data) == df_col
    col_tmp <- tmp_data[y,which(btmp)] %>% as.integer()
    if(length(col_tmp) == 0) col_tmp = 0
    btmp <- names(tmp_data) == df_row
    row_tmp <-  tmp_data[y,which(btmp)] %>% as.integer()
    if(length(row_tmp) == 0) row_tmp = 0
    ifelse(is.na(col_tmp),return(NA),return((col_tmp + row_tmp) / 2))
  }) %>% unlist()
  # 追加行生成
  tmp_data2 <- df
  tmp <- names(tmp_data2) == df_col
  sp <- ifelse(which(tmp) %>% length() != 0,tmp_data2 <- tmp_data2[,-which(tmp)] %>% as.data.frame() ,0) 
  tmp <- names(tmp_data2) == df_row
  sp <- ifelse(which(tmp) %>% length() != 0,tmp_data2 <- tmp_data2[,-which(tmp)] %>% as.data.frame() ,0) 
  # factor型を消す
  if (is.factor(tmp_data2[1,])) tmp_data2$. <- as.character(tmp_data2$.)
  # 行データ
  if (length(tmp_data2[1,]) >= 2){
    row_data <- lapply(2:length(tmp_data2[1,]),function(y){
      btmp <- tmp_data2[,1] == df_col
      col_tmp <- tmp_data2[which(btmp),y] %>% as.integer()
      if(length(col_tmp) == 0) col_tmp = 0
      btmp <- tmp_data2[,1] == df_row
      row_tmp <-  tmp_data2[which(btmp),y] %>% as.integer()
      if(length(row_tmp) == 0) row_tmp = 0
      ifelse(is.na(col_tmp),NA,(col_tmp + row_tmp) / 2)
    }) %>% unlist()
  } else {
    row_data <-c()
  }
  row_data <- c(moji[x],row_data,NA)
  # 行削除
  df <- tmp_df
  #列削除
  tmp <- names(df) == df_col
  sp <- ifelse(which(tmp) %>% length() != 0,df <- df[,-which(tmp)]  %>% as.data.frame() ,0) 
  tmp <- names(df) == df_row
  sp <- ifelse(which(tmp) %>% length() != 0,df <- df[,-which(tmp)]  %>% as.data.frame() ,0) 
  # newデータをくっつける
  if (is.factor(df[1,])) df$. <- as.character(df$.)
  df <- cbind(df,col_data)
  df <- rbind(df,row_data)
  aa <- names(df)  %>% .[-length(.)] %>% c(.,moji[x]) 
  colnames(df) <- aa
  #全部NAの列を削除
  spoil <- lapply(1:nrow(df), function(y){
    ifelse(df[y,2:ncol(df)] %>%  is.na %>% which() %>% length() == length(df[y,2:ncol(df)]),y,0) 
  }) %>% unlist()
  sp <- ifelse(sum(spoil) != 0,df <- df[-spoil,],0)
  #全部NAの行を削除
  spoil <- lapply(2:ncol(df), function(y){
    ifelse(df[1:nrow(df),y] %>%  is.na %>% which() %>% length() == length(df[1:nrow(df),y]),y,0) 
  }) %>% unlist()
  sp <- ifelse(sum(spoil) != 0,df <- df[,-spoil],0)
  df <<- df
  return()
}) %>% unlist()

#変更した名前を元に戻す
lapply(1:14, function(x){
  hoge <<- gsub( paste0("\\(",LETTERS[x],":"), paste0("(",df_col_name[x],":"), hoge)
  hoge <<- gsub( paste0(",",LETTERS[x],":"), paste0(",",df_col_name[x],":"), hoge)
})
hoge <- paste0(hoge,";")
write (hoge, file = "result.nwk")

