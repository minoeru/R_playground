# 入力例
# >seq1
# GAATCGGTGTACGTCTAGGGCCGATTGATC
# >seq2
# TACTGCTTCACGAAGGAACCCACTATTA

data <- c(1:4)
for(i in 1:4) data[i] <- readline()

data1 <- unlist(strsplit(data[2],""))
data2 <- unlist(strsplit(data[4],""))
data1_len <- length(data1)
data2_len <- length(data2)
row_len <- data1_len+1
col_len <- data2_len+1

# 使用スコア作成
matnum <- c(1,-1,-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1,-1,1)
score_mat <- matrix(matnum, nrow=4, ncol=4)
rownames(score_mat) <- c("A","T","G","C")
colnames(score_mat) <- c("A","T","G","C")
# ギャップ
gap_num <- -2
# アライメントのスコア作成
data_mat <- matrix(0, nrow=row_len, ncol=col_len)
rownames(data_mat) <- c(" ",data1)
colnames(data_mat) <- c(" ",data2)
data_mat[2:(data1_len+1),1] <- gap_num * 1:data1_len
data_mat[1,2:(data2_len+1)] <- gap_num * 1:data2_len
# アライメントの方向作成
data_dir_mat <- matrix(0, nrow=row_len, ncol=col_len)
rownames(data_dir_mat) <- c(" ",data1)
colnames(data_dir_mat) <- c(" ",data2)
data_dir_mat[2:row_len,1] <- "u"
data_dir_mat[1,2:col_len] <- "l"
# データ入力
tmp_num <- c(1:3)# lest,up,dlanting
dir_label <- c("u","l","d")
spoil <- lapply(1:data1_len,function(i){
  lapply(1:data2_len,function(j){
    tmp_num[1] <- data_mat[i,j+1] -2
    tmp_num[2] <- data_mat[i+1,j] -2
    tmp_num[3] <- data_mat[i,j] + score_mat[data1[i],data2[j]]
    # スコア
    data_mat[i+1,j+1] <<- max(tmp_num)
    # 方向
    data_dir_mat[i+1,j+1] <<- ifelse(length(dir_label[tmp_num == max(tmp_num)]) == 1,dir_label[tmp_num == max(tmp_num)],dir_label[max(tmp_num == max(tmp_num))])
    
  })
})

# アライメント作成
ans_1 <- ""
ans_2 <- ""
while(TRUE){
  if(data_dir_mat[row_len,col_len] == "d"){
    ans_1 <- paste0(data1[row_len - 1],ans_1)
    ans_2 <- paste0(data2[col_len-1],ans_2)
    row_len <- row_len -1
    col_len <- col_len -1
  }
  else if(data_dir_mat[row_len,col_len] == "u"){
    ans_1 <- paste0(data1[row_len - 1],ans_1)
    ans_2 <- paste0("-",ans_2)
    row_len <- row_len - 1
  }
  else{
    ans_1 <- paste0("-",ans_1)
    ans_2 <- paste0(data2[col_len-1],ans_2)
    col_len <- col_len - 1
  }
  if(row_len == 1 && col_len == 1) break
}

print(data_mat)
print(ans_1)
print(ans_2)
