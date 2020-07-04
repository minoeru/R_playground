library("magrittr")

N = 100
slotTime <- 100000

template <- matrix(1:(slotTime*N),nrow=N,ncol = slotTime) * 0

spoil <- lapply(1:60,function(x){
  lambda <- 0.0005 * x
  
  data <- template
  
  # 各局毎に送信
  for(i in 1:N){
    tmp <- runif(slotTime,0,1/lambda) %>% ceiling()
    tmp <- (1:slotTime)[tmp == 1] [1]
    data[i,tmp] <- 1
    interval <- round((-1 * log(runif(1)) / lambda) + 1)
    num <- floor((slotTime - tmp) / interval)
    for(j in 1:num) data[i,tmp + j * interval] <- 1
  }
  
  # 重複数カウント
  tmp <- lapply(1:slotTime,function(y){
    sum(data[,y]) == 1
  }) %>% unlist()
  count = sum(tmp == TRUE)
  # 成功率
  return(count / slotTime)
}) %>% unlist()
  

print(max(spoil))
print(ave(spoil))
plot(spoil,xlab="Incidence",ylab="Throughput")