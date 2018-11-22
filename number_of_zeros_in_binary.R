rm(list = ls(all = TRUE))

n <- function(x){
  c <- c()   ## %/%是整除得商，%%是取余数
  
  while(x != 0){
    mod <- x %% 2
    c <- c(as.character(mod),c)
    x <- x %/% 2
  }
  
  bino <- as.numeric(c); print(bino)
  
  sum(c == 0)
  
  i <- 1; maxc <- 0; index <- 1
  while(i < length(c) && index){
    j <- i
    if(bino[i] == 1 && bino[i+1] == 0){
      beginc <- i
      for(j in (i+1):length(c)){
        if(bino[j] == 1){
          endc <- j
          break;
        }
        if(j == length(c)){
          j <- i
          index <- 0
        }
      }
      max0 <- sum(c[i:j] == 0)
      if(maxc < max0) maxc <- max0
      if(i < j) print(bino[i:j])
      i <- j
    }else i <- i+1
  }
  return(maxc)
}