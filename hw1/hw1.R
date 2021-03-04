args = commandArgs(trailingOnly=TRUE)
if(length(args) == 4){
  if (args[1]=="--input"){
    i_f <- args[2]
    o_f <- args[4]
  }else if(args[1]=="--output"){
    o_f <- args[2]
    i_f <- args[4]
  }
}else{
  stop("USAGE: Rscript hw1_107306079.R input", call.=FALSE)
}
all_content <- read.csv(i_f)
w<-999
h<-999
for(i in c(1:length(colnames(all_content)))){
  if(colnames(all_content)[i]=="weight"){
    w<-i
  }else if(colnames(all_content)[i]=="height"){
    h<-i
  }
}
all_weight <- all_content[w]
all_height <- all_content[h]
max_height <- 0
max_weight <- 0
all_length <- length(all_weight[,1])
for (i in c(1:all_length)){
  if(all_weight[i,]>=max_weight){
    max_weight <- all_weight[i,]
  }
  if(all_height[i,]>=max_height){
    max_height <- all_height[i,]
  }
}
set <- gsub(".csv", "", i_f)
weight <- round(max_weight, digits = 2)
height <- round(max_height, digits = 2)
out_frame <- data.frame(set, weight, height)
write.csv(out_frame, file = o_f, quote = FALSE, row.names = FALSE)