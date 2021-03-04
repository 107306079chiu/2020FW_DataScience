bad_prop<-function(all_ref){
  times<-0
  all<-0
  if(query_m =="bad"){
    for(ref in all_ref){
      all<-all+1
      if(ref == "bad"){
        times<-times+1
      }
    }
  }else{
    for(ref in all_ref){
      all<-all+1
      if(ref == "good"){
        times<-times+1
      }
    }
  }
  # for(ref in all_ref){
  #   all<-all+1
  #   if(ref == "bad"){
  #     times<-times+1
  #   }
  #}
  return(times/all)
}
cal_sentv<-function(tp,fn){
  return(tp/(tp+fn))
}
cal_spec<-function(tn,fp){
  return(tn/(tn+fp))
}
cal_f1<-function(tp,fp,fn){
  precision<-tp/(tp+fp)
  recall<-tp/(tp+fn)
  return(2*precision*recall/(precision+recall))
}
cal_log<-function(target, all_ref, all_pscore){
  likeli<-0
  if(target=="bad"){
    likeli_model<-sum(ifelse(all_ref=='bad', log(all_pscore), log(1-all_pscore)))
    likeli<-likeli_model
  }else{
    likeli_model<-sum(ifelse(all_ref=='good', log(all_pscore), log(1-all_pscore)))
    likeli<-likeli_model
  }
  return(likeli)
}
cal_pseudoR2<-function(likeli_model, likeli_null){
  s<-0
  dev_model<- -2*likeli_model-s
  dev_null<- -2*likeli_null-s
  return(1-(dev_model/dev_null))
}

#read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_107306079.R --target bad/good --badthre <threshold> --input meth1 meth2 ... methx --output result.csv", call.=FALSE)
}

#parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--badthre"){
    bthre<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

methd<-c()
sentv<-c()
specf<-c()
f1<-c()
loghd<-c()
psuR2<-c()

for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  all_ref<-c(d$reference)
  
  #build a null model here
  null_prop<-bad_prop(all_ref)
  all_pscore_null<-c()
  for(i in length(all_ref)){
    all_pscore_null<-c(all_pscore_null, null_prop)
  }
  templog_null<-cal_log(query_m, all_ref, all_pscore_null) 
  
  if(query_m == "bad"){
    all_pscore<-c(d$pred.score)
    cM<-table(truth=all_ref, prediction=all_pscore>as.double(bthre))
    tp<-cM[1,2]
    fp<-cM[2,2]
    tn<-cM[2,1]
    fn<-cM[1,1]
  }else if(query_m == "good"){
    # bthre<-as.double(bthre)
    # bthre<-1-bthre
    # all_pscore<-c(1-d$pred.score)
    bad_score<-c(d$pred.score)
    all_pscore<-c(1-d$pred.score)
    cM<-table(truth=all_ref, prediction=bad_score<as.double(bthre))
    tp<-cM[2,2]
    fp<-cM[1,2]
    tn<-cM[1,1]
    fn<-cM[2,1]
  }
  methd<-c(methd, name)
  sentv<-c(sentv, round(cal_sentv(tp,fn), digits = 2)) 
  specf<-c(specf, round(cal_spec(tn,fp), digits = 2))
  f1<-c(f1, round(cal_f1(tp,fp,fn), digits = 2))
  templog_model<-cal_log(query_m, all_ref, all_pscore) 
  loghd<-c(loghd, round(cal_log(query_m, all_ref, all_pscore), digits = 2))
  psuR2<-c(psuR2, round(cal_pseudoR2(templog_model, templog_null), digits = 2))
}
out_data<-data.frame(method=methd, sensitivity=sentv, specificity=specf, F1=f1, logLikelihood=loghd, pseudo_R2=psuR2, stringsAsFactors = F)
index<-sapply(out_data[2:6], which.max)
#output file
out_data<-rbind(out_data,c("max",methd[index]))
write.table(out_data, file=out_f, row.names = F, quote = F, sep=",")
