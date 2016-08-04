rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

original_path <- getwd()

##Libraries
library(readxl)
library(dplyr)
#library(RecordLinkage)
library(data.table)
options(java.parameters = "-Xmx4g")
library(XLConnect)
#library(pbapply)
library(stringdist)
library(tmcn)
library(foreach) 
library(doSNOW)

##Import all files.
files <- list.files(file.path('raw data','就業藍圖所有學校原始資料'),full.names = TRUE)
##Something went wrong with lapply
files.lists <- pblapply(files,function(file.name){
  wb <- loadWorkbook(file.name)
  ##Read all sheets in a xlsx file as a list.
  file.list <- readWorksheet(wb, sheet = getSheets(wb)) 
  ##Turn the list into a data frame.
  file.list <- do.call(rbind,file.list) 
  gc()
  rm(wb)
  ##If the file only have 1 sheet, it should use method below to traslate to data frame.
  if(class(file.list)!="data.frame")
    file.list <- data.frame(t(file.list),stringsAsFactors=F)
  return(file.list)
})

##Combine all the lists into data frame.
total.data <- do.call(rbind,files.lists)
rm(files.lists,files)
##save the environment.
##save.image("D:/abc/wjhong/projects/Graduated.From.University/import.complete.RData")
##load("D:/abc/wjhong/projects/Graduated.From.University/import.complete.RData")
total.school.lookup.table <- read.csv("raw data\\最新學校科系資料.csv",stringsAsFactors=F)
industry.transform <- read.csv("raw data\\產業新類別vs舊類別.csv",stringsAsFactors=F)
job.transform <- read.csv("raw data\\職務小類轉換表.csv",stringsAsFactors=F)

setwd(file.path('Graduated.From.University'))
dir.create('output', showWarnings = FALSE)

##Remove redundant curriculum vitae.
total.data$履歷編號 <- NULL
total.data <- setDF(unique(setDT(total.data)))
nrow(total.data)

##Correct colleges' name.
college.name.transfer <- read.csv("學校名稱正規化表格.csv",stringsAsFactors=F)

##First job
first.job <- total.data[,c("會員編號","學校代碼", "學校名稱", "科系名稱", "科系類別代號", 
                          "科系類別名稱", "產業小類代碼", "產業小類名稱", 
                          "職務小類代碼", "職務小類名稱", "產業小類代碼1",
                          "產業小類名稱1", "職務小類代碼1", "職務小類名稱1",
                          "產業小類代碼2", "產業小類名稱2", "職務小類代碼2",
                          "職務小類名稱2", "產業小類代碼3", "產業小類名稱3", 
                          "職務小類代碼3", "職務小類名稱3")]
##Remove curriculum vitae without first job.
nrow(first.job)
#first.job %>% filter(職務小類名稱!="") %>% nrow()
#first.job <- first.job %>% filter(職務小類名稱!="")

first.job$學校名稱 <- toUTF8(first.job$學校名稱) ##iconv(first.job$學校名稱,"UTF-8")
first.job$科系名稱 <- toUTF8(first.job$科系名稱) ##iconv(first.job$科系名稱,"UTF-8")

##Correct colleges' name
uni.college.names <- unique(first.job$學校名稱)
for(x in 1:length(uni.college.names)){
  tmp <- ifelse(toString(rev(sort(college.name.transfer$對應表[college.name.transfer$trim後原始==uni.college.names[x]]))[1])!="" & toString(rev(sort(college.name.transfer$對應表[college.name.transfer$trim後原始==uni.college.names[x]]))[1])!="NA"
         , rev(sort(college.name.transfer$對應表[college.name.transfer$trim後原始==uni.college.names[x]]))[1]
         , uni.college.names[x])
  cat("\r" , format(round(x/length(uni.college.names)*100,3),nsmall=3),"% ")
  if(tmp!=uni.college.names[x]){
    first.job$學校名稱[first.job$學校名稱==uni.college.names[x]] <- tmp
    
    cat(uni.college.names[x], "==> ", tmp, rep(" ",50))
  }
}

##Create new col to record whether it's error or not.
first.job$正規化科系名稱 <- ""
first.job$error <- 0
first.job <- first.job[!grepl("學分班",first.job$科系名稱),]
##Correct departments' name
unique.college.department <- unique(first.job[,c("學校名稱", "科系名稱")])

##科系正規化依據應為多年科系(如104,105年), 再把舊名稱轉成新的
tmp <- total.school.lookup.table[,c("學校名稱","X105年科系名稱.藍字.105新增.綠字.改名.橘字.合併.紅字.停招.")]
tmp2 <- total.school.lookup.table[,c("學校名稱","X104年科系名稱")]
names(tmp) <- c("學校名稱","科系名稱")
names(tmp2) <- c("學校名稱","科系名稱")
department.old2new <- total.school.lookup.table
total.school.lookup.table <- rbind(tmp, tmp2)
total.school.lookup.table <- unique(total.school.lookup.table)
rm(tmp,tmp2)

for(x in 1:nrow(unique.college.department)){
  #jarowinkler('資管',c('資訊管理系','資訊工程學系'))
  ##check if having value in lookup table first.
  department.x <- total.school.lookup.table[which(unique.college.department$學校名稱[x] == total.school.lookup.table$學校名稱)
                                            ,"科系名稱"]
  department.x <- unlist(c(department.x))
  names(department.x)=NULL
  
  cat("\r", format(round(x/nrow(unique.college.department)*100,3),nsmall=3),"% ")
  
  if(length(department.x) > 0)
  {
    word <- unique.college.department$科系名稱[x]
    ##department.x[which.max(jarowinkler(word,department.x))[1]]
    first.job$正規化科系名稱[first.job$學校名稱==unique.college.department$學校名稱[x] & first.job$科系名稱==unique.college.department$科系名稱[x]] = department.x[which.max(1-stringdist(word,department.x ,method='jw'))[1]]
    cat(unique.college.department$科系名稱[x] , " ", department.x[which.max(1-stringdist(word,department.x ,method='jw'))[1]],rep(" ",50))
  }else{
    first.job$error[first.job$學校名稱==unique.college.department$學校名稱[x] & first.job$科系名稱==unique.college.department$科系名稱[x]] <- 1
    print(sprintf("Index %s Error: %s %s", x, unique.college.department$學校名稱[x], unique.college.department$科系名稱[x]))
  }
}
##Update old names to new.
department.old2new <- department.old2new[,c("學校名稱", "X104年科系名稱", "X105年科系名稱.藍字.105新增.綠字.改名.橘字.合併.紅字.停招.")]
department.old2new <- department.old2new[which(department.old2new[,2]!=department.old2new[,3]),]
names(department.old2new) <- c("學校名稱", "old", "new")
for(i in 1:nrow(department.old2new)){
  first.job$正規化科系名稱[first.job$學校名稱==department.old2new$學校名稱[x] & first.job$科系名稱==department.old2new$old[x]] <- department.old2new$new[x]
}

##save the environment.
##save.image("D:/abc/wjhong/projects/Graduated.From.University/names.tranfer.complete.RData")
##load("D:/abc/wjhong/projects/Graduated.From.University/names.tranfer.complete.RData")

##no.job:1~4 (first job ? ... or the forth one?)   ; type: 0(職務) or 1(產業)
job_func <- function(no.job,type){  
  gc()
  if(no.job==1){
    num.of.job <- first.job[,c("會員編號", "學校代碼", "學校名稱", "正規化科系名稱", "科系類別代號", 
                               "科系類別名稱", "產業小類代碼", "產業小類名稱", 
                               "職務小類代碼", "職務小類名稱","error")]
  }else{
    num.of.job <- first.job[,c("會員編號", "學校代碼", "學校名稱", "正規化科系名稱", "科系類別代號", 
                               "科系類別名稱", 
                               paste0(c("產業小類代碼", "產業小類名稱", "職務小類代碼",
                                        "職務小類名稱"),no.job-1),"error")]
  }
  names(num.of.job) <- c("會員編號", "學校代碼", "學校名稱", "正規化科系名稱", "科系類別代號", 
                         "科系類別名稱", "產業小類代碼", "產業小類名稱", 
                         "職務小類代碼", "職務小類名稱","error")
  
  if(type==1)
    col2p <- "產業小類名稱"
  if(type==0)
    col2p <- "職務小類名稱"
  print(paste0(substr(col2p,1,2)," => ", no.job))
  ##num.of.job %>% filter(職務小類名稱!="") %>% nrow()
  ##num.of.job <- num.of.job %>% filter(職務小類名稱!="")
  eval(parse(text=paste0("num.of.job %>% filter(",col2p,"!='') %>% nrow()")))
  num.of.job <- eval(parse(text=paste0("num.of.job %>% filter(",col2p,"!='')")))
  
  ##Remove data with error. (==1) 
  nrow(num.of.job[num.of.job$error==0,])
  num.of.job = num.of.job[num.of.job$error==0,]
  num.of.job$error = NULL
  num.of.job <- setDF(unique(setDT(num.of.job)))
  
  ##Counting job freq in each department of college.
  tmp.first <- num.of.job[,c("學校名稱", "正規化科系名稱",col2p)]
  names(tmp.first) <- c("school","department","job")
  tmp.first <- tmp.first %>% group_by(., school, department, job)
  count.num.of.job <- summarize(tmp.first,count=n()) 
  
  ##Combine with old standard file.
  standard.job <- read.csv("就業藍圖基準.csv",stringsAsFactors=F)
  names(standard.job)
  standard.job.first <- standard.job[standard.job$"類別.0.職務..1.產業."==type,c("學校名稱", "科系名稱", paste0(c("名稱", "樣本數"), no.job))]
  names(standard.job.first) <- c("school","department","job","count")
  
  ##Old names to new names
  for(i in 1:nrow(job.transform)){
    standard.job.first$job[standard.job.first$job==job.transform$old[i]] <- job.transform$new[i]
  }
  
  count.num.of.job <- rbind(count.num.of.job,standard.job.first)
  count.num.of.job <- count.num.of.job %>% group_by(., school, department, job)
  count.num.of.job <- count.num.of.job[count.num.of.job$count!="NULL",]
  #write.csv(count.num.of.job, "合併前查看2.csv",row.names=F)
  count.num.of.job <- count.num.of.job %>% mutate(., count=sum(as.numeric(count)))
  
  ##Move part-time worker and others to the last of the data frame.
  #count.num.of.job <- count.num.of.job %>% arrange(., school, department, -count)
  count.num.of.job <- unique(count.num.of.job)
  count.num.of.job <- count.num.of.job[order(count.num.of.job$school,count.num.of.job$department,-count.num.of.job$count),]
  count.num.of.job <- rbind(count.num.of.job[count.num.of.job$job!="工讀生" & count.num.of.job$job!="其他",],count.num.of.job[count.num.of.job$job=="工讀生" | count.num.of.job$job=="其他",])
  
  count.num.of.job <- count.num.of.job %>% group_by(school, department) %>% mutate(., percentage=count/sum(count))
  
  ##Get each departments of colleges first 10 jobs.
  #count.num.of.job <- count.num.of.job %>% group_by(school, department) %>% top_n(n = 10)
  cl <- makeCluster(2) ##2
  registerDoSNOW(cl)
  tmp.uni <- count.num.of.job[,c("school", "department")] %>% unique
  #pb <- txtProgressBar(min = 1, max = nrow(tmp.uni), style = 3)
  print("Output data format processing...")
  
  dopar.num.of.job <- foreach (x = 1:nrow(tmp.uni), .combine=rbind) %dopar% {
    tmp <- head(count.num.of.job[which(count.num.of.job$school==tmp.uni$school[x] & count.num.of.job$department==tmp.uni$department[x]),], 10)
    if(nrow(tmp)<10){
      tmp <- rbind(tmp, setNames(data.frame(matrix(rep(c(tmp$school[1], tmp$department[1], NA, NA, NA),11-nrow(tmp)), byrow=T, ncol=5)),names(tmp)))    
    }else{
      tmp <- rbind(head(tmp,10), unlist(c(tmp[1,1:2],"其他", (tmp[1,4]/tmp[1,5]*(1-sum(tmp$percentage))), (1-sum(tmp$percentage))))) 
    }
    #setTxtProgressBar(pb, x) 
    return(tmp)
  }
  #close(pb)
  stopCluster(cl)
  
  dopar.num.of.job[which(dopar.num.of.job$job=="工讀生"),c("job", "count", "percentage")] <- c(NA, NA, NA)
  
  return(dopar.num.of.job)
}

##tmp <- job_func(1)
##write.csv(tmp, "output/各校就業狀況-產業-2.csv",row.names=F)

total.output.lst <- list()
for(x in 1:4){
  total.output.lst[[x]] <- job_func(x,0)
}
write.csv(total.output.lst[[1]],"test.csv",row.names=F)

