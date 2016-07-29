rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

original_path <- getwd()

##Libraries
library(readxl)
library(dplyr)
library(RecordLinkage)
library(data.table)
options(java.parameters = "-Xmx4g")
library(XLConnect)
library(pbapply)

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
total.school.lookup.table <- read_excel("raw data\\105 科系職涯地圖-學系資料(進行中).xlsx",1)
industry.transform <- read_excel("raw data\\產業新類別vs舊類別.xlsx",1)

setwd(file.path('Graduated.From.University'))
dir.create('output', showWarnings = FALSE)

##Remove redundant curriculum vitae.
total.data$履歷編號 <- NULL
total.data <- unique(total.data)
nrow(total.data)

##Correct colleges' name.
college.name.transfer <- read.csv("學校名稱正規化表格.csv",stringsAsFactors=F)

##First job
first.job <- total.data[,c("學校代碼", "學校名稱", "科系名稱", "科系類別代號", 
                          "科系類別名稱", "產業小類代碼", "產業小類名稱", 
                          "職務小類代碼", "職務小類名稱")]
##Remove curriculum vitae without first job.
nrow(first.job)
first.job %>% filter(職務小類名稱!="") %>% nrow()
first.job <- first.job %>% filter(職務小類名稱!="")

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
##Correct departments' name
unique.college.department <- unique(first.job[,c("學校名稱", "科系名稱")])

for(x in 1:nrow(unique.college.department)){
  #jarowinkler('資管',c('資訊管理系','資訊工程學系'))
  ##check if having value in lookup table first.
  department.x <- total.school.lookup.table[which(unique.college.department$學校名稱[x] == total.school.lookup.table$學校名稱),"科系名稱（藍字=105新增；綠字=改名；橘字=合併；紅字=停招）"]
  department.x <- unlist(c(department.x))
  names(department.x)=NULL
  
  cat("\r", format(round(x/nrow(unique.college.department)*100,3),nsmall=3),"% ")
  
  if(length(department.x) > 0)
  {
    word <- unique.college.department$科系名稱[x]
    first.job$正規化科系名稱[first.job$學校名稱==unique.college.department$學校名稱[x] & first.job$科系名稱==unique.college.department$科系名稱[x]] = department.x[which.max(jarowinkler(word,department.x))[1]]
    cat(unique.college.department$科系名稱[x] , " ", department.x[which.max(jarowinkler(word,department.x))[1]],rep(" ",50))
  }else{
    first.job$error[first.job$學校名稱==unique.college.department$學校名稱[x] & first.job$科系名稱==unique.college.department$科系名稱[x]] <- 1
    print(sprintf("Index %s Error: %s %s", x, unique.college.department$學校名稱[x], unique.college.department$科系名稱[x]))
  }
}

##Remove data with error. (==1) 
nrow(first.job[first.job$error==0,])
first.job = first.job[first.job$error==0,]
first.job$error = NULL

##Counting job freq in each department of college.
tmp.first <- first.job[,c("學校名稱", "正規化科系名稱","職務小類名稱")]
names(tmp.first) <- c("school","department","job")
tmp.first <- tmp.first %>% group_by(., school, department, job)
count.first.job <- summarize(tmp.first,count=n()) 
count.first.job <- count.first.job %>% arrange(., school, department, -count)
#count.first.job <- count.first.job %>% mutate(., percentage=count/sum(count))

##Combine with old standard file.
standard.job <- read.csv("就業藍圖基準.csv",stringsAsFactors=F)
names(standard.job)
standard.job.first <- standard.job[standard.job$"類別.0.職務..1.產業")==0,c("學校名稱", "科系名稱", "名稱.一.", "樣本數.一.")]
names(standard.job.first) <- c("school","department","job","count")

count.first.job <- rbind(count.first.job,standard.job.first)
count.first.job <- count.first.job %>% group_by(., school, department, job)
count.first.job <- count.first.job[count.first.job$count!="NULL",]
#write.csv(count.first.job, "合併前查看.csv",row.names=F)
count.first.job <- count.first.job %>% mutate(., count=sum(as.numeric(count)))

##Move part-time worker and others to the last of the data frame.
count.first.job <- rbind(count.first.job[count.first.job$job!="工讀生" & count.first.job$job!="其他",],count.first.job[count.first.job$job=="工讀生" | count.first.job$job=="其他",])

##Get each departments of colleges first 10 jobs.
tmp.uni <- count.first.job[,c("school", "department")] %>% unique
for(x in 1:nrow(tmp.uni)){
  tmp <- count.first.job[which(count.first.job$school==tmp.uni$school[x] & count.first.job$department==tmp.uni$department[x]),] %>% head(.,10)
  tmp <- rbind(head(tmp,10), unlist(c(tmp[1,1:2],"其他", (tmp[1,4]/tmp[1,5]*(1-sum(tmp$percentage))), (1-sum(tmp$percentage)))))
  
  if(x==1)
    total.tmp <- tmp
  else
    total.tmp <- rbind(total.tmp,tmp)
  
  cat("\r",format(round(x/nrow(tmp.uni)*100,2),nsmall=2)," % ")
}

##To be continued...

write.csv(total.tmp, "各校就業狀況-職務.csv",row.names=F)
