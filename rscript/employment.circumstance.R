rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

original_path <- getwd()

##Libraries
library(readxl)
library(dplyr)
library(RecordLinkage)
library(data.table)
options(java.parameters = "-Xmx3g")
library(XLConnect)
library(pbapply)

##Import all files.
files <- list.files(file.path('raw data','�N�~�ŹϩҦ��Ǯխ�l���'),full.names = TRUE)
files.lists <- pblapply(files,function(file.name){
  ##excel_sheets(file.name) ##Get all sheets' name
  #readWorksheet(loadWorkbook(file.name), sheet = x, header = TRUE)
  wb <- loadWorkbook(file.name)
  file.list <- readWorksheet(wb, sheet = getSheets(wb)) ##Read all sheets in a xlsx file as a list.
  file.list <- do.call(rbind,file.list) ##Turn the list into a data frame.
  return(file.list)
})
total.data <- do.call(rbind,files.lists)
rm(files.lists,files)
##save the environment.
##save.image("D:/abc/wjhong/projects/Graduated.From.University/import.complete.RData")
##load("D:/abc/wjhong/projects/Graduated.From.University/import.complete.RData")
total.school.lookup.table <- read_excel("raw data\\105 ��t¾�P�a��-�Ǩt���(�i�椤).xlsx",1)
industry.transform <- read_excel("raw data\\���~�s���Ovs�����O.xlsx",1)

setwd(file.path('Graduated.From.University'))
dir.create('output', showWarnings = FALSE)

##total.data
total.data$�i���s�� <- NULL
total.data <- unique(total.data)
nrow(total.data)

college.name.transfer <- read.csv("�ǮզW�٥��W�ƪ���.csv",stringsAsFactors=F)

##First job
first.job <- total.data[,c("�ǮեN�X", "�ǮզW��", "��t�W��", "��t���O�N��", 
                          "��t���O�W��", "���~�p���N�X", "���~�p���W��", 
                          "¾�Ȥp���N�X", "¾�Ȥp���W��")]
nrow(first.job)
first.job %>% filter(¾�Ȥp���W��!="") %>% nrow()
first.job <- first.job %>% filter(¾�Ȥp���W��!="")

##Correct colleges' name
uni.college.names <- unique(first.job$�ǮզW��)
for(x in 1:length(uni.college.names)){
  tmp <- ifelse(toString(rev(sort(college.name.transfer$������[college.name.transfer$trim���l==uni.college.names[x]]))[1])!="" & toString(rev(sort(college.name.transfer$������[college.name.transfer$trim���l==uni.college.names[x]]))[1])!="NA"
         , rev(sort(college.name.transfer$������[college.name.transfer$trim���l==uni.college.names[x]]))[1]
         , uni.college.names[x])
  cat("\r" , format(round(x/length(uni.college.names)*100,3),nsmall=3),"% ")
  if(tmp!=uni.college.names[x]){
    first.job$�ǮզW��[first.job$�ǮզW��==uni.college.names[x]] <- tmp
    
    cat(uni.college.names[x], "==> ", tmp, rep(" ",50))
  }

}

first.job$���W�Ƭ�t�W�� <- ""
first.job$error <- 0

unique.college.department <- unique(first.job[,c("�ǮզW��", "��t�W��")])

for(x in 1:nrow(unique.college.department)){
  #jarowinkler('���',c('��T�޲z�t','��T�u�{�Ǩt'))
  ##check if having value in lookup table first.
  department.x <- total.school.lookup.table[which(unique.college.department$�ǮզW��[x] == total.school.lookup.table$�ǮզW��),"��t�W�١]�Ŧr=105�s�W�F��r=��W�F��r=�X�֡F���r=���ۡ^"]
  department.x <- unlist(c(department.x))
  names(department.x)=NULL
  
  cat("\r", format(round(x/nrow(unique.college.department)*100,3),nsmall=3),"% ")
  
  if(length(department.x) > 0)
  {
    word <- unique.college.department$��t�W��[x]
    first.job$���W�Ƭ�t�W��[first.job$�ǮզW��==unique.college.department$�ǮզW��[x] & first.job$��t�W��==unique.college.department$��t�W��[x]] = department.x[which.max(jarowinkler(word,department.x))[1]]
    cat(unique.college.department$��t�W��[x] , " ", department.x[which.max(jarowinkler(word,department.x))[1]],rep(" ",50))
  }else{
    first.job$error[first.job$�ǮզW��==unique.college.department$�ǮզW��[x] & first.job$��t�W��==unique.college.department$��t�W��[x]] <- 1
    print(sprintf("Index %s Error: %s %s", x, unique.college.department$�ǮզW��[x], unique.college.department$��t�W��[x]))
  }
}

nrow(first.job[first.job$error==0,])
first.job = first.job[first.job$error==0,]
first.job$error = NULL

tmp.first <- first.job[,c("�ǮզW��", "���W�Ƭ�t�W��","¾�Ȥp���W��")]
names(tmp.first) <- c("school","department","job")
tmp.first <- tmp.first %>% group_by(., school, department, job)
count.first.job <- summarize(tmp.first,count=n()) 
count.first.job <- count.first.job %>% arrange(., school, department, -count)
#count.first.job <- count.first.job %>% mutate(., percentage=count/sum(count))

##combine
standard.job <- read.csv("�N�~�Źϰ��.csv",stringsAsFactors=F)
names(standard.job)
standard.job.first <- standard.job[,c("�ǮզW��", "��t�W��", "�W��.�@.", "�˥���.�@.")]
names(standard.job.first) <- c("school","department","job","count")

count.first.job <- rbind(count.first.job,standard.job.first)
count.first.job <- count.first.job %>% group_by(., school, department, job)
count.first.job <- count.first.job[count.first.job$count!="NULL",]
#write.csv(count.first.job, "�X�֫e�d��.csv",row.names=F)
count.first.job <- count.first.job %>% mutate(., count=sum(as.numeric(count)))

count.first.job <- rbind(count.first.job[count.first.job$job!="�uŪ��" & count.first.job$job!="��L",],count.first.job[count.first.job$job=="�uŪ��" | count.first.job$job=="��L",])

tmp.uni <- count.first.job[,c("school", "department")] %>% unique
for(x in 1:nrow(tmp.uni)){
  tmp <- count.first.job[which(count.first.job$school==tmp.uni$school[x] & count.first.job$department==tmp.uni$department[x]),] %>% head(.,10)
  tmp <- rbind(head(tmp,10), unlist(c(tmp[1,1:2],"��L", (tmp[1,4]/tmp[1,5]*(1-sum(tmp$percentage))), (1-sum(tmp$percentage)))))
  
  if(x==1)
    total.tmp <- tmp
  else
    total.tmp <- rbind(total.tmp,tmp)
  
  cat("\r",format(round(x/nrow(tmp.uni)*100,2),nsmall=2)," % ")
}

write.csv(total.tmp, "�U�մN�~���p-¾��.csv",row.names=F)