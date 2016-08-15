if(no.job==1){
  num.of.job <- first.job[,c("�ǮեN�X", "�ǮզW��", "��t�W��", "��t���O�N��", 
                             "��t���O�W��", "���~�p���N�X", "���~�p���W��", 
                             "¾�Ȥp���N�X", "¾�Ȥp���W��")]
}else{
  num.of.job <- first.job[,c("�ǮեN�X", "�ǮզW��", "��t�W��", "��t���O�N��", 
                             "��t���O�W��", 
                             paste0(c("���~�p���N�X", "���~�p���W��", "¾�Ȥp���N�X",
                                      "¾�Ȥp���W��"),no.job-1))]
}
names(num.of.job) <- c("�ǮեN�X", "�ǮզW��", "��t�W��", "��t���O�N��", 
                       "��t���O�W��", "���~�p���N�X", "���~�p���W��", 
                       "¾�Ȥp���N�X", "¾�Ȥp���W��")


first.job %>% filter(¾�Ȥp���W��!="") %>% nrow()
first.job <- first.job %>% filter(¾�Ȥp���W��!="")

##Remove data with error. (==1) 
nrow(first.job[first.job$error==0,])
first.job = first.job[first.job$error==0,]
first.job$error = NULL

##Counting job freq in each department of college.
tmp.first <- first.job[,c("�ǮզW��", "���W�Ƭ�t�W��","¾�Ȥp���W��")]
names(tmp.first) <- c("school","department","job")
tmp.first <- tmp.first %>% group_by(., school, department, job)
count.first.job <- summarize(tmp.first,count=n()) 

##Combine with old standard file.
standard.job <- read.csv("�N�~�Źϰ��.csv",stringsAsFactors=F)
names(standard.job)
standard.job.first <- standard.job[standard.job$"���O.0.¾��..1.���~"==0,c("�ǮզW��", "��t�W��", "�W��.�@.", "�˥���.�@.")]
names(standard.job.first) <- c("school","department","job","count")

##Old names to new names
for(i in 1:nrow(job.transform)){
  standard.job.first$job[standard.job.first$job==job.transform$old[i]] <- job.transform$new[i]
}

count.first.job <- rbind(count.first.job,standard.job.first)
count.first.job <- count.first.job %>% group_by(., school, department, job)
count.first.job <- count.first.job[count.first.job$count!="NULL",]
#write.csv(count.first.job, "�X�֫e�d��2.csv",row.names=F)
count.first.job <- count.first.job %>% mutate(., count=sum(as.numeric(count)))

##Move part-time worker and others to the last of the data frame.
#count.first.job <- count.first.job %>% arrange(., school, department, -count)
count.first.job <- unique(count.first.job)
count.first.job <- count.first.job[order(count.first.job$school,count.first.job$department,-count.first.job$count),]
count.first.job <- rbind(count.first.job[count.first.job$job!="�uŪ��" & count.first.job$job!="��L",],count.first.job[count.first.job$job=="�uŪ��" | count.first.job$job=="��L",])

count.first.job <- count.first.job %>% group_by(school, department) %>% mutate(., percentage=count/sum(count))

##Get each departments of colleges first 10 jobs.
#count.first.job <- count.first.job %>% group_by(school, department) %>% top_n(n = 10)
cl <- makeCluster(2) ##2
registerDoSNOW(cl)
pb <- txtProgressBar(min = 1, max = nrow(tmp.uni), style = 3)
print("Output data format processing...")
tmp.uni <- count.first.job[,c("school", "department")] %>% unique
dopar.first.job <- foreach (x = 1:nrow(tmp.uni), .combine=rbind) %dopar% {
  tmp <- head(count.first.job[which(count.first.job$school==tmp.uni$school[x] & count.first.job$department==tmp.uni$department[x]),], 10)
  if(nrow(tmp)<10){
    tmp <- rbind(tmp, setNames(data.frame(matrix(rep(c(tmp$school[1], tmp$department[1], NA, NA, NA),11-nrow(tmp)), byrow=T, ncol=5)),names(tmp)))    
  }else{
    tmp <- rbind(head(tmp,10), unlist(c(tmp[1,1:2],"��L", (tmp[1,4]/tmp[1,5]*(1-sum(tmp$percentage))), (1-sum(tmp$percentage))))) 
  }
  setTxtProgressBar(pb, x) 
  return(tmp)
}
close(pb)
stopCluster(cl)

dopar.first.job[which(dopar.first.job$job=="�uŪ��"),c("job", "count", "percentage")] <- c(NA, NA, NA)

##To be continued...

write.csv(dopar.first.job, "output/�U�մN�~���p-¾��.csv",row.names=F)