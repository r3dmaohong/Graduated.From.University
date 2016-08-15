
new_df <- read.csv(file.choose(),stringsAsFactors=F)
old_df <- read.csv(file.choose(),stringsAsFactors=F)

names(new_df)
new_df <- new_df[,c("學校名稱","科系名稱.藍字.105新增.綠字.改名.橘字.合併.紅字.停招.")]
names(old_df)
old_df <- old_df[,c("學校名稱","科系名稱")]
old_df <- unique(old_df)
old_df$模糊比對結果 <- ""

for(i in 1:nrow(old_df)){
  old_df$模糊比對結果[i] <- new_df$科系名稱[which(new_df$學校名稱==old_df$學校名稱[i])][which.max(1-stringdist(old_df$科系名稱[i],new_df$科系名稱[which(new_df$學校名稱==old_df$學校名稱[i])] ,method='jw'))]
  cat("\r",i/nrow(old_df)*100)
}