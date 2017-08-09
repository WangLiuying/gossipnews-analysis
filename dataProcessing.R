body_length <- as.vector(sapply(df.merge$body,nchar))
df.merge$body_length <- body_length
length_interval <- character(length(df.merge$time))
length_interval[which(body_length<1000)] <- "[0,1000)"
length_interval[which(body_length>=1000&body_length<2000)] <- "[1000,2000)"
length_interval[which(body_length>=2000&body_length<3000)] <- "[2000,3000)"
length_interval[which(body_length>=3000&body_length<4000)] <- "[3000,4000)"
length_interval[which(body_length>=4000&body_length<5000)] <- "[4000,5000)"
length_interval[which(body_length>=5000)] <- "[5000,inf)"
length_interval
length_interval <- as.factor(length_interval)
str(length_interval)
df.merge$length_interval <- length_interval

mean_length <- tapply(df.merge$body_length,df.merge$account,mean)
prop_advertisement <- tapply(df.merge$advertisement,df.merge$account,mean)
max_times <- tapply(df.merge$time,df.merge$account,max)
min_times <- tapply(df.merge$time,df.merge$account,min)
time_range <- as.integer(max_times-min_times)
article_numbers <- tapply(df.merge$title,df.merge$account,length)
freq <- article_numbers/time_range
account_info <- data.frame(freq,mean_length,prop_advertisement)

con <- file("topstars.txt", encoding = "UTF-8")
topsinfo <- readLines(con)
close(con)
top50_stars <- topsinfo[seq(2,150,by=3)]


?sapply
sum(stars)
stars <- integer(length(df.merge$title))
for (i in seq_along(stars)){
  stars[i] <-  sum( str_detect(df.merge$body[i],top50_stars))
}
topstars <- stars!=0
df.process$topstars <- topstars
save(df.process,account_info,top50_stars,file="data_processing.RData")
