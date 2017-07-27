##merge data
#items: account,title,read,like,original,-body

library(stringr)

account.list <- c("dsmovie .RData","iiiher .RData","yansubagua .RData",
                  "gossipmaker .RData","realmovie520 .RData","shenyebagua818 .RData")


df.merge <- data.frame()
for (a in account.list)
{
  #formatting
  account <- strsplit(a,split=" .")[[1]][1]
  load(a)
  df$title <- as.character(df$title)
  df$time <- as.Date(df$time)
  df$original <- ifelse(!is.na(df$original),1,0)
  df$body <- as.character(df$body)
  read <- as.character(df$read)
  read[which(str_detect(read,pattern="^\\d{4}$"))]=0
  read[which(str_detect(read,pattern="^(1|2|3|4|5)\\d{4}$"))]=1
  read[which(str_detect(read,pattern="^(6|7|8|9)\\d{4}$"))]=2
  read[which(str_detect(read,pattern="(10万+)|(^\\d{6}$)"))]=3
  read <- factor(read,levels = c(0,1,2,3),labels=c("[0,10k)","[10k,60k)","[60k,100k)","[100k,infty)"))
  df$read <- read
  df$account <- account
  #merge
  df$body <- NULL
  time_tru <- which(df$time=="2016-03-31")
  time_tru <- time_tru[length(time_tru)]
  df <- df[1:time_tru,]
  df.merge <- rbind(df.merge,df)
}

save(df.merge,file="no_body.RData")
