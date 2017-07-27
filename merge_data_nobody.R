##merge data
#items: account,title,read,like,original,-body

library(stringr)

account.list <- c("dsmovie .RData","iiiher .RData","yansubagua .RData",
                  "gossipmaker .RData","realmovie520 .RData","shenyebagua818 .RData")


df.merge <- data.frame()
for (a in account.list)
{
  account <- strsplit(a,split=" .")[[1]][1]
  load(a)
  df$account <- account
  df$body <- NULL
  time_tru <- which(df$time=="2016-03-31")
  time_tru <- time_tru[length(time_tru)]
  df <- df[1:time_tru,]
  df.merge <- rbind(df.merge,df)
}

save(df.merge,file="no_body.RData")