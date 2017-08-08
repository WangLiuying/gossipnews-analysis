##get games

library(rvest)
wangyou <- read_html("http://www.17173.com/zq/all.shtml")%>%
  html_nodes(css=".link") %>% html_text()

urllist <- str_c("http://top.17173.com/list-2-0-4-0-0-0-0-0-0-0-",1:32,".html",sep="")
shouyou <- c()
for (url in urllist)
{
  page <- read_html(url)
  game.shou <- page %>% html_nodes(".con>a") %>% html_text() %>% 
  str_replace_all(pattern="\n            ",replacement="") %>%
  str_replace_all(pattern="          ",replacement="")
  shouyou <- c(shouyou,game.shou)
}

urllist <- str_c("http://top.yeyou.com/?page=1",1:30,sep="")
yeyou <- c()
for (url in urllist)
{
  page <- read_html(url)
  game.shou <- page %>% html_nodes(".td1>span>a") %>% html_text()
  yeyou <- c(yeyou,game.shou)
}
yeyou

save(w)