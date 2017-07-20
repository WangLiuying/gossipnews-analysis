
library(rvest)
library(stringr)

#df <- data.frame()

##处理超时问题，休息10分钟后重试
readingURL <- function(link)
{
  link_page <- tryCatch(read_html(link),
                        error=function(e){
                          print(e);
                          if(str_detect(e,pattern="504")) 
                          {Sys.sleep(300);readingURL(link)}
                          else {return(read_html("<html><title>NA<title></html>"))}
                        })
  return(link_page)
}

#################################################################
#设置:统一从2月中旬开始吧
account <- "iiiher"
  page <- seq(1608,1800,by=12)

  (url <-
    paste("http://chuansong.me/account/",
          account,
          "?start=",
          page,
          sep = ""))
##################################################################  
  for (i in seq_along(url))
  {
    #url_page <- read_html(url[i])
    url_page <- readingURL(url[i])
    links <- url_page %>% html_nodes(".question_link") %>%
      html_attr("href")
    cat("page",url[i],"\n")
    Sys.sleep(runif(1,5,15))
    for (link in links)
    {
      link <- paste("http://chuansong.me", link, sep = "")
      link_page <- readingURL(link)
      #link_page <-read_html(link)
      body <- link_page %>% html_nodes("#js_content>*") %>% html_text()
      body <- body[nchar(body) > 3]
      body <- Reduce(f = paste, body)
      title <-
        link_page %>% html_node("#activity-name") %>% html_text() %>%
        str_replace_all(pattern = "[:blank:]|\\n", replacement = "")
      time <- link_page %>% html_node("#post-date") %>% html_text()
      hot <-
        link_page %>% html_nodes(".StatsRow>span>strong") %>% html_text()
      read <- hot[1]
      like <- as.numeric(hot[2])
      original <-
        link_page %>% html_node("#copyright_logo") %>% html_text()
      if(!is.na(title))
        {
        tryCatch(df <- rbind(df, data.frame(title, time, read, like, original,body)),
                     error=function(e){cat("fail in fetching this document.\n")})
        cat("获取：", title, "\n")
        Sys.sleep(runif(1,3,15))
        Sys.sleep(sample(x = c(0,0,0,0,1),size = 1)*20)
        }  
    }
    if(sample(c(rep(0,8),1),1)) {cat("sleep for 10 min\n");Sys.sleep(600)}
    save.image()
  }
save.image()
######################################################################  
save(df, file = paste(account, ".RData"))
View(df)



