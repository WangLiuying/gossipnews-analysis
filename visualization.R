###############################
##明星词典
###############################
for (p in 1:702){
	starurl<-paste("http://www.yoka.com/dna/star/item------------",p,".html",sep = "")
	starpg<-getURL(starurl, .encoding="UTF-8")
	starpg=strsplit(starpg,"\n")[[1]]
	star=starpg[grep("<div class=\"autocont clearfix\">",starpg)+c(1:350)]
	star=star[grep("<a href=\"/dna/star/",star)+2]
	star<-str_trim(star)
	star=unlist(str_split(star,"</a>"))
	star=star[str_length(star)>0]
	write.table(star,"明星.txt",col.names = F,append = T,row.names = F)
	cat("当前为第",p,"页")
	Sys.sleep(3)
}
###############################
##样本描述
###############################
#####1.拆分得到的主题
library(stringr)
str(df.merge)
body <- df.merge$body
str(df.merge)
df.merge$topics <- factor(df.merge$topics,levels=1:9,labels=c("电影评论","港台娱乐圈","粉丝八卦爆料",
                                                              "歌唱综艺节目","时尚穿搭","明星婚姻",
                                                              "豪门风云","家庭亲子生活","演员演艺点评"))
load("data_processing.RData")
df.merge$topstar <- df.process$topstars
rm(df.process)
#######2.明星、时间、点赞、阅读量
#明星
library(jiebaR)
workdevice1<-worker(user = "dictionary/starname2.txt",stop_word = "dictionary/chinese_stopword.txt",bylines = T,topn = 5)
seg_document<- segment(unlist(body),workdevice1)
star <- readLines("dictionary/starname2.txt",encoding="UTF-8")
library(text2vec)
it <- itoken(seg_document)
vocab <- create_vocabulary(it)
stopwords <- vocab$vocab$terms[!(vocab$vocab$terms %in% star)]
vocab.star <- create_vocabulary(it,stopwords = stopwords);rm(stopwords)

vect <- vocab_vectorizer(vocab.star)
corpus <- create_corpus(it,vect)
dtm.star <- corpus$get_dtm()
docstar <- vocab.star$vocab$terms[apply(dtm.star,1,which.max)]
docstar[apply(dtm.star,1,sum)==0] <- NA
df.merge$docstar <- docstar
str(df.merge)
save(df.merge,file="no_body.RData")


#公众号发文密度
library(ggplot2)
ggplot(df.merge,mapping = aes(x=time,y=account,alpha=0.1))+
  geom_point(position="jitter")+aes(colour=account,fill=account)+
  labs(x = "时间",y = "公众号",title="公众号发文时间密度")+
  guides(colour="none",alpha="none",size="none",fill="none")+theme_light()+scale_color_brewer(palette="Spectral")

##空缺情况：休刊，黄金周休假
ggplot(df.merge,mapping = aes(x=account,fill=account))+
  geom_bar()+
  labs(x = "公众号",y = "频数",title="公众号发文总量")+
  guides(fill="none")+theme_light()+scale_fill_brewer(palette="Spectral")
account.total <- table(df.merge$account)


#公众号最青睐明星
topstar <- vocab.star$vocab$terms[order(vocab.star$vocab$doc_counts,decreasing = T)]
topstar <- data.frame(star=topstar,doc_counts=vocab.star$vocab$doc_counts[order(vocab.star$vocab$doc_counts,decreasing = T)])

library(wordcloud2)
wordcloud2(topstar,size=0.5)
dotchart(x=topstar$doc_counts[30:1],labels=topstar$star[30:1],main="公众号最青睐明星top30")

str(df.merge)
# df.merge$topics <- factor(df.merge$topics,levels=1:9,labels=c("电影评论","港台娱乐圈","粉丝八卦爆料",
#                                                      "歌唱综艺节目","时尚穿搭","明星婚姻",
#                                                      "豪门风云","家庭亲子生活","演员演艺点评"))
# 1.电影评论
# 2.港台娱乐圈
# 3.粉丝八卦爆料
# 4.歌唱综艺节目
# 5.时尚穿搭
# 6.明星婚姻八卦
# 7.豪门风云
# 8.家庭亲子生活
# 9.演员演艺点评

##1.每个时间段报道了哪些主题
####picture 1
ggplot(df.merge,mapping = aes(x=time,y=topics,alpha=1))+
  geom_point(position="jitter")+aes(colour=topics)+
  labs(x = "时间",y = "话题",title="各主题报道时间密度")+
  guides(colour="none",alpha="none")+theme_light()+scale_color_brewer(palette="Spectral")
  
####主题报道数
ggplot(df.merge,mapping = aes(x=topics,fill=account))+
  geom_bar()+
  labs(x = "主题",y = "频数",title="各主题报道数")+
  guides(fill=guide_legend(title="公众号"))+theme_light()+scale_fill_brewer(palette="Spectral")


topic.total <- as.data.frame(table(df.merge$topics))
topic.total$freeq <- topic.total$Freq/sum(topic.total$Freq)
topic.total
ggplot(data=topic.total,mapping=aes(x=1,y=freeq,fill=Var1))+
  geom_col(position=position_fill(reverse = T))+
  geom_text(y=(cumsum(topic.total$freeq)+cumsum(c(0,topic.total$freeq[-9])))/2,
            x=0.9,label=round(topic.total$freeq,2))+
  geom_text(y=(cumsum(topic.total$freeq)+cumsum(c(0,topic.total$freeq[-9])))/2,
            x=1.25,label=topic.total$Var1,cex=3)+
  coord_polar(theta="y",direction = 1)+labs(x="",y="",title="各大话题占比")+
  guides(fill="none")+theme_light()+
  theme(axis.text.y =element_blank())+scale_fill_brewer(palette="Spectral")
#经常被报道的主题：5（时尚穿搭）、3（粉丝八卦爆料）、8（家庭亲子生活）、9（演员演艺点评）
#各公众号热衷的题材
topic_in_account <- as.data.frame(table(df.merge$topics,df.merge$account))
topic_in_account$freqq <-topic_in_account$Freq/rep(account.total,each=9) 
ggplot(data=topic_in_account,mapping=aes(x=Var1,y=freqq,fill=Var1))+geom_col()+
  #coord_polar(theta="x",direction=1)+
  facet_wrap(~Var2,nrow=2,ncol=5)+guides(fill="none")+
  labs(x="话题",y="频率",title="各公众号热衷题材")+
  theme_light()+theme(axis.text.x = element_text(angle=90))+scale_fill_brewer(palette="Spectral")

#公众号最青睐明星
library(text2vec)
topstar <- prune_vocabulary(vocab.star,doc_proportion_min = 0.028)
topstar <- topstar$vocab
gender <-c(2,2,1,1,2,2,2,2,2,1,2,2,2,1,2,1,2,1,1,2,1,1,2,2,1,2,2,1,2,1,1,2,2,1,1,2,1,1,2,2,1,2,2,2,1,2,2,2,2) 
color <- rep(gender,times=topstar$doc_counts %/% 100)
topstar <- rep(topstar$terms,times=topstar$doc_counts %/% 100)

ggplot(mapping=aes(x=topstar,fill=as.factor(color),colour=as.factor(color)))+
  geom_dotplot(dotsize=0.5,stackratio = 2)+coord_flip()+
  labs(x="明星",y="曝光次数",title="公众号最青睐明星top50")+
  theme_light()+guides(fill="none",colour="none")+theme(axis.text.x  = element_blank())+
  theme(axis.text.y=element_text(size=7))+
  scale_fill_brewer(palette = 5)#+scale_color_brewer(palette=5)



##3.主题与明星
topstar <- unique(topstar)
topic_star <- df.merge[,c("account","topics","docstar")]
head(topic_star)
topic_star[!(topic_star$docstar %in% topstar),"docstar"] <- NA
topic_star <- na.omit(topic_star)
ggplot(topic_star,mapping=aes(x=topics,y=docstar))+geom_bin2d()+theme_light()+
  labs(x="话题",y="明星",title="话题-明星热度")+
  scale_fill_continuous(low = "white",high="red")+
  theme(panel.grid = element_blank(),axis.text.y=element_text(size=7))+
  guides(fill=guide_legend(title = "频数"))
#带货女王杨幂，婚姻八卦宝强+杨幂，家庭亲子生活某渣男
df.merge$title[which(df.merge$docstar=="文章")]
#粗略看了一下，频频出现在家庭生活经营鸡汤类中，hhhh文章电影发布会紧张大哭：我演爱情戏可能没人相信
#另外文章的名字太坑爹，有一些误判的


##发文特征刻画:周末分布
df.merge$weekday <- weekdays(df.merge$time)
weekday.doc <- table(df.merge$account,df.merge$weekday)
weekday.doc <- weekday.doc[,c(7,1,4,5,6,2,3)]
weekday.doc <- prop.table(weekday.doc,margin = 1)
weekday.doc <- as.data.frame(weekday.doc)
ggplot(data=weekday.doc,mapping=aes(x=1,y=Freq,fill=Var2))+geom_col()+
  coord_polar(theta="y",direction=1)+
  facet_wrap(~Var1,nrow=2,ncol=5,shrink = T)+#guides(fill="none")+
  labs(x="",y="",title="每周发文时间")+
  theme_light()+scale_fill_brewer(palette="Spectral")+
  theme(axis.text.y = element_blank())+guides(fill=guide_legend(title="每周"))


#广告占比
adv.doc <- table(df.merge$account,df.merge$advertisement)
adv.doc <- prop.table(adv.doc,margin = 1)
adv.doc <- as.data.frame(adv.doc)
ggplot(data=adv.doc,mapping=aes(x=1,y=Freq,fill=Var2))+geom_col()+
  coord_polar(theta="y",direction=1)+
  facet_wrap(~Var1,nrow=2,ncol=5,shrink = T)+#guides(fill="none")+
  labs(x="",y="",title="广告发布占比")+
  theme_light()+scale_fill_brewer(palette=3)+
  theme(axis.text.y =element_blank(),axis.ticks.y = element_blank(),
        axis.text.x=element_text(size=5))+
  guides(fill=guide_legend(title="是否广告"))
#一年中的发文频率变动
# ggplot(df.merge,mapping = aes(x=time,colour=account))+geom_density(bw=7,trim=T)+
#   scale_color_brewer(palette="Spectral")+theme_light()+
#   labs(x="时间",y="发文频率",title="发文频率变动")
#不如散点图来的好
#一年中的广告频率变动
df.merge$adv <- df.merge$time
df.merge[!df.merge$advertisement,"adv"] <- NA
ggplot(df.merge,mapping = aes(x=adv,colour=account))+geom_density(bw=7,trim=T)+
  scale_color_brewer(palette="Spectral")+theme_light()+
  labs(x="时间",y="广告发布频率",title="广告发布频率变动")#同样感觉不如散点图

ggplot(df.merge,mapping = aes(x=adv,y=account,alpha=0.1))+
  geom_point()+aes(colour=account,fill=account)+
  labs(x = "时间",y = "公众号",title="广告发布时间密度")+
  guides(fill="none",colour="none",alpha="none",size="none")+
  theme_light()+scale_color_brewer(palette="Spectral")


#热度分析
table(df.merge$account,df.merge$read)

hot <- data.frame(time=unique(df.merge$time))
for (account in unique(df.merge$account))
{
 
  hot.temp <- as.integer(df.merge[df.merge$account==account,"read"])
  hot.temp[hot.temp==4] <- 12.5
  hot.temp[hot.temp==3] <- 7.5
  hot.temp[hot.temp==2] <- 3
  hot.temp[hot.temp==1] <- 0.5
  hot.temp<- tapply(hot.temp,
                         INDEX=df.merge[df.merge$account==account,"time"],FUN=sum)
  hot.temp.df <- data.frame(time=as.Date(names(hot.temp)))
  hot.temp.df[[account]] <- hot.temp
  hot <- merge(x=hot,y=hot.temp.df,by="time",all.x = T)
}
dim(hot)
summary(hot)
library(data.table)
hot <- melt(hot,id.vars=c("time"),variable.name="account")
ggplot(data=hot,mapping=aes(x=time,y=value,color="lightblue",alpha=0.05))+geom_point(position="jitter")+
  geom_smooth(se = F,span=0.5,color="blue")+guides(alpha="none",colour="none")+
  theme_light()+facet_wrap(~account)+labs(x="时间",y="热度",title="每日阅读热度变化")+
  theme(axis.text.x = element_text(angle=90))

#暂放，没想好。
# table(df.merge$account,df.merge$read)
# 
# hot <- data.frame(time=unique(df.merge$time))
# for (account in unique(df.merge$account))
# {
#   
#   
#   hot.temp<- tapply(as.integer(df.merge[df.merge$account==account,"read"]),
#                     INDEX=df.merge[df.merge$account==account,"time"],FUN=sum,na.rm=T)
#   hot.temp.df <- data.frame(time=as.Date(names(hot.temp)))
#   hot.temp.df[[account]] <- hot.temp
#   hot <- merge(x=hot,y=hot.temp.df,by="time",all.x = T)
# }
# hot <- melt(hot,id.vars=c("time"),variable.name="account")
# hot[is.na(hot$value),"value"] <- 0
# hot.heatmap.time <- rep(x=hot$time,times=hot$value)
# hot.heatmap.account <- rep(x=hot$account,times=hot$value)
# ggplot(mapping=aes(x=hot.heatmap.time,y=hot.heatmap.account))+geom_()

#广告的热度变化
table(df.merge$account,df.merge$read)

hot <- data.frame(time=unique(df.merge$time))
for (account in unique(df.merge$account))
{
  
  
  hot.temp <- as.integer(df.merge[df.merge$account==account&df.merge$advertisement==T,"read"])
  hot.temp[hot.temp==4] <- 12.5
  hot.temp[hot.temp==3] <- 7.5
  hot.temp[hot.temp==2] <- 3
  hot.temp[hot.temp==1] <- 0.5
  hot.temp<- tapply(hot.temp,
                    INDEX=df.merge[df.merge$account==account&df.merge$advertisement==T,"time"],FUN=sum)
  hot.temp.df <- data.frame(time=as.Date(names(hot.temp)))
  hot.temp.df[[account]] <- hot.temp
  hot <- merge(x=hot,y=hot.temp.df,by="time",all.x = T)
}
dim(hot)
summary(hot)
library(data.table)
hot <- melt(hot,id.vars=c("time"),variable.name="account")
ggplot(data=hot,mapping=aes(x=time,y=value,color="lightblue",alpha=0.05))+geom_point(position="jitter")+
  geom_smooth(se = F,span=0.5,color="blue")+guides(alpha="none",colour="none")+
  theme_light()+facet_wrap(~account)+labs(x="时间",y="广告热度",title="广告每日阅读热度变化")+
  theme(axis.text.x = element_text(angle=90))

