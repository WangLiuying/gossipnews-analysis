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
ggplot(df.merge,mapping = aes(x=time,y=account,alpha=1))+
  geom_point(position="jitter")+aes(colour=account)+
  labs(x = "时间",y = "公众号",title="公众号发文时间密度")+
  guides(colour="none",alpha="none")+theme_light()#+scale_color_brewer(palette="Spectral")

##空缺情况：休刊，黄金周休假
ggplot(df.merge,mapping = aes(x=account,fill=account))+
  geom_bar()+
  labs(x = "公众号",y = "频数",title="公众号发文总量")+
  guides(fill="none")+theme_light()
account.total <- table(df.merge$account)


#公众号最青睐明星
topstar <- vocab.star$vocab$terms[order(vocab.star$vocab$doc_counts,decreasing = T)]
topstar <- data.frame(star=topstar,doc_counts=vocab.star$vocab$doc_counts[order(vocab.star$vocab$doc_counts,decreasing = T)])

library(wordcloud2)
wordcloud2(topstar,size=0.5)
dotchart(x=topstar$doc_counts[30:1],labels=topstar$star[30:1],main="公众号最青睐明星top30")

str(df.merge)
df.merge$topics <- factor(df.merge$topics,levels=1:9,labels=c("电影评论","港台娱乐圈","粉丝八卦爆料",
                                                     "歌唱综艺节目","时尚穿搭","明星婚姻",
                                                     "豪门风云","家庭亲子生活","演员演艺点评"))
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
  guides(fill=guide_legend(title="公众号"))+theme_light()


ggplot(mapping=aes(x=1,y=,fill=Var1))+geom_col()+
  #coord_polar(theta="x",direction=1)
  facet_wrap(~Var2,nrow=2,ncol=5)+guides(fill="none")+
  labs(x="话题",y="频数",title="各公众号热衷题材")+
  theme_light()+theme(axis.text.x = element_text(angle=90))

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
  theme(axis.text.y =element_blank())
#经常被报道的主题：5（时尚穿搭）、3（粉丝八卦爆料）、8（家庭亲子生活）、9（演员演艺点评）
#各公众号热衷的题材
topic_in_account <- as.data.frame(table(df.merge$topics,df.merge$account))
topic_in_account$freqq <-topic_in_account$Freq/rep(account.total,each=9) 
ggplot(data=topic_in_account,mapping=aes(x=Var1,y=freqq,fill=Var1))+geom_col()+
  #coord_polar(theta="x",direction=1)+
  facet_wrap(~Var2,nrow=2,ncol=5)+guides(fill="none")+
  labs(x="话题",y="频数",title="各公众号热衷题材")+
  theme_light()+theme(axis.text.x = element_text(angle=90))

#公众号最青睐明星
library(text2vec)
topstar <- prune_vocabulary(vocab.star,doc_proportion_min = 0.029)
topstar <- topstar$vocab
topstar <- rep(topstar$terms,times=topstar$doc_counts %/% 100)
ggplot(mapping=aes(x=topstar,fill=topstar,colour=topstar))+
  geom_dotplot(dotsize=0.5,stackratio = 2)+coord_flip()+
  labs(x="明星",y="曝光次数",title="公众号最青睐明星top50")+
  theme_light()+guides(fill="none",colour="none")+theme(axis.text.x  = element_blank())



##3.主题与明星
topstar <- unique(topstar)
topic_star <- df.merge[,c("account","topics","docstar")]
head(topic_star)
topic_star[!(topic_star$docstar %in% topstar),"docstar"] <- NA
topic_star <- na.omit(topic_star)
ggplot(topic_star,mapping=aes(x=topics,y=docstar))+geom_bin2d()+theme_light()+
  labs(x="话题",y="明星",title="话题-明星热度")+
  scale_fill_continuous(low = "white",high="red")+
  theme(panel.grid = element_blank())
#带货女王杨幂，婚姻八卦宝强+杨幂，家庭亲子生活某渣男
df.merge$title[which(df.merge$docstar=="文章")]
#粗略看了一下，频频出现在家庭生活经营鸡汤类中，hhhh文章电影发布会紧张大哭：我演爱情戏可能没人相信
#另外文章的名字太坑爹，有一些误判的


##发文特征刻画
df.merge$weekday <- weekdays(df.merge$time)
weekday.doc <- table(df.merge$account,df.merge$weekday)
weekday.doc <- weekday.doc[]
