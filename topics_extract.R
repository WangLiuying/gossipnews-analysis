##title topics

#segment
library(jiebaR)
library(stringr)

load("with_body.RData")

title <- df.merge$title
body <- df.merge$body
jieba <- worker(type="mix",user = "./dictionary/topics.txt",
                stop_word = "./dictionary/chinese_stopword.txt")
jieba$bylines <- T
jieba2 <- worker(user = "./dictionary/topics.txt",
                stop_word = STOPPATH,byline=T)
title.seg <- segment(code =unlist(title),jiebar = jieba2)
body.seg <- segment(code=unlist(body),jiebar=jieba)


kws <- worker("keywords",user="./dictionary/topics.txt",
              stop_word = "./dictionary/chinese_stopword.txt",topn=200)
title.key <- vector_keywords(unlist(title.seg),jiebar=kws)
title.key

# kws <- worker("keywords",user="./dictionary/topics.txt",
#               stop_word = "./dictionary/chinese_stopword.txt",topn=100)
# body.seg <- lapply(body.seg,vector_keywords,kws)

rm(df.merge,jieba,jieba2)
##wordcloud

library(wordcloud2)

wordfreq <- data.frame(title.key,
                       Freq=floor(as.numeric(names(title.key))))
wordcloud2(data=wordfreq,shape="star",size=0.3)

rm(title,body)
rm(wordfreq)
##topics

library(text2vec)
help(package="text2vec")
#it <- itoken(iterable = title.seg)
it <- itoken(iterable = body.seg)
vocab <- create_vocabulary(it)
vocab
vocab <- prune_vocabulary(vocab,term_count_min = 15,doc_proportion_min = 0.0015)

vectorizer <- vocab_vectorizer(vocab)
corpus <- create_corpus(it,vectorizer)
dtm <- corpus$get_dtm()
dim(dtm)


library(topicmodels)

# 
# K <- 5:15
# fold <- 5;n <- dim(dtm)[1]
# alpha=0.1;delta=0.1
# # perp <- data.frame()
# for (k in K)
# {
#   log.likelihood <- c()
#   foldedperp <- c()
#   for (f in 1:fold)
#   {
#     cat("training:k=",k,",fold=",f,"\n")
#     index <- sample(x=1:n,size=n%/%3)
#     tindex <- sample(x=setdiff(1:n,index),size=n%/%3)
#     corpus.loop <- as.matrix(dtm[index,])
#     testset <- as.matrix(dtm[tindex,])
#     gibbs <- LDA(corpus.loop,k,method="Gibbs",
#                  control = list(alpha=alpha,delta=delta,burnin=300,iter=200,seed=6646+k+f))
#     log.likelihood <- c(log.likelihood,gibbs@loglikelihood)
#     foldedperp <- c(foldedperp,perplexity(gibbs,testset))
#     
#   }
#   perp <- rbind(perp,data.frame(Num_topics=k,loglikelihood=mean(log.likelihood),perplexity=mean(foldedperp)))
#   save.image("lda_ver1.RData")
#   Sys.sleep(2)
# }
# 
# 
#likelihood
perp
library(ggplot2)
ggplot(data=perp,mapping=aes(x=Num_topics,y=loglikelihood))+geom_line()+
  geom_point()+labs(xlab="Num of topics",ylab="loglikelihood")
ggplot(data=perp,mapping=aes(x=Num_topics,y=perplexity))+geom_line()+
  geom_point()+labs(xlab="Num of topics",ylab="perplexity")



k=9##预计将在10~13左右，待数据完备再标注一次
alpha=0.1;delta=0.1
gibbs <- LDA(as.matrix(dtm),k,method="Gibbs",
             control = list(alpha=alpha,delta=delta,burnin=500,iter=500,seed=6646))
table(fit.topics <- topics(gibbs))#提出主题
(topwords <- terms(gibbs,20))#提出主题中最可能的关键词

#试着总结了一下不知有否不妥
#if k=10
# 1.家庭亲子生活
# 2.婚姻故事
# 3.红毯时尚穿搭
# 4.演员演艺评论
# 5.微博八卦爆料
# 6.明星自拍美图
# 7.娱乐公司事务八卦
# 8.豪门风云
# 9.电影评论
# 10.歌唱综艺节目

#if k=9
# 1.电影评论
# 2.港台娱乐圈
# 3.粉丝八卦爆料
# 4.歌唱综艺节目
# 5.时尚穿搭
# 6.明星婚姻八卦
# 7.豪门风云
# 8.家庭亲子生活
# 9.演员演艺点评




#plot
#doc-topic theta matrix(posterior)
theta <- gibbs@gamma
#topic-word phi matrix
phi <- exp(gibbs@beta)
doc.length <- apply(dtm,1,length)





library(LDAvis)
json <- createJSON(phi=phi,theta=theta,vocab=vocab$vocab$terms,
                   doc.length =doc.length,term.frequency = vocab$vocab$terms_counts)

dir.path <- "./LDAvis-gossip-version1"
serVis(json,out.dir = dir.path,open.browser=F)


writeLines(iconv(readLines(paste(dir.path,"/lda.json",sep="")), from = "GBK", to = "UTF8"),

           file(paste(dir.path,"/lda.json",sep=""), encoding="UTF-8"))



# # require:utf-8 encoding
# lda <- LatentDirichletAllocation$new(12,vocabulary=vocab,doc_topic_prior=0.1,
#                                      topic_word_prior=0.1)
# doc_topic_distr <- lda$fit_transform(dtm,n_iter = 2000)
# topic_word_distr <- lda$get_word_vectors()
# 
# lda$plot()

load("no_body.RData")
df.merge$topics <- fit.topics
save(df.merge,file = "no_body.RData")
rm(df.merge)
load("with_body.RData")
df.merge$topics <- fit.topics
save(df.merge,file="with_body.RData")
rm(df.merge)
