##title topics

#segment
library(jiebaR)
library(stringr)

load("no_body.RData")

title <- df.merge$title
jieba <- worker(user = "./dictionary/starname2.txt",
                stop_word = "./dictionary/chinese_stopword.txt")
jieba$bylines <- T

title.seg <- segment(code =unlist(title),jiebar = jieba)
title.seg[1:10]

kws <- worker("keywords",user="./dictionary/starname2.txt",
              stop_word = "./dictionary/chinese_stopword.txt",topn=200)
title.key <- vector_keywords(unlist(title.seg),jiebar=kws)
title.key
##topics

library(text2vec)
help(package="text2vec")
it <- itoken(iterable = title.seg)
vocab <- create_vocabulary(it)
vocab
vectorizer <- vocab_vectorizer(vocab)
corpus <- create_corpus(it,vectorizer)
dtm <- corpus$get_dtm()


lda <- LatentDirichletAllocation$new(20,vocabulary=vocab,doc_topic_prior=0.1,
                                     topic_word_prior=0.1)
doc_topic_distr <- lda$fit_transform(dtm,n_iter = 200)
topic_word_distr <- lda$get_word_vectors()
lda$plot()


##ldavis
library(LDAvis)
dim(doc_topic_distr)
doc_topic_distr <- t(apply(doc_topic_distr,1,function(x) {x/sum(x)}))
dim(topic_word_distr)
topic_word_distr <- t(apply(topic_word_distr,2,function(x) {x/sum(x)}))

jason <- createJSON(phi = topic_word_distr,theta=doc_topic_distr,
                    doc.length = apply(dtm,1,sum),
                    vocab=vocab$vocab$terms,term.frequency = vocab$vocab$terms_counts)




##wordcloud

library(wordcloud2)

wordfreq <- data.frame(title.key,
                       Freq=floor(as.numeric(names(title.key))))
wordcloud2(data=wordfreq,shape="star",size=0.3)
