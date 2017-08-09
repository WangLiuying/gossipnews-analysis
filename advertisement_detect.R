##advertisement extract

library(jiebaR)
library(stringr)

load("with_body.RData")
body <- df.merge$body
body <- lapply(body,str_trunc,800,"left")


jieba <- worker(type="mix",user = "./dictionary/newbrands.txt",stop=STOPPATH,bylines = T)
body.seg <- segment(unlist(body),jiebar = jieba)
brands <- readLines(con = "./dictionary/newbrands.txt",encoding = "UTF-8")

library(text2vec)
it <- itoken(iterable = body.seg)
vocab <- create_vocabulary(it)
vocab <- vocab$vocab$terms
stopwords <- vocab[!(vocab %in% brands)]
stopwords[1:10]

stop <- readLines(STOPPATH,encoding="UTF-8")
fileopen <- file("./dictionary/notbrands.txt",open="w",encoding = "UTF-8")
writeLines(c(stop,stopwords),con=fileopen)
close(fileopen)

jieba <- worker(type="mix",user="./dictionary/newbrands.txt",stop="./dictionary/notbrands.txt",bylines = T )
body.brand <- segment(unlist(body),jiebar = jieba)
it <- itoken(iterable = body.brand)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab,term_count_min = 2)
vec <- vocab_vectorizer(vocab)
corpus <- create_corpus(it,vec)
dtm <- corpus$get_dtm()
mybrand <- which(apply(dtm>2,2,sum)!=0)
mybrand <- names(mybrand)


fileopen <- file("./dictionary/mybrands.txt",open="w",encoding = "UTF-8")
writeLines(mybrand,con=fileopen)
close(fileopen)

###############################################################################
#分割线以上均在整理调整品牌词
##############################################################################
#分割线以下我们要正式提取品牌词了！！！

jieba <- worker(type="mix",user = "./dictionary/mybrands.txt",stop=STOPPATH,bylines = T)
body.brand <- segment(unlist(body),jiebar = jieba)
brands <- readLines(con = "./dictionary/mybrands.txt",encoding = "UTF-8")

library(text2vec)
it <- itoken(iterable = body.brand)
vocab <- create_vocabulary(it)
vocab <- vocab$vocab$terms
stopwords <- vocab[!(vocab %in% brands)]
stopwords[1:10]

stop <- readLines(STOPPATH,encoding="UTF-8")
fileopen <- file("./dictionary/notbrands.txt",open="w",encoding = "UTF-8")
writeLines(c(stop,stopwords),con=fileopen)
close(fileopen)

#####extract

jieba <- worker(type="mix",user="./dictionary/mybrands.txt",stop="./dictionary/notbrands.txt",bylines = T )
body.brand <- segment(unlist(body),jiebar = jieba)
it <- itoken(iterable = body.brand)
brands <- readLines("./dictionary/mybrands.txt",encoding="UTF-8")
vocab <- create_vocabulary(it)
stop <- vocab$vocab$terms[!(vocab$vocab$terms %in% brands)]
vocab <- create_vocabulary(it,stopwords = stop)
vocab$vocab$terms
vect <- vocab_vectorizer(vocab)
corpus <- create_corpus(it,vect)
dtm <- corpus$get_dtm()



advjudge <- (apply(dtm,1,sum)>1)
advindex <- which(advjudge>0)

test <- sample(advindex,size=1)
body.brand[test]

file <- file("text.txt",open="w",encoding="UTF-8")
writeLines(body[[test]],con=file)
close(file)

#vocab <- create_vocabulary(it,stopwords=stopwords)
#notbrands <- vocab$vocab$terms
# termfreq <- vocab$vocab$terms_counts/vocab$vocab$doc_counts

df.merge$advertisement <- (advjudge>0)
save(df.merge,file="with_body.RData")


load("no_body.RData")
df.merge$advertisement <- (advjudge>0)
save(df.merge,file="no_body.RData")
