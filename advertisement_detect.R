##advertisement extract

library(jiebaR)
library(stringr)

load("with_body.RData")
body <- df.merge$body

jieba <- worker(type="mix",user = "./dictionary/brands.txt",stop=STOPPATH,bylines = T)
body.seg <- segment(unlist(body),jiebar = jieba)
brands <- readLines(con = "./dictionary/brands.txt",encoding = "UTF-8")

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

jieba <- worker(type="mp",user="./dictionary/brands.txt",stop="./dictionary/notbrands.txt",bylines = T )
body.brand <- segment(unlist(body),jiebar = jieba)

file <- file("text.txt",open="w",encoding="UTF-8")
writeLines(body[2],con=file)
close(file)

#vocab <- create_vocabulary(it,stopwords=stopwords)
#notbrands <- vocab$vocab$terms
# termfreq <- vocab$vocab$terms_counts/vocab$vocab$doc_counts

