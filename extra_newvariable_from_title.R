##title: new variables extract

#loading
library(text2vec)
library(jiebaR)
load('no_body.RData')

#stars in title
stars = readLines('dictionary/starname2.txt', encoding = 'UTF-8')
str(stars)

jiebar = worker(user = 'dictionary/starname2.txt', bylines = T)
divide = segment(df.merge$title, jiebar)
str(divide)

it = itoken(divide)
vocab = create_vocabulary(it)
stopword =  vocab$term[!vocab$term %in% stars]
stopword

vocab = create_vocabulary(it, stopwords = stopword)
vocab
vec = vocab_vectorizer(vocab)
dtm = create_dtm(it, vec)
dtm
ifstars = (apply(dtm, 1, sum) >= 1)
df.merge$ifstar = ifstars

#### delete star
vocab = create_vocabulary(it, stopwords = stars)
vocab
jiebar = worker(type = 'keywords',user = 'dictionary/topics.txt', stop_word = 'dictionary/chinese_stopword.txt',topn = 100)
findkeyword = rep(vocab$term, vocab$doc_count)
title_keyword = vector_keywords(findkeyword, jiebar)
title_keyword
