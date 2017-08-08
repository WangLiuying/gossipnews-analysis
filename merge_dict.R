library(dplyr)
library(stringr)
filelist <- dir("./dictionary/") %>% str_detect(pattern=".txt$")
filelist <- dir("./dictionary/")[filelist]
filelist <- filelist[c(9,11,13,15,16,17,18,19,21,23,16,1,5,4)]

setwd("./dictionary/")
brands <- file("newbrands.txt",open = "w+",encoding = "UTF-8")
for (each in filelist)
{
  bs <- readLines(each,warn=F,encoding = "UTF-8")
  bs <- str_replace_all(bs,pattern=" n$",replacement = "")
  writeLines(bs,con=brands)
}
close(brands)
filelist <- filelist[-c(16)]


brandswords<- readLines("newbrands.txt",encoding="UTF-8")
brandswords <- unique(brandswords)
brands <- file("newbrands.txt",open = "w+",encoding = "UTF-8")
writeLines(brandswords,con=brands)
close(brands)

