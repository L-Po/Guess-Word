library(tm)
library(SnowballC)  
library(ggplot2) 
library(RWeka)
library(wordcloud)
library(xtable)
library(knitr)
library(stringr)

Blogs <- readLines("E:/Original Files/en_US.blogs.txt")
News <- readLines("E:/Original Files/en_US.news.txt")
Twitter <- readLines("E:/Original Files/en_US.twitter.txt")

SampleBlogs <- Blogs[1:round(length(Blogs)/50, 0)]
SampleNews <- News[1:round(length(News)/50, 0)]
SampleTwitter <- Twitter[1:round(length(Twitter)/50, 0)]

cname <- file.path("E:", "texts") 
setwd(cname)

write(SampleBlogs, file = "SampleBlogs.txt")
write(SampleNews, file = "SampleNews.txt")
write(SampleTwitter, file = "SampleTwitter.txt")

path_name <- file.path ("E:", "capstone")
setwd(path_name)

docs <- Corpus(DirSource(cname))

Docs <- tm_map(docs, removePunctuation)
Docs <- tm_map(Docs, removeNumbers) 
Docs <- tm_map(Docs, tolower)
Docs <- tm_map(Docs, stripWhitespace) 
Docs <- tm_map(Docs, PlainTextDocument)

dtm <- DocumentTermMatrix(Docs)   

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm.ng <- DocumentTermMatrix(Docs, control = list(tokenize = BigramTokenizer))
freq2 <- sort(colSums(as.matrix(dtm.ng)), decreasing=TRUE)
wf2 <- data.frame(rank = c(1:length(freq2)), word=names(freq2), freq=freq2)


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm.3g <- DocumentTermMatrix(Docs, control = list(tokenize = TrigramTokenizer))
freq3 <- sort(colSums(as.matrix(dtm.3g)), decreasing=TRUE)
wf3 <- data.frame(rank = c(1:length(freq3)), word=names(freq3), freq=freq3) 


fourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm.4g <- DocumentTermMatrix(Docs, control = list(tokenize = fourgramTokenizer))
freq4 <- sort(colSums(as.matrix(dtm.4g)), decreasing=TRUE)
wf4 <- data.frame(rank = c(1:length(freq4)), word=names(freq4), freq=freq4)





wf2$word  <- as.character(wf2$word)
wf2$first <- word(wf2$word, 1)
wf2$last  <- word(wf2$word, -1)

wf3$word  <- as.character(wf3$word)
wf3$first <- word(wf3$word, 1, 2)
wf3$last  <- word(wf3$word, -1)

wf4$word  <- as.character(wf4$word)
wf4$first <- word(wf4$word, 1, 3)
wf4$last  <- word(wf4$word, -1)

write.csv(wf4, "Quadrigram table.csv")
write.csv(wf3, "Trigram table.csv")
write.csv(wf2, "Bigram table.csv")

wf2 <- read.csv("E:/capstone/Bigram table.csv")
wf3 <- read.csv("E:/capstone/Trigram table.csv")
wf4 <- read.csv("E:/capstone/Quadrigram table.csv")


wf2$X     <- NULL
wf2$word  <- NULL 
wf2       <- wf2[wf2$freq !=1, ]

wf3$X     <- NULL
wf3$word  <- NULL 
wf3       <- wf3[wf3$freq !=1, ]

wf4$X     <- NULL
wf4$word  <- NULL 
wf4       <- wf4[wf4$freq !=1, ]

setwd("E:/Guess Word App")

write.csv(wf4, "Quadrigram table2.csv")
write.csv(wf3, "Trigram table2.csv")
write.csv(wf2, "Bigram table2.csv")




