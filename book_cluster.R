library(tm)
Sample_data<-read.csv('Path of csv file')
data_frame<- do.call('rbind', lapply(Sample_data, as.data.frame))


data_frame = data[["text"]]

myCorpus <- Corpus(VectorSource(data_frame))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus<- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, stripWhitespace)
dtm <- TermDocumentMatrix(myCorpus,control = list(minWordLength = 1))
dtm_tfxidf <- weightTfIdf(dtm)
m1 <- as.matrix(dtm_tfxidf)
m<-t(m1)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
num_cluster<-6
cl <- kmeans(m_norm, num_cluster)
round(cl$centers, digits = 1)
for (i in 1:num_cluster) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(cl$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
}

help("memory.size")
