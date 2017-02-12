setwd ("C://Users/Darius/Desktop/sentiment140/Final_duomenys/")

library(tm)
corpus = Corpus(VectorSource(neg$txt))
corpus <- tm_map(tm_map(tm_map(corpus,
                               removePunctuation), content_transformer(tolower)), function(x) removeWords(x, c(stopwords("english"),"hateful", "eight")))

tdMatrix <- as.matrix(TermDocumentMatrix(corpus))
tdMatrix <- sort(rowSums(tdMatrix), decreasing=T)
freqDF <- data.frame(words=names(tdMatrix), freq=tdMatrix)


corpus <- tm_map(corpus, content_transformer(tolower))


dtm <- DocumentTermMatrix(corpus)

dtm2 <- as.matrix(dtm)

frequency <- colSums(dtm2)

frequency <- sort(frequency, decreasing=TRUE)

----------------------------------------------------
test2 = final_hateful[final_hateful$lang %in% "en",]
idx = c(1,2,3, 4,5)
neg = test2[test2$negative %in% idx, ]

neg = neg[sample(nrow(neg), 100000), ]




reviews <- test2$txt
----------------------------------------------
# read in some stopwords:
library(tm)
a <- c("hateful","fuck", "shit", "ass", "sex", "fucking", "mocking", "jay", "eight")
stop_words <- (c(stopwords("SMART"),a))

# stop_words <- (stopwords("SMART"))
# stop_words <- readLines("stopwords.txt", encoding = "UTF-8")



reviews = neg$txt



# pre-processing:
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("[[:digit:]]", "", reviews) #skaiciukai
reviews <- gsub("[[:graph:]])", "", reviews) # blogus simbolius
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)



# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

#a = as.data.frame(vocab)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)


# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

# MCMC and model tuning parameters:
K <- 10 #pics def=20
G <- 5000 #itterations (def=5000)
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop// 17min hehe

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

#Lentele1 = cbind(doc.l, theta)

#nrow(theta)

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)


serVis(json, out.dir = 'pos', open.browser = T)
serVis(json, out.dir = 'neg', open.browser = FALSE)
------------------------------------------------
top.words <- top.topic.words(fit$topics, 9 , by.score=T)
-------------------------------------------

  out <- file("top.topic.words_neg.csv", "w", encoding="UTF-8") 
write.table(top.words, out, sep=",", row.names=FALSE) 
close(out) 

out <- file("phi_neg.csv", "w", encoding="UTF-8") 
write.table(phi, out, sep=",", row.names=FALSE) 
close(out) 

# out <- file("final_creed.csv", "w", encoding="UTF-8") 
# write.table(final_creed, out, sep=",", row.names=FALSE) 
# close(out) 


  
  

??serVis
??LDAvis
reviews
json
