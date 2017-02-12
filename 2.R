
install.packages("Rserve")
library(Rserve)
run.Rserve()

install.packages("devtools")
library(devtools)


install_github('sentiment140', 'okugami79')
library(sentiment)

sentiment('I LOVE #Apple')
sentiment('I hate #Apple')



setwd ("C://Users/Darius/Desktop/sentiment140")
test.text = readLines("m_full.csv")
dat = read.csv("m_full.csv", header = TRUE)
text = dat[["text"]] 

#analysis = sentiment(test.text)
#write.table(analysis, file = "sentiment_micro.csv")
#print (analysis)

#tmp <- read.csv("original_file.csv")
#tmp <- cbind(tmp, new_column)
#write.csv(tmp, "modified_file.csv")

analysis = sentiment(text)
polarity = analysis["polarity"]

tmp1 <- cbind(dat, polarity)



