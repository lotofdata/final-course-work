# Example of using STM to perform topic modelling
# Load Libraries
library(stm)
library(stringr)

# Read in data
data <- read.csv("data.csv", sep="^", quote="")

# Read in stopwords
stopwords <- read.csv("all_stopwords.txt",header=F)
stopwords[] <- lapply(stopwords,as.character)
stopwords <- stopwords$V1

# Process meta data
processed <- textProcessor(data$topic_content, metadata=data,customstopwords=stopwords)
meta <- processed$meta
meta$date_no <- as.numeric(as.Date(meta$sitting_date))
meta$year <- as.POSIXlt(as.Date(meta$sitting_date))$year + 1900
out <- prepDocuments(processed$documents, processed$vocab, meta)

# The main STM modelling function - See STM help file for more options
mod.out <- stm(out$documents, out$vocab, K=20, prevalence = ~s(date_no)+year,data=out$meta,ngroups=4,seed=7244757,init.type="Spectral") 
?stm
# Diagnostic plot for convergence
plot(mod.out$convergence$bound,type="l",ylab="Approximate Objective",main="Convergence")

# Plot of topic proportions
plot.STM(mod.out, xlim=c(0,.5), labeltype="prob", n=7)

# Identify topics
findThoughts(mod.out, meta$topic_content, topics=11) 

# Output examples of each topic and top words to a text file for human analysis and labelling
topics_out <- capture.output(findThoughts(mod.out, meta$topic_content, topics=c(1:20),n=5))
cat(topics_out,file="/topics_text.txt",sep="\n",append=TRUE)
words_out <- capture.output(labelTopics(mod.out,c(1:20)))
cat(words_out,file="topics_text.txt",sep="\n",append=TRUE)

TopicNames = c() # Set topic names once you're done!

# STM's estimate effect function is essentially a regression fit
prep <- estimateEffect(1:20~s(date_no)+year, mod.out, metadata=out$meta)

# You can plot selected topics.
plot.estimateEffect(prep, "date_no", topics=c(9,4,19), method="continuous",labeltype="custom", custom.labels=c("Municipal", "Education","Citizenship"),axis(1, at=seq(-1825,-730, by=365), labels=seq(1965,1968)))

plot.estimateEffect(prep, "date_no", topics=c(9), method="continuous",ci.level=0,xaxt="n",xlab="Time (Month)",printlegend=FALSE)
yearseq <- as.numeric(seq(from=as.Date("1966-01-01"),to=as.Date("1968-01-01"),by="year"))
axis(1,at=yearseq,labels=c(1966,1967,1968))
title(main="Prevalence of Topic 9 over Time")

# Point Estimate option can be useful in some situations
plot.estimateEffect(prep, "year", topics=9, method="pointestimate")

# Topic correlations - STM also has a plotting function to show the network
mod.out.corr<-topicCorr(mod.out)
plot.topicCorr(mod.out.corr, vlabels=TopicNames)

# Word clouds

cloud(mod.out,topic=11)

# Save wordclouds to file
for(i in 1:length(topicNames)) {
  writeLines(paste0("Topic ",i,": ",topicNames[i]))
  png(paste0('./topic',i,'.png'), width=600, height=600)
  cloud(mod.out, topic=i, scale=c(3,.5), random.order=FALSE)
  dev.off()
}

# STM also has a helper package to help with visualization of topic correlations in a web format

library(stmCorrViz)
stmCorrViz(mod.out, "stmviz.html", documents_raw=as.character(data$topic_content), documents_matrix=out,title="Analysis of _____", labels_number=7, display=TRUE, verbose=TRUE)
Status API Training Shop Blog About Pricing
© 2016 GitHub, Inc. Terms Privacy Security Contact Help