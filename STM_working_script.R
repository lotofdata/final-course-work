load(url("http://goo.gl/91KbfS"))
library(stm)
library(igraph)

##############################################
#READ IN DATA
##############################################


data <- read.csv("poliblogs2008.csv")
data <- mach_text
shortdoc= head(data, 250)
##############################################
#PREPROCESSING
##############################################

#stemming/stopword removal, etc.
processed <- textProcessor(pos$bjp_txt, metadata=pos)#idedam customstopwords 
processed <- textProcessor(neg$bjp_txt, metadata=neg)
processed <- textProcessor(reviews$V1, metadata=reviews)
??textProcessor


test2 <- gsub("[[:digit:]]", "", test2)
test2 <- gsub("[[:punct:]]", " ", test2)



meta<-processed$meta
vocab<-processed$vocab
docs<-processed$documents
out <- prepDocuments(docs, vocab, meta)





#structure and index for usage in the stm model. Verify no-missingness. can remove low frequency words using 'lower.thresh' option. See ?prepDocuments for more info
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
?prepDocuments
out <- c("processed$documents", processed$vocab, processed$meta)
#take a look at how many words and documents would be removed with different lower.thresholds    !!! check Error: could not find function "plotRemoved"
plotRemoved(processed$documents, lower.thresh=seq(1,200, by=100))

#output will have object meta, documents, and vocab 
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

##############################################
#RUN AND CHOOSE THE BEST TOPIC MODEL
##############################################


#run an stm model using the 'out' data. 20 topics. Asking how prevalaence of topics varies across documents' meta data, including 'rating' and day. !! option s(day) applies a spline normalization to day variable. I may want to understand/use this!! maximum number of expectation-maximization iterations = 75, and the authors have specified the seed they are using for the sake of reproducibility.
poliblogPrevFit <- stm(out$documents,out$vocab,K=20, max.em.its=10, data=out$meta,seed=5926696)

poliblogPrevFit <- stm(docs,vocab,K=20, max.em.its=75, data=meta,seed=5926696)
#poliblogPrevFit <- stm(out$documents,out$vocab,K=20,prevalence =~ rating+ s(day), max.em.its=75, data=out$meta,seed=5926696)
#let STM help you compare a number of models side by side. It will keep the models that don't stink (i.e. that converge quickly) 

#poliblogSelect <- selectModel(out$documents,out$vocab,K=20,prevalence =~ rating+s(day), max.em.its=75,data=meta,runs=20,seed=8458159)

poliblogSelect <- selectModel(out$documents,out$vocab,K=20, max.em.its=10,data=meta,runs=30,seed=8458159)

poliblogSelect <- selectModel(docs,vocab,K=20, max.em.its=75,data=meta,runs=30,seed=8458159)

#plot the different models that make the cut along exclusivity and semantic coherence of their topics
plotModels(poliblogSelect)

#the 3rd one looks best, so choose it and give it the name poliblogPrevFit
poliblogPrevFit<-poliblogSelect$runout[[3]] #choose the third model

#####
#####
#another option: let R figure out the best model for you (defined by exclusivity and semantic coherence) for each K (i.e. # of topics) (here among 10 options per K)  
#storage<-manyTopics(out$documents,out$vocab,K=c(7,10), prevalence =~ rating+s(day),data=meta, runs=10)
storage<-manyTopics(out$documents,out$vocab,K=c(7,10),data=meta, runs=75)
storage<-manyTopics(docs,vocab,K=c(7,10),data=meta, runs=75)
#####
#####

#since data are stored in a list environment, the 'storage' container above, you need to choose the output you want by looking at the first, then second, then 3rd etc. model. E.g. if you set K=c(7,8,9), you'd seek
#output with output_for_7_topic_model<-storage$out[[1]] -- output_for_8_topic_model<-storage$out[[2]] -- etc.  Then, presumably, you can use output_for_7_topic_model as 
#Is it possible to use plotModels(storage)????? to see which of different K models is best????? Would be ideal. 
mod <- stm(docs, vocab, K=25,
           data=meta,
           max.em.its=75, init.type="Spectral") 
#please don't run a model with 2 iterations
#this is done here to make it run quickly.
toLDAvis(mod=poliblogPrevFit, docs=docs)
toLDAvis(mod=mod, docs=docs)
##############################################
#BEGIN INTERPRETING THE MODEL -- FUN!!!
##############################################

###LIST OF TOP WORDS for topics 1, 7, & 10
labelTopics(poliblogPrevFit, c(1,2,3,4,5,6, 7,8,9, 10))

df <-labelTopics(poliblogPrevFit, c(1,2,3,4,5,6, 7,8,9, 10))
df = data.frame(df)

topic_cluster = labelTopics(poliblogPrevFit)
topic_cluster2 = labelTopics(poliblogSelect)

par("mar")
par(mar=c(1,1,1,1))

#lentele su zodziais
plot(poliblogPrevFit, type = c("summary"),
     n = 7, 
     labeltype=c("prob"), 
     main = , xlab = NULL, 
     family = "", width = 80, 
     covarlevels = NULL, plabels = NULL, text.cex=1, 
     custom.labels=NULL
    )

#lentele su network rysiais
mod.out.corr<-topicCorr(poliblogPrevFit)
plot.topicCorr(mod.out.corr)







df <- data.frame(matrix(unlist(df), nrow=132, byrow=T),stringsAsFactors=FALSE)

topic_cluster = do.call(rbind.data.frame, topic_cluster)
write.table(topic_cluster, "starwars_pos_topiccluster.csv", row.names=TRUE, col.names=FALSE)







###WORDCLOUD for a specified TOPIC
cloud(poliblogPrevFit, topic=1)

###Read DOCUMENTS that are highly correlated with the topics you specify using findThoughts() function
#object 'thoughts1' contains 2 documents about topic 1. 'texts=shortdoc,' gives you just the first 250 words
thoughts1<-findThoughts(poliblogPrevFit, texts=docs,n=2, topics=1)$docs[[1]]
??findThoughts
#will show you the output
plotQuote(thoughts7, width=40, main="Topic 1")

#how about more documents for more of these topics?
thoughts7 <- findThoughts(poliblogPrevFit, texts=docs,n=2, topics=7)$docs[[1]]
thoughts10 <- findThoughts(poliblogPrevFit, texts=docs,n=2, topics=10)$docs[[1]]
thoughts4 <- findThoughts(poliblogPrevFit, texts=docs,n=2, topics=4)$docs[[1]]
#And in a 2X2 table? We like 2X2 tables!  --- Note: this command will force all remaining plots into a 2X2 table format
par(mfrow = c(2, 2),mar=c(.5,.5,1,.5)) 
plotQuote(thoughts1, width=40, main="Topic 1")
plotQuote(thoughts10, width=40, main="Topic 4")
plotQuote(thoughts7, width=40, main="Topic 7")

##see PROPORTION OF EACH TOPIC in the entire CORPUS. Just insert your STM output
??plot.stm
plot.STM(poliblogPrevFit, type="summary", xlim=c(0,.4), n=6,labeltype = "prob")
plot.STM(poliblogPrevFit, type="labels", xlim=c(0,.4), n=6,labeltype = "prob")
plot.STM(poliblogPrevFit, type="summary", xlim=c(0,.5),n=6,labeltype = "prob")


??plot.topicCorr

##see GRAPHICAL NETWORK DISPLAY of how closely related topics are to one another, (i.e., how likely they are to appear in the same document) Requires 'igraph' package
mod.out.corr<-topicCorr(poliblogPrevFit)
 plot.topicCorr(mod.out.corr)

##VISUALIZE DIFFERENCES BETWEEN TWO DIFFERENT TOPICS using the ,type="perspectives" option
plot.STM(poliblogPrevFit,type="perspectives", topics=c(9, 10))


######################################################################################################################
# STM: SEE HOW PREVALENCE OF TOPICS VARIES ACROSS DOCUMENTS ACCORDING TO DOCUMENT COVARIATES (METADATA) -- MEGA FUN!!!
######################################################################################################################


###See CORRELATIONS BTWN METADATA & TOPIC PREVALANCE in documents
###First, must estimate an effect of the metadata covariates on topic prevalence in a document, so that we have anything to plot
###Estimating the expected proportion of a document that belongs to a topic as a function of a covariate
#Right-quick, for this vignette, we need to tell R to convert the 'rating' variable in the meta file (which is currently a string variable) into a categorical variable
meta$rating<-as.factor(meta$rating)
#since we're preparing these coVariates by estimating their effects we call these estimated effects 'prep'
#we're estimating Effects across all 20 topics, 1:20. We're using 'rating' and normalized 'day,' using the topic model poliblogPrevFit. 
#The meta data file we call meta. We are telling it to generate the model while accounting for all possible uncertainty. Note: when estimating effects of one covariate, others are held at their mean
prep <- estimateEffect(1:20 ~ rating+s(day),poliblogPrevFit,meta=meta, uncertainty = "Global")

###See how PREVALENCE of TOPICS DIFFERS across VALUES of a CATEGORICAL COVARIATE  
plot.estimateEffect(prep, covariate = "rating", topics = c(1, 7, 10),
                    #topic model=poliblogPrevFit. Method="difference" 
                    model=poliblogPrevFit, method="difference",
                    #only using two values of covariate, and labeling them... assume we could do this with a non-binary covariate and just specify
                    cov.value1="Liberal",cov.value2="Conservative",
                    xlab="More Conservative ... More Liberal",
                    main="Effect of Liberal vs. Conservative",
                    xlim=c(-.1,.1), labeltype = "custom",
                    custom.labels = c('Jeremiah Wright', 'Sarah Palin',
                                      'Bush Presidency'))

#See how PREVALENCE of TOPICS DIFFERS across VALUES of a CONTINUOUS COVARIATE
#plotting prep data on day variable, a continuous variable with a continous plot. focusing on topic 7. not sure what model= z means !!! (removal has no effect on image) will want to do this for protester topics a lot!
plot.estimateEffect(prep, "day", method="continuous", topics=7, model=z,
                    printlegend=FALSE, xaxt="n", xlab="Time (2008)")
monthseq <- seq(from=as.Date("2008-01-01"),
                to=as.Date("2008-12-01"), by="month")
monthnames <- months(monthseq)
axis(1,at=as.numeric(monthseq)-min(as.numeric(monthseq)),
     labels=monthnames)


###########################################################################################################################################
# STM: SEE HOW WORDS OF THE TOPICS ARE EMPHASIZED DIFFERENTY ACROSS DOCUMENTS ACCORDING TO DOCUMENT COVARIATES (METADATA) -- ALSO FUN!!!
###########################################################################################################################################
#### Let's do something different. Instead of looking at how prevalent a topic is in a class of documents categorized by meta-data covariate... 
#### ... let's see how the words of the topic are emphasized differently in documents of each category of the covariate
##First, we we estimate a new stm. It's the same as the old one, including prevalence option, but we add in a content option
poliblogContent <- stm(docs,vocab,K=20,
              
                       max.em.its=75, data=meta,seed=5593453)
##Next, we plot using the ,type="perspectives" option to the plot.STM function
plot.STM(poliblogContent, topics=1)
??plot.stm
plot(poliblogPrevFit)
plot(poliblogContent, type = "hist" )

########################################
# STM: USING INTERACTION VARIABLES
########################################

####
###Interacting covariates. Maybe we have a hypothesis that cities with low $$/capita become more repressive sooner, while cities with higher budgets are more patient 
##first, we estimate an STM with the interaction
poliblogInteraction <- stm(out$documents,out$vocab,K=20,
                           prevalence =~ rating* day, max.em.its=75,
                           data=out$meta,seed=5926696)
#Then, as above, we prep the covariates usine the estimateEffect() function -- but this time, we include the interaction variable.
prep <- estimateEffect(c(1) ~ rating*day, poliblogInteraction,
                       metadata=meta, uncertainty="None")
#Then, we use our plotting function
plot.estimateEffect(prep, covariate="day", model=poliblogInteraction,
                    method="continuous",xlab="Days", moderator="rating",
                    moderator.value="Liberal", linecol="blue", ylim=c(0,.08),
                    printlegend=F)
plot.estimateEffect(prep, covariate="day", model=poliblogInteraction,
                    method="continuous",xlab="Days", moderator="rating",
                    moderator.value="Conservative", linecol="red", add=T,
                    printlegend=F)
legend(0,.08, c("Liberal", "Conservative"),
       lwd=2, col=c("blue", "red"))

