setwd ("C://Users/Darius/Desktop/sentiment140/Final_duomenys/")


library(syuzhet)
library(plotrix)
library(coreNLP)
library(pander)
library(rJava)



#veikia
test2 <- read.csv("test.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")

idx = c("en","","")
dataset1 = dataset[dataset$lang %in% "en" ,] 
neg = dataset[dataset$negative %in% idx,] 
--------------------------------------------------------------
#pasalina vienodas zinutes
test2 <- test2 [!duplicated(test2$text),]

txt = test2[["text"]]
date = test2[["created_at"]]
date1 <- as.Date(test2$created_at)
location = test2[["location"]]
realname = test2[["from_user_realname"]]
timezone = test2[["from_user_timezone"]]
lang = test2[["lang"]]


#timezone1 <- test2 [!duplicated(test2$from_user_timezone),]


timezone= gsub("\\s*\\([^\\)]+\\)","", timezone)
timezone = gsub("Central Time", "USA", timezone)
timezone = gsub("Eastern Time", "USA", timezone)
timezone = gsub("Pacific Time", "USA", timezone)
timezone = gsub("Mountain Time", "USA", timezone)
timezone = gsub("Atlantic Time", "Canada", timezone)
timezone = gsub("JST", "Tokyo", timezone)
timezone = gsub("Athens", "Rome", timezone)
timezone = gsub("Central America", "Honduras", timezone)
timezone = gsub("Central Time", "USA", timezone)
timezone = gsub("Mid-Atlantic", "USA", timezone)
#timezone = gsub("Central Time", "washington", timezone)



#test2$V2 <- sub("^$", "N", dat$V2)
#uzpildom tuscias vietas nuliais
#timezone <- sub("^$", "0", timezone)
#location <- sub("^$", "0", location)
timezone[is.na(timezone)] <- 0


#geo <- data.frame(timezone)


#TEXT CLEANING
# First we will remove retweet entities from the stored tweets (text)
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
# Then remove all "@people"
txt = gsub("@\\w+", "", txt)
# Then remove all the punctuation
txt = gsub("[[:punct:]]", "", txt)
# Then remove numbers, we need only text for analytics
txt = gsub("[[:digit:]]", "", txt)
# the remove html links, which are not required for sentiment analysis
txt = gsub("http\\w+", "", txt)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
txt = gsub("[ \t]{2,}", "", txt)
txt = gsub("^\\s+|\\s+$", "", txt)

t1 <- Sys.time()
nrc_data <- get_nrc_sentiment(txt)
t2 <- Sys.time()
t2-t1

----------------------------------------------------------------
  dataset<- cbind(date,date1, realname, lang, timezone, txt, nrc_data)
----------------------------------------------------------------
  # con2<-file('test.csv',encoding="utf-8")
  # #write.csv(...,file=con,...)  
  # write.csv(dataset1, file=con2, row.names=FALSE)
  
  #write.csv(dataset, "creed_final.csv", row.names=FALSE)
  
out <- file("superman.csv", "w", encoding="UTF-8") 
write.table(dataset, out, sep=",", row.names=FALSE) 
close(out) 

-----------------------------------------------
#clean.text = function(x)
#{
  # tolower
  #x = tolower(x)
  # remove rt
   x = gsub("RT", "", x)
  # remove at
   x = gsub("@\\w+", "", x)
  # remove punctuation
   x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  #x = gsub("http\\w+", "", x)
  # remove tabs
  # x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
   x = gsub("^ ", "", x)
  # remove blank spaces at the end
  # x = gsub(" $", "", x)
  x = gsub("http[^[:space:]]*", "", x)

  return(x)
}

# clean texts
#text_clean = clean.text(text)

#write.table(bjp_txt, "text.csv", row.names=FALSE, col.names=FALSE)


#encoding = "UTF-8"
#tweets = readLines("text.csv")

nrc_data <- get_nrc_sentiment(txt)

sentiment_vector <- get_sentiment(tweets, method="bing")
summary(sentiment_vector)


----------------------------------------------------------------
dataset<- rbind(starwars_08, starwars_22)
----------------------------------------------------------------
# con2<-file('test.csv',encoding="utf-8")
# #write.csv(...,file=con,...)  
# write.csv(dataset1, file=con2, row.names=FALSE)

#write.csv(dataset, "creed_final.csv", row.names=FALSE)

out <- file("final_creed.csv", "w", encoding="UTF-8") 
write.table(dataset, out, sep=",", row.names=FALSE) 
close(out) 



write.table(text, file=con2, row.names=FALSE, col.names=TRUE)
write.table(text, "test.txt", row.names=FALSE, col.names=TRUE)
dataset<- cbind(time, realname, bjp_txt, nrc_data)

dataset <- dataset [!duplicated(dataset$tweets),]



idx = c(3,4,5)
pos = dataset[dataset$positive %in% idx,] 
neg = dataset[dataset$negative %in% idx,] 

########################################################################################################################################


dataset <- read.csv("dataset.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")

timezone = dataset[["timezone"]]
geo <- data.frame(timezone)

###############GISGRAPHY launch####################################
#cd /var/www/html/geo/
#./launch.sh
#http://192.168.56.101:8080/
#./stop.sh

#geocoding function

geo.dsk <- function(addr){
  require(httr)
  require(rjson)
  
  out <- tryCatch({
    url      <- "http://192.168.56.101:8080/fulltext/search"
    response <- GET(url,query=list(q=addr,allwordsrequired="false", spellchecking="true", radius="10000",suggest="false",style="MEDIUM",format="JSON",from="1",to="1", indent="false"))
    json <- fromJSON(content(response,type="text")) #application/json;charset=UTF-8 #text
    loc  <- json['response'][[1]]$docs[[1]]
    return(c(timezone=addr,long=loc$lng, lat= loc$lat, country=loc$country_name))
  },
  
  error = function(cond) {
    message(paste("Address not geocoded:", addr))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  
  warning = function(cond) {
    message(paste("Address caused a warning:", addr))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  
  finally = {
    message(paste("Processed Address:", addr))
    message("One down...")
  }
  
  )
  return(out)
}


result <- do.call(rbind,lapply(as.character(geo$timezone),geo.dsk))
geo.result <- data.frame(result)

dataset2<- cbind(geo.result, dataset)






#negative bus su minusu
nrc_data$negative <- -abs(nrc_data$negative)

dataset<- cbind(time, realname, location, geo.result, text_clean, nrc_data)
write.csv(dataset2, "starbucks_final.csv", row.names=FALSE)


##################################################
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
#################################################



poa_sent <- get_sentiment(s_v2, method="bing")
poa_sent <- sentiment_vector

plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

percent_vals <- get_percentage_values(poa_sent)
plot(
  percent_vals, 
  type="l", 
  main="Joyce's Portrait Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

##???
ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="h", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
#



























