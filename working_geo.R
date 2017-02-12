
setwd ("C://Users/Darius/Desktop/sentiment140")

test <- read.csv("test.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8", na.strings = "NULL")
na.strings = "NULL"
DF <- read.table("test.csv", header = TRUE,sep = ",", na.strings = "NULL")
test <- data.frame(address=c(
  "vilnius",
  "los angeles"
))

###############################################################

geo.dsk <- function(addr){ # single address geocode with data sciences toolkit
  require(httr)
  require(rjson)
  url      <- "http://192.168.56.101:8080/fulltext/search"
  response <- GET(url,query=list(q=addr,allwordsrequired="false", spellchecking="true", radius="10000",suggest="false",style="MEDIUM",format="JSON",from="1",to="1", indent="false"))
  json <- fromJSON(content(response,type="text")) #application/json;charset=UTF-8 #text
  loc  <- json['response'][[1]]$docs[[1]]
  return(c(address=addr,long=loc$lng, lat= loc$lat, name=loc$country_name))
}

result <- do.call(rbind,lapply(as.character(timezone$address),geo.dsk))
geo.result <- data.frame(result)
result


#http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false&address=vilnius
#http://free.gisgraphy.com/geocoding/geocode?address=vilnius&format=JSON&indent=false
#http://192.168.56.101:8080/fulltext/search?q=vilnius&allwordsrequired=false&spellchecking=false&radius=10000&suggest=false&style=MEDIUM&format=JSON&from=1&to=1&indent=false



###########################################################################
dff <- data.frame(address=c(
  "Birmingham, Alabama, United States",
  "Mobile, Alabama, United States",
  "Phoenix, Arizona, United States",
  "Tucson, Arizona, United States",
  "Little Rock, Arkansas, United States",
  "Berkeley, California, United States",
  "Duarte, California, United States",
  "Encinitas, California, United States",
  "La Jolla, California, United States",
  "Los Angeles, California, United States",
  "Orange, California, United States",
  "Redwood City, California, United States",
  "Sacramento, California, United States",
  "San Francisco, California, United States",
  "Stanford, California, United States",
  "Hartford, Connecticut, United States",
  "riga",
  "New Haven, Connecticut, United States"
))


geo.dsk <- function(addr){ # single address geocode with data sciences toolkit
  require(httr)
  require(rjson)
  url      <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
  response <- GET(url,query=list(sensor="FALSE",address=addr))
  json <- fromJSON(content(response,type="text"))
  loc  <- json['results'][[1]][[1]]$geometry$location
  return(c(address=addr,long=loc$lng, lat= loc$lat))
}
result <- do.call(rbind,lapply(as.character(dff),geo.dsk))
result <- data.frame(result)


#http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false&address=vilnius
#http://free.gisgraphy.com/geocoding/geocode?address=vilnius&format=JSON&indent=false

url1      <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
response1 <- GET(url,query=list(sensor="FALSE",address="vilnius"))


url      <- "http://free.gisgraphy.com/geocoding/geocode"
response <- GET(url,query=list(address="vilnius"))



url      <- "http://192.168.56.101:8080/geocoding/geocode"
response <- GET(url,query=list(address="vilnius", format="JSON",indent="false"))

url      <- "http://192.168.56.101:8080/fulltext/search"
response <- GET(url,query=list(q="vilnius",allwordsrequired="false", spellchecking="false", radius="10000",suggest="false",style="MEDIUM",format="JSON",from="1",to="1", indent="false"))





url1      <- "http://192.168.56.101:8080/fulltext/search"
response1 <- GET(url1,query=list(q="fsdfsdfsd",allwordsrequired="false", spellchecking="false", radius="10000",suggest="false",style="MEDIUM",format="JSON",from="1",to="1", indent="false"))
json1 <- fromJSON(content(response1,type="text")) #application/json;charset=UTF-8 #text
loc1  <- json1['results'][[1]][[1]]$geometry$location

a = json['response'][[1]]$docs[[1]]
long=a$lat
name= a$country_name


url      <- "http://192.168.56.101:8080/fulltext/search"
response <- GET(url,query=list(q="vilnius",allwordsrequired="false", spellchecking="true", radius="10000",suggest="false",style="MEDIUM",format="JSON",from="1",to="1", indent="false"))
json <- fromJSON(content(response,type="text")) #application/json;charset=UTF-8 #text
loc  <- json['response'][[1]]$docs[[1]]


name= a$country_name













