# load and install packages

pkg <- c("httr", "rjson", "dplyr", "stringr", "devtools", "leaflet")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

suppressMessages(library(httr))
suppressMessages(library(rjson))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(devtools))

suppressMessages(if (!require("leaflet")) devtools::install_github("rstudio/leaflet"))
suppressMessages(library(leaflet))


# create sample data frame of addresses to geocode

name   <- c('Carleton College', 'Pomona College', 'asdasda')
street <- c('300 North College St', '333 N College Way', 'asdasda') 
city   <- c("Northfield", "Claremont", "asdasda") 
state  <- c('MN', 'CA', 'asda') 
zip    <- c('55057', '91711', 'asdasd')
data   <- data.frame(name, street, city, state, zip)      


# create location variable 

data$location <- paste(str_trim(as.character(data$street)),
                       str_trim(as.character(data$city)),
                       str_trim(as.character(data$state)),
                       str_trim(as.character(data$zip)), sep=' ')


# create geocode function with tryCatch 
# geocoding api is from http://www.datasciencetoolkit.org/

geo.dsk <- function(addr){
  require(httr)
  require(rjson)
  
  out <- tryCatch({
    url      <- "http://192.168.56.101:8080/fulltext/search"
    response <- GET(url,query=list(q=addr,allwordsrequired="false", spellchecking="true", radius="10000",suggest="false",style="MEDIUM",format="JSON",from="1",to="1", indent="false"))
    json <- fromJSON(content(response,type="text")) #application/json;charset=UTF-8 #text
    loc  <- json['response'][[1]]$docs[[1]]
    return(c(address=addr,long=loc$lng, lat= loc$lat, name=loc$country_name))
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



result <- do.call(rbind,lapply(as.character(timezone$address),geo.dsk))
geo.result <- data.frame(result)


































# geocode data and bind coordinates onto data

geo_result <- 
  cbind(data,
        as.data.frame(
          do.call(rbind,
                  lapply(as.character(timezone), geo.dsk))))


write.table(result, "result2.csv", row.names=FALSE, col.names=TRUE)


result <- do.call(rbind,lapply(as.character(test$address),geo.dsk))
result <- data.frame(result)







index <- mydata$Var1 <= 1
mydata$Freq[index] = -abs(mydata$Freq[index])


index <- test2$negative <= 1
test2$negative[index] = -abs(test2$negative[index])



new.Freq <- with(test2, ifelse(negative <= 1, -Freq, Freq))



test2$negative <- -abs(test2$negative)
write.csv(test2, "paypal_final2.csv", row.names=FALSE)

??freq

test <- -abs(test2$positive)

test1 <- data.frame(test)


??abs



