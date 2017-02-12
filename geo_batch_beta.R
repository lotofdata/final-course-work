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


geo <- data.frame(timezone)

result <- do.call(rbind,lapply(as.character(geo$timezone),geo.dsk))
geo.result <- data.frame(result)

write.csv(geo.result, "ufc_geo.csv", row.names=FALSE)

write.csv(dataset, "ufc_final.csv", row.names=FALSE)

