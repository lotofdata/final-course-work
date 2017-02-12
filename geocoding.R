setwd ("C://Users/Darius/Desktop/sentiment140/")
library(ggmap)

#get coordinates from google maps
coords = geocode(as.character(test$Country))
coord = geocode(as.character(test2$V1))
geocode("Konstitucijos prospektas 7a")
#iso8859-13
#add points to map
points(coords$lon,coords$lat,col="red")

?points

Sys.setlocale(category = "LC_ALL", locale = "Lithuanian")
sessionInfo()


test2
-------------------------------------------------
test2 <- read.csv("coordinates.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")
---------------------------------------------------
test2 <- read.csv("vaistines4.csv")
test2 <- readLines("vaistines3.csv")
df <- read.table("vaistines3.csv", header = TRUE)

vaistines <- cbind (test2, coord)
write.csv(vaistines, "coordinates.csv", row.names=FALSE)



grep("Upës", test2$V1)

------------------------
# i notepada kopinam lietuviskom raidem, save as utf-8, tada sitas 
# test2 <- read.csv("vaistines4.csv", header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")
# CP -> regional and language -> administrative -> system locale


test2$V1 = gsub("pr.", "prospektas", test2$V1)
test2$V1 = gsub("skg.", "skersgatvis", test2$V1)
test2$V1 <- sub("$", ", vilnius", test2$V1 )

coord = geocode(as.character(test2$V1))



library(leaflet)
library(magrittr)

# SFmap <- leaflet() %>% 
#   addTiles() %>% 
#   setView(-122.42, 37.78, zoom = 13) %>% 
#   addMarkers(-122.42, 37.78, popup = 'Bay Area')
# SFmap

#54.6833° N, 25.2833° E

SFmap <- leaflet() %>% 
  addTiles() %>% 
  setView(25.28, 54.68, zoom = 14) %>% 
  addCircleMarkers(25.28, 54.68, popup = 'Vilnius', radius = 5, color = 'red')
SFmap


SFMap <- leaflet() %>% 
  addTiles() %>%
  setView(25.28, 54.68, zoom = 13) %>%
  addCircleMarkers(data = test2, lng = ~ lon, lat = ~ lat, radius = 5, color = 'blue'
  )
SFMap

# SFMap <- leaflet() %>% 
#   addTiles() %>%
#   setView(25.28, 54.68, zoom = 13) %>%
#   addCircleMarkers(data = test2, lng = ~ lon, lat = ~ lat, popup = test2$V1, radius = 5, color = 'blue',
#                    clusterOptions = markerClusterOptions())
# SFMap

?addMarkers
