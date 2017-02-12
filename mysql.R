library(RMySQL)

con <- dbConnect(MySQL(),
                 user = 'root',
                 password = 'xxxx',
                 host = 'localhost',
                 dbname='db_03')

con <- dbConnect(MySQL(),
                 user = 'root',
                 password = 'xxxxx',
                 host = 'localhost',
                 dbname='db')

dbGetQuery(con, "SET NAMES 'utf8'")

#test2 = dbGetQuery(con, "SELECT * FROM deadpool_tweets SET NAMES 'utf8'")
#df = dbGetQuery(con, "SELECT *, CONVERT(text USING utf8)FROM vilnius_tweets")

#write.csv(df, "dataset.csv", row.names=FALSE)
#df <- read.csv("dataset.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")
test2 = dbGetQuery(con, "SELECT * FROM star_wars_tweets LIMIT 0, 1000")


test2 = dbGetQuery(con, "SELECT * FROM deadpool_tweets LIMIT 0, 2000000")
test2 = dbGetQuery(con, "SELECT * FROM deadpool_tweets LIMIT 2000000, 5000000 ")


test3 = dbGetQuery(con, "SELECT * FROM hunger_games_tweets LIMIT 500000, 900000")

summary(test2)

test2 = dbGetQuery(con, "SELECT * FROM batman_superman_tweets")
test2= dbGetQuery(con, "SELECT * FROM star_wars1_tweets ")

??rbind()

starwars <- rbind(test2, test1)
write.csv(starwars, "starwars.csv", row.names=FALSE)


idx = c(4,5)
pos = dataset[dataset$positive %in% idx,] 
neg = dataset[dataset$negative %in% idx,] 

neg = test2[test2$negative %in% idx,] 
pos = test2[test2$positive %in% idx,]

test2 <- read.csv("starwars_final.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
write.csv(starwars, "starwars_final.csv", row.names=FALSE)



