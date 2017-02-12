setwd ("C://Users/Darius/Desktop/sentiment140/")


text = test2[["text"]]
for( i in 1:length(text)){
  capture.output(text[i], file = paste("doc_number_", i, ".txt", sep="") )
}


library(openxlsx)
df2 <- read.xlsx("address.xlsx", sheet = 4, skipEmptyRows = TRUE)
df2$Date <- convertToDate(df2$Date)
sapply(df2, class)
head(df2)
View(df2)
summary(df2)
??read.xlsx

require(XLConnect)
wb = loadWorkbook("address.xlsx")
df = readWorksheet(wb, sheet = "Sheet1", header = TRUE)
