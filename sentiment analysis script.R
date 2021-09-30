## Sentiment Analysis

library(tidytext)
library(readr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(jsonlite)
library(dplyr)
library(tidytext)
library(DT)
library(stringr)
library(sentimentr)
library(ggplot2)
library(tidyquant)

path <- "C://Users//cantilk//Documents//Wordcloud Project//"
text1 <- readLines(paste(path, "Hanna Lin - 1.txt", sep=""))
text2 <- readLines(paste(path, "Hanna Lin - 2.txt", sep=""))
text2b <- readLines(paste(path, "Hanna Lin - 2b.txt", sep=""))
text3 <- readLines(paste(path,"Hanna Lin - 3.csv",sep=""))
text4 <- (read_json(paste(path,"Hanna Lin - 4.json",sep=""), simplifyVector = TRUE))$messages
text5 <- (read_json(paste(path,"Hanna Lin - 5.json",sep=""), simplifyVector = TRUE))$messages

whois <- "Keiran"

if (whois == "Hanna"){
  who_google <- "<Hanna Lin>"
  who_signal <- "<Hanna>"
  who_insta <- "Hanna Lin"
}
if(whois == "Keiran"){
  who_google <- "<Keiran Cantilina>"
  who_signal <- "<Keiran>"
  who_insta <- "Keiran Cantilina"
}

hanna_rows <- c()
test_vector <- c()
filter <- TRUE

## Text 1 (Google Hangouts): 10-19-2016 to 04-12-2018
for(i in 1:length(text1)){
  current_row <- text1[i]
  if(grepl(who_google, current_row)|| filter){
    hanna_rows <- append(hanna_rows, paste(str_split(current_row, ">", n = 2)[[1]][2]," ",sep=""), after = length(hanna_rows))
  }
}

## Text 2 (Google Hangouts): 04-09-2018 to 01-26-2021
for(i in 1:length(text2)){
  current_row <- text2[i]
  if(grepl(who_google, current_row)|| filter){
    hanna_rows <- append(hanna_rows, paste(str_split(current_row, ">", n = 2)[[1]][2]," ",sep=""), after = length(hanna_rows))
  }
}

## Text 2b (Google Hangouts): 01-27-2021 to 02-21-2021
for(i in 1:length(text2b)){
  current_row <- text2b[i]
  if(grepl(who_google, current_row)|| filter){
    hanna_rows <- append(hanna_rows, paste(str_split(current_row, ">", n = 2)[[1]][2]," ",sep=""), after = length(hanna_rows))
  }
}

## Text 3 (Signal): 02-16-2021 to 04-22-2021
for(i in 1:length(text3)){
  current_row <- text3[i]
  if(grepl(who_signal, current_row)|| filter){
    hanna_rows <- append(hanna_rows, paste(str_split(current_row, ">,", n = 2)[[1]][2]," ",sep=""), after = length(hanna_rows))
  }
}

# Text 4 (Instagram): 03-11-2018 to 02-05-2021
for (i in 1:nrow(text4)){
  current_row <- text4[i,]
  if((grepl(who_insta, current_row$sender_name) || filter) && current_row$type=="Generic" && !is.na(current_row$content) && current_row$content!="Liked a message"){
    hanna_rows <- append(hanna_rows, current_row$content, after = length(hanna_rows))
    test_vector <- append(test_vector, current_row$content, after = length(test_vector))
  }
}

# Text 5 (Instagram): 02-06-2021 to 09-15-2021
for (i in 1:nrow(text5)){
  current_row <- text5[i,]
  if((grepl(who_insta, current_row$sender_name) || filter) && current_row$type=="Generic" && !is.na(current_row$content) && current_row$content!="Liked a message"){
    hanna_rows <- append(hanna_rows, current_row$content, after = length(hanna_rows))
    test_vector <- append(test_vector, current_row$content, after = length(test_vector))
  }
}

sentences <- get_sentences(hanna_rows)
sentiment_table <- sentiment(sentences, by=NULL)
average_sentiment <- mean(sentiment_table$sentiment)
sentiment_dataframe <- data.frame(index=1:76131, sentiment=sentiment_table$sentiment)
sentiment_over_time <- ggplot(data=sentiment_dataframe, aes(x=index, y=sentiment, group=1))+
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
sentiment_over_time

# Print plots to a pdf file
pdf("C://Users//cantilk//Documents//Wordcloud Project//sentiment_over_time.pdf")
print(sentiment_over_time)
dev.off() 




