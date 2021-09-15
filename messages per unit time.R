## Gchat messages per unit time analyzer

library(stringr)
library(readr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(jsonlite)
library(dplyr)

path <- "C://Users//cantilk//Documents//Wordcloud Project//"
text1 <- readLines(paste(path, "Hanna Lin - 1.txt", sep=""))
text2 <- readLines(paste(path, "Hanna Lin - 2.txt", sep=""))
text2b <- readLines(paste(path, "Hanna Lin - 2b.txt", sep=""))
text3 <- readLines(paste(path,"Hanna Lin - 3.csv",sep=""))
#text3 <- readLines(paste(path,"//Signal backup raw  data old//Hanna Lin - 3.csv",sep=""))
text4 <- (read_json(paste(path,"Hanna Lin - 4.json",sep=""), simplifyVector = TRUE))$messages
text5 <- (read_json(paste(path,"Hanna Lin - 5.json",sep=""), simplifyVector = TRUE))$messages



# as.Date(as.POSIXct(text4$timestamp_ms[1]/1000, origin="1970-01-01", timezone="America/New_York"))

hanna_message_dates <- c()
test_vector <- c()
who <- "<Hanna"
who_insta <- "Hanna Lin"
unfiltered <- FALSE

## Text 1 (Google Hangouts): 10-19-2016 to 04-12-2018
for(i in 1:length(text1)){
  current_row <- text1[i]
  if(grepl(who, current_row)||unfiltered){
    if(grepl("¿", current_row)){
      hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(str_split(current_row, "¿", n=2)[[1]][2]," ", n=2)[[1]][1]), after = length(hanna_message_dates))
    }
    else{
      hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(str_split(current_row, " ", n=2)[[1]][1]," ", n=2)[[1]][1]), after = length(hanna_message_dates))
    }
  }
}

## Text 2 (Google Hangouts): 04-09-2018 to 01-26-2021
for(i in 1:length(text2)){
  current_row <- text2[i]
  if(grepl(who, current_row)||unfiltered){
    if(grepl("¿", current_row)){
      hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(str_split(current_row, "¿", n=2)[[1]][2]," ", n=2)[[1]][1]), after = length(hanna_message_dates))
    }
    else{
      hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(str_split(current_row, " ", n=2)[[1]][1]," ", n=2)[[1]][1]), after = length(hanna_message_dates))
    }
  }
}

## Text 2b (Google Hangouts): 01-27-2021 to 02-21-2021
for(i in 1:length(text2b)){
  current_row <- text2b[i]
  if(grepl(who, current_row)||unfiltered){
    if(grepl("¿", current_row)){
      hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(str_split(current_row, "¿", n=2)[[1]][2]," ", n=2)[[1]][1]), after = length(hanna_message_dates))
    }
    else{
      hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(str_split(current_row, " ", n=2)[[1]][1]," ", n=2)[[1]][1]), after = length(hanna_message_dates))
    }
  }
}

# Text 3 (Signal): 02-16-2021 to 09-14-2021
for(i in 1:length(text3)){
  current_row <- text3[i]
  if(grepl(who, current_row)||unfiltered){
    hanna_message_dates <- append(hanna_message_dates, as.Date(str_split(current_row, ",", n=2)[[1]][1], "%m/%d/%Y"), after = length(hanna_message_dates))
    test_vector <- append(test_vector, as.Date(str_split(current_row, ",", n=2)[[1]][1], "%m/%d/%Y"), after = length(test_vector))
  }
}

# Text 4 (Instagram): 03-11-2018 to 02-05-2021
for (i in 1:nrow(text4)){
  current_row <- text4[i,]
  if(grepl(who_insta, current_row$sender_name)||unfiltered){
    hanna_message_dates <- append(hanna_message_dates, as.Date(as.POSIXct(current_row$timestamp_ms/1000, origin="1970-01-01", timezone="America/New_York")), after = length(hanna_message_dates))
    #test_vector <- append(test_vector, as.Date(as.POSIXct(current_row$timestamp_ms/1000, origin="1970-01-01", timezone="America/New_York")), after = length(test_vector))
  }
}

# Text 5 (Instagram): 02-06-2021 to 09-15-2021
for (i in 1:nrow(text5)){
  current_row <- text5[i,]
  if(grepl(who_insta, current_row$sender_name)||unfiltered){
    hanna_message_dates <- append(hanna_message_dates, as.Date(as.POSIXct(current_row$timestamp_ms/1000, origin="1970-01-01", timezone="America/New_York")), after = length(hanna_message_dates))
    #test_vector <- append(test_vector, as.Date(as.POSIXct(current_row$timestamp_ms/1000, origin="1970-01-01", timezone="America/New_York")), after = length(test_vector))
  }
}


hist(hanna_message_dates, "days", freq=TRUE, main="Messages per Day", las=2, xlab="")
