# Tidy wordcloud parsing script

library(tidytext)
library(readr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(jsonlite)
library(dplyr)
library(tidytext)


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



write_lines(hanna_rows, paste(path, "hanna_lines.txt", sep=""),sep = "\n", append = FALSE)
dat <- read.table(paste(path, "hanna_lines.txt", sep=""), header = FALSE, fill = TRUE)
tidy_dat <- tidyr::gather(dat, key, word)
gc()

# tokenize
tokens <- tidy_dat %>% 
  unnest_tokens(word, word) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup()

# remove stop words
data("stop_words")
tokens_clean <- tokens %>%
  anti_join(stop_words)

# remove numbers
nums <- tokens_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()

tokens_clean <- tokens_clean %>% 
  anti_join(nums, by = "word")

# remove unique stop words that snuck in there
uni_sw <- data.frame(word = c("com", "imgur", "gallery", "lol", "just", "like", "okay", "ok", 
                              "lhgoogleusercontentcom", "https", "wwwreddit", "http", "wwwredditcom", "yeah", 
                              "also", "imgurcom", "message", "reacted", "dont", "youre", "ill", "can", 
                              "thats", "get", "â", "ã", "googleusercontent.com", "lh3", "s0", "å", "imgur.com", 
                              "pã", "www.bonappetit.com", "xdã", "xdâ", "xd", "www.reddit.com", "pete"))

tokens_clean <- tokens_clean %>% 
  anti_join(uni_sw, by = "word")

tokens_clean %>% head(10)

# define a nice color palette
pal <- brewer.pal(8,"Dark2")

# plot the 50 most common words
#dev.new(width = 1000, height = 1000, unit = "px")
set.seed(1234)
tokens_clean %>% 
  with(wordcloud(word, n, colors=pal, min.freq = 1, scale = c(2.5,0.8),
                 max.words=200, random.order=FALSE, rot.per=0.0,))
