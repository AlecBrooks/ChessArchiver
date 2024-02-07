#https://api.chess.com/pub/player/katanabladez0/games/archives
#https://api.chess.com/pub/player/Finished_Milk/games/archives
#https://api.chess.com/pub/player/katanabladez0/games/2008/06
#https://www.kaggle.com/datasets/jramponi/chess-mapping-file-elo-code-to-opening-name

#install.packages("tidyverse")
#install.packages("jsonlite")
#install.packages("stringr")
#install.packages("data.table")

library(jsonlite)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
conf_user <- readLines("chess.conf")
print(paste("Pulling archive list for",gsub(' ','',gsub(".*=","",conf_user)),". . ."))
df <- fromJSON(gsub(' ','',paste("https://api.chess.com/pub/player/",gsub(' ','',gsub(".*=","",conf_user)),"/games/archives")))
archives <- as.data.frame(df[["archives"]])
remove(df)
Matrix <- matrix(1:23, nrow = 0, ncol = 23)
col_names <- c("Event", "x2", "x3","x4","white_id","black_id", "winner","x5","x6","opening_eco","x7","date","x8",
                "white_rating","black_rating","x9", "x10","x11","x12","x13","x14","x15","moves")
gr <- data.frame(matrix = Matrix)
colnames(gr) <- col_names
for (x in archives$`df[["archives"]]`) {
  temp_archive <- fromJSON(x)
  games <- temp_archive[["games"]][["pgn"]]
  for (y in games) {
    gr[nrow(gr) + 1,] <- as.data.frame(str_split_fixed(y, "\n", 23))
  }
  print(paste("Downloading game archives for",gsub(' ','',gsub(".*=","",conf_user)),". . ."))
    print(x)
    remove(temp_archive,games,x,y)
  }
remove(archives,Matrix,col_names)
#cleaning output
print("Cleaning Download. . .")
gr1 <- gr
gr1$Event <- gsub('"','',gr1$Event)
gr1 <- filter(gr1, gr1$Event =="[Event Let's Play!]" | gr1$Event =="[Event Live Chess]")
gr2 <- gr1[,-c(1,2,3,4,8,9,11,13,16,17,18,19,20,21,22)]
gr2$moves <- str_remove_all(gr2$moves, "\n")
gr2$moves <- paste('[Moves"',gr2$moves,'"]')
my_fun <- function(x) {  
  x <- stri_extract_all_regex(x, '(?<=").*?(?=")') 
}
gr2[] <- lapply(gr2, my_fun)
remove(my_fun)
gr2$winner[gr2$winner=="0-1"] <- "black"
gr2$winner[gr2$winner=="1-0"] <- "white"
gr2$winner[gr2$winner=="1/2-1/2"] <- "draw"
gr2$white_id <- gsub(' ','',gr2$white_id)
gr2$black_id <- gsub(' ','',gr2$black_id)
gr2$winner <- gsub(' ','',gr2$winner)
gr2$opening_eco <- gsub(' ','',gr2$opening_eco)
gr2$date <- gsub('\\.','/',gr2$date)
gr2$date <- gsub(' ','',gr2$date)
gr2$date <- as.Date(gr2$date, format = "%Y/%m/%d")
gr2$white_rating <- gsub(' ','',gr2$white_rating)
gr2$white_rating <- as.numeric(as.character(gr2$white_rating))
gr2$black_rating <- gsub(' ','',gr2$black_rating)
gr2$black_rating <- as.numeric(as.character(gr2$black_rating))
gr2$moves <- paste('"',gr2$moves,'"')
#name ECO codes
big_elo_map <- read_csv("big_elo_map.csv")
colnames(big_elo_map) <- c("opening_eco","opening_name")
gr2 <- merge(gr2,big_elo_map,by="opening_eco",all.x = TRUE)
gr2 <- na.omit(gr2)
gr2$avg_rating <-  round((gr2$white_rating + gr2$black_rating)/2,0)
gr2$count <- 1
gr2$white_win <- 0
gr2$black_win <- 0
gr2$game_draw <- 0
gr2$white_win[gr2$winner=="white"] <- 1
gr2$black_win[gr2$winner=="black"] <- 1
gr2$game_draw[gr2$winner=="draw"] <- 1
#building summary
print("building summary. . .")
gameSummary <- data.table(gr2)
gameSummary <- gameSummary[,list(freq=sum(count), avg_elo=sum(avg_rating), white_win=sum(white_win),black_win=sum(black_win),game_draw=sum(game_draw)), by='opening_name']
gameSummary$avg_elo <- round(gameSummary$avg_elo / gameSummary$freq,0)
gameSummary$white_win <- round((gameSummary$white_win/gameSummary$freq),5)
gameSummary$black_win <- round((gameSummary$black_win/gameSummary$freq),5)
gameSummary$game_draw <- round((gameSummary$game_draw/gameSummary$freq),5)
#finishing up
MyGames <- gr2[,-c(11,12,13,14)]
MyGames <- select(MyGames,date,white_id,black_id,white_rating,black_rating,avg_rating,winner,moves,opening_eco,opening_name)
print("Saving to csv. . .")
write_csv(MyGames, gsub(' ','',paste("output/",gsub(' ','',gsub(".*=","",conf_user)),"_Games.csv")))
write_csv(gameSummary, gsub(' ','',paste("output/",gsub(' ','',gsub(".*=","",conf_user)),"_openingSummary.csv")))
remove(gr)
remove(gr1,gr2,big_elo_map,MyGames,gameSummary,conf_user)
print("DONE")
