## 

# Loading the rvest package for web-scraping
library(tidyverse)
library(stringr)
library(httr)
library(rvest)

# Initialization code
setwd("F:/Media/Google Drive/GitHub/goodreads")
gr_raw <- read_csv("goodreads_library_export.csv")
gr <- gr_raw
gr <- select(gr, 1:10, 12:16, 19)
names(gr) <- gsub(x = names(gr), pattern = " ", replacement = "")
gr$YearRead <- substr(gr$DateRead, 1,4)
gr$ISBN <- str_extract(gr$ISBN, "[[:digit:]]+")
gr$ISBN13 <- str_extract(gr$ISBN13, "[[:digit:]]+")
gr <- filter(gr, ExclusiveShelf == "read")


# Initialize variables
genre_df <- data.frame(BookId = numeric(nrow(gr)), Genre = character(nrow(gr)), stringsAsFactors = FALSE)
genre_list <- list()

Sys.time()
start <- 1
end <- nrow(gr)
pb <- txtProgressBar(min = start, max = end, style = 3)                             # text based progress bar

for (i in start:end) {
  url_base <- 'https://www.goodreads.com/book/show/'                                # Sets base URL
  url <- paste(url_base, gr[i,]$BookId, sep = "")                                   # paste final URL together and read HTML
  xurl <- GET(url, add_headers('user-agent' = 'r'))
  webpage <- read_html(url)                                                         # 
  genre_data_html <- html_nodes(webpage, '.bookPageGenreLink')                      # use CSS selectors to scrape genre links
  genre_vect <- html_text(genre_data_html)                                          # convert html data to text
  
  genre_df[i, 2] <- genre_vect[1]                                                   # first genre result stored as genre
  genre_df[i, 1] <- as.numeric(gr[i,]$BookId)                                       # corresponding BookId
  
  genre_vect_addId <- c(as.numeric(gr[i,]$BookId), genre_vect)
  genre_list[[i]] <- genre_vect_addId                                               # store in redundant list
  
  Sys.sleep(1)                                                                      # adds delay between requests
  setTxtProgressBar(pb, i)
}
close(pb)
Sys.time()

