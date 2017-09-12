library(tidyverse)
library(stringr)
library(rvest)
library(httr)
library(gridExtra)

#importing data
setwd("goodreads")
gr_raw <- read_csv("https://raw.githubusercontent.com/bernardkung/goodreads/master/goodreads_library_export.csv")
gr <- gr_raw
head(gr)
names(gr)


#fix dataframe
gr2 <- select(gr, 1:10, 12:16, 19)
names(gr2) <- gsub(x = names(gr2), pattern = " ", replacement = "")
gr2$YearRead <- substr(gr2$DateRead, 1,4)
names(gr2)

#check ISBN
head(gr2$ISBN)

unique(sapply(gr2$ISBN, nchar))
unique(sapply(gr2$ISBN13, nchar))

# fix ISBN
gr3 <- gr2
gr3$ISBN <- str_extract(gr2$ISBN, "[[:digit:]]+")
gr3$ISBN13 <- str_extract(gr2$ISBN13, "[[:digit:]]+")
head(gr3$ISBN)

#filter Read books only
gr4 <- filter(gr3, ExclusiveShelf == "read")


# Sample Scrape Loop
# 
# genre_list <- list()
# 
# for (i in 1:5) {
#   url_base <- 'https://www.goodreads.com/book/show/'                  # Sets base URL
#   url <- paste(url_base, gr4[i,]$BookId, sep = "")                    # paste final URL together and read HTML
#   xurl <- GET(url, add_headers('user-agent' = 'r'))
#   webpage <- read_html(url)                                                         
#   genre_data_html <- html_nodes(webpage, '.bookPageGenreLink')        # use CSS selectors to scrape genre links
#   genre_vect <- html_text(genre_data_html)                            # convert html data to text
#   
#   genre_list[[i]] <- genre_vect                                
# }
#
#genre_list

# Full Scrape Loop
# 
# # Initialize variables
# genre_df <- data.frame(BookId = numeric(nrow(gr)), Genre = character(nrow(gr)), stringsAsFactors = FALSE)
# 
# Sys.time()
# start <- 1
# end <- nrow(gr)
# pb <- txtProgressBar(min = start, max = end, style = 3)                             # text based progress bar
# 
# for (i in start:end) {
#   url_base <- 'https://www.goodreads.com/book/show/'                                # Sets base URL
#   url <- paste(url_base, gr[i,]$BookId, sep = "")                                   # paste final URL together and read HTML
#   xurl <- GET(url, add_headers('user-agent' = 'r'))
#   webpage <- read_html(url)                                                         
#   genre_data_html <- html_nodes(webpage, '.bookPageGenreLink')                      # use CSS selectors to scrape genre links
#   genre_vect <- html_text(genre_data_html)                                          # convert html data to text
#   
#   genre_df[i, 2] <- genre_vect[1]                                                   # first genre result stored as genre
#   genre_df[i, 1] <- as.numeric(gr[i,]$BookId)                                       # corresponding BookId
#   
#   genre_vect_addId <- c(as.numeric(gr[i,]$BookId), genre_vect)
#   
#   Sys.sleep(1)                                                                      # adds delay between requests
#   setTxtProgressBar(pb, i)
# }
# close(pb)
# Sys.time()


#Read scrape data from online CSV
genre_df_csv <- read_csv("https://raw.githubusercontent.com/bernardkung/goodreads/master/scrapes.csv")
genre_df <- genre_df_csv[, c(2,4)]
head(genre_df)

gr5 <-gr4 %>% left_join(genre_df, by = "BookId")


# Title errors from importing and exporting
gr_error <- gr4 %>% left_join(genre_df_csv, by = "BookId")
names(gr_error)
gr_error[which(!(gr_error$Title.x %in% gr_error$Title.y)), c("Title.x", "Title.y")]


# Stripping out Series
regex1 <- "\\((.*)\\)"
regex2 <- ",*[[:space:]]*#\\d.*$"
regex3 <- ",*[[:space:]]*#"

gr6 <- gr5


# Extracting series names out into a new column  
Series <- str_extract(gr5$Title, regex1) %>%                    # extract entire series based on parentheses
  gsub(pattern = regex1,replacement =  "\\1")   # remove parentheses

# Removing series name from Title column
gr6$Title <- gsub(regex1, "", gr_full$Title)

# Extracting number in series into new column
gr6$Series <- gsub(regex2, "\\2", Series)                       

gr6$SeriesNum <- str_extract(Series, regex2) %>%
  gsub(pattern = regex3, replacement = "")


head(gr6[which(!is.na(gr6$Series)), c("Title", "Series", "SeriesNum")])


# anomalies
gr6[302,c("Title", "Series", "SeriesNum")]      # Includes omnibus novellas/novelettes
gr6[213,c("Title", "Series", "SeriesNum")]      # Book 3, Part 1
gr6[130,c("Title", "Series", "SeriesNum")]      # Prequel with unique series numbering
gr6[252,c("Title", "Series", "SeriesNum")]      # 6 Books stored as one entry, belonging to separate series 
# row 252 earlier had genre issues as well
# would probably excise this row, however series info is largely irrelevant


# Count genres
genres <- gr6 %>%
  group_by(Genre) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print(n = Inf)


# Just the books with rare genres (n < 4)
rare_genres <- gr6 %>% 
  group_by(Genre) %>%
  tally() %>%
  filter(n <= 4) %>%
  select(Genre)

# Examine these rare genre books
subset(gr6, gr6$Genre %in% unlist(rare_genres)) %>%
  select(Title, Genre) %>%
  group_by(Genre) %>%
  arrange(Genre)


# Reducing to 4 Genres
gr7 <- gr6

genres <- list()
genres[["Fiction"]] <- c("Young Adult", "Historical", "Thriller", 
                         "Mystery", "Classics", "Short Stories", "Childrens", 
                         "Romance", "Womens Fiction")
genres[["Fantasy"]] <- c("Horror", "Dungeons and Dragons", "Paranormal")
genres[["Science Fiction"]] <- c("Gothic", "Adventure", "Anthologies", "Sequential Art")
genres[["Nonfiction"]] <- c("History", "Food and Drink", "Philosophy", "Autobiography",
                            "Biography", "Poetry", "Writing")

for (i in 1:length(genres)) {
  gr7[gr6$Genre %in% genres[[i]], ]$Genre <- names(genres)[i]
}


# Verifying genre reduction
gr7 %>%
  group_by(Genre) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print(n = Inf)

ggplot(gr7, aes(Genre, fill = Genre)) + geom_bar()


# Initial Distribution per MyRating
ggplot(gr7, aes(x= MyRating)) +
  geom_bar(fill = "lightblue") +
  ggtitle("Initial Distribution by MyRating") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

# Initial AverageRating~MyRating Plot
plot1 <- ggplot(gr7, aes(x= MyRating, fill= YearRead)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = 0:5) +
  ggtitle("Fig 1a: Books per MyRating by YearRead") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))
print(plot1)


# Stripping out 0 Rating books
gr8 <- gr7[-which(gr7$MyRating == 0),]

plot2 <- ggplot(gr8, aes(x=MyRating, fill = YearRead)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = 0:5) +
  ggtitle("Fig 1b: Rated Books per MyRating by YearRead") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

print(plot2)

plot3 <- ggplot(gr8, aes(x=MyRating, fill= Genre)) +
  geom_bar(position="dodge") + 
  ggtitle("Fig 2: Distribution of MyRating by Genre") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

print(plot3)

#### Analysis
# General distribution of across YearRead and Genre
# Is there a relationship between AverageRating and MyRating
# Is this relationship affected by Genre?
# Is this relationship affected by YearRead?

plot4 <- ggplot(gr8, aes(x=MyRating, fill= Genre)) +
  geom_bar(position="dodge") + 
  facet_grid(~Genre) +
  ggtitle("Distribution of MyRating by Genre") +
  theme(plot.title = element_text(size= 11, hjust= 0.696))

plot5 <- ggplot(gr8, aes(x=MyRating, fill= YearRead)) +
  geom_bar(position="dodge") +
  facet_wrap(~YearRead) + 
  ggtitle("Distribution of MyRating by YearRead") +
  theme(plot.title = element_text(size= 11, hjust= 0.696))

grid.arrange(plot4, plot5, top= "Fig 3: Exploratory Distributions")

plot_hist <- ggplot(gr8, aes(x=AverageRating)) +
    geom_histogram(bins = 30, fill = "lightgreen") +
    ggtitle("AverageRating Histogram")

plot_density <- ggplot(gr8, aes(x=AverageRating)) +
  geom_density(aes(y=..scaled..), fill="lightgreen", linetype = 0) +
  ggtitle("AverageRating Density Plot")

grid.arrange(plot_hist, plot_density, top = "Distribution of AverageRating")

#### Linear Regression in Ratings
plot6 <- ggplot(gr8, aes(x=MyRating, y=AverageRating, group=MyRating))+
  geom_boxplot() +
  scale_y_continuous(limits = c(2, 5)) +
  ggtitle("Distribution of AverageRating by MyRating") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

plot7 <- ggplot(gr8, aes(x=MyRating, y=AverageRating)) +
  geom_point(pos= position_jitter(0.04), alpha= 0.3, shape= 16, size= 3) +
  geom_smooth(method = "lm", se = F) + 
  scale_y_continuous(limits = c(2, 5)) + 
  scale_x_continuous(breaks = 0:5) +
  ggtitle("Linear Regression of AverageRating by MyRating") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

grid.arrange(plot6, plot7, top= "Fig 3: Exploring Relationship between AverageRating and MyRating")


lm_rating <- lm(AverageRating ~ MyRating, gr8)
anova(lm_rating)
coef(lm_rating)[1]



# LinReg by Genre
plot7 <- ggplot(gr8, aes(MyRating, AverageRating)) +
  geom_point(position = position_jitter(0.2), alpha = 0.5, aes(color = Genre)) +
  geom_segment(aes(x=1, xend=5, 
                   y= coef(lm_rating)[1] + coef(lm_rating)[2], 
                   yend= coef(lm_rating)[1] + length(unique(gr8$MyRating))*coef(lm_rating)[2]),
               size = 1) +
  facet_grid(. ~ Genre) +
  geom_smooth(method = "lm", se = F, aes(color = Genre)) +
  scale_y_continuous(limits = c(2, 5)) + 
  scale_x_continuous(breaks = 0:5) +
  ggtitle("Fig 4: AverageRating by MyRating for each Genre") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

print(plot7)


# LinReg by YearRead
plot8 <- ggplot(gr8, aes(MyRating, AverageRating)) +
  geom_point(position = position_jitter(0.2), alpha = 0.5, aes(color = YearRead)) +
  geom_segment(aes(x=1, xend=5, 
                   y= lm_rating[[1]][1] + lm_rating[[1]][2], 
                   yend= lm_rating[[1]][1] + 5*lm_rating[[1]][2]),
               size = 1) +
  facet_grid(. ~ YearRead) +
  geom_smooth(method = "lm", se = F, aes(color = YearRead)) +
  scale_x_continuous(breaks = 0:5) +
  ggtitle("Fig 4: AverageRating by MyRating for each YearRead") +
  theme(plot.title = element_text(size= 11, hjust= 0.5))

print(plot8)



#### WIP

gr8 %>% group_by(YearRead) %>%
  ggplot(aes(MyRating, AverageRating, color = Genre, fill = Genre)) +
  geom_point(position = position_jitter(0.2), alpha = 0.5) +
  facet_grid(. ~ YearRead) +
  geom_smooth(method = "lm", se = F) + 
  ggtitle("Fig: Rating Relationship by Year and Genre") +
  theme(plot.title = element_text(size= 11, hjust= 0.5)) +
  scale_y_continuous(limits = c(2, 5)) + 
  scale_x_continuous(breaks = 0:5)



gr8[which(gr8$YearRead %in% c(2016, 2017)),]%>% group_by(YearRead) %>%
  ggplot(aes(MyRating, AverageRating, color = Genre, fill = Genre)) +
  geom_point(position = position_jitter(0.1)) +
  facet_grid(. ~ YearRead) +
  geom_smooth(method = "lm", se = F) + 
  scale_y_continuous(limits = c(2, 5)) + 
  scale_x_continuous(breaks = 0:5)








