

## Initialization
library(tidyverse)
library(stringr)

setwd("F:/Media/Google Drive/GitHub/goodreads")

gr_raw <- read_csv("goodreads_library_export.csv")

gr <- gr_raw

# Considerations at this point:
# Clean up the ISBN and ISBN13 columns. Import misread some formatting(?) data as characters and I want to isolate the numbers
# Prune columns that are empty and not interesting to me (e.g. Condition, Spoiler, etc)
# Augment data with columns that would be interesting (e.g. Genre)

### Excising empty/uninteresting columns
gr <- select(gr, 1:10, 12:16, 19)

### Cleaning up column names
names(gr) <- gsub(x = names(gr), pattern = " ", replacement = "")

### Binning date read by year
gr$YearRead <- substr(gr$DateRead, 1,4)


### Cleaning ISBNs
# type1: ="0802142990"
# type2: =""

# Checking ISBN lengths for consistency
# unique(sapply(gr$ISBN, nchar))
# unique(sapply(gr$ISBN13, nchar))

# Re-writes with extracted digits or NAs out of ISBN and ISBN13
gr$ISBN <- str_extract(gr$ISBN, "[[:digit:]]+")
gr$ISBN13 <- str_extract(gr$ISBN13, "[[:digit:]]+")

### Filter out to-read books
gr <- filter(gr, ExclusiveShelf == "read")

##
## Scraping genres with gr_scrape.R
##

gr_title_genre <- gr[, 1:2] %>% left_join(genre_df, by = "BookId")

gr_full <-gr %>% left_join(genre_df, by = "BookId")
# gr_backup <- gr_full
write.csv(file="gr_backup.csv", x=gr_backup)

# Let's take a look at the scraped genres
genres <- gr_full %>%
  group_by(Genre) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print(n = Inf)

# Just the books with rare genres (n < 4)
rare_genres <- gr_full %>% 
  group_by(Genre) %>%
  tally() %>%
  filter(n <= 4) %>%
  select(Genre)

# Quick Aside:
# Let's shorten the title lengths
# Simple and potentially meaningful: extract series names 
# Complications:
# sometimes there's a comma before the series number
# sometimes there isn't a # before the series number
# sometimes the series number isn't a single number

# Extracting series names out into a new column  
Series <- str_extract(gr_full$Title, "\\(.*\\)") %>%                    # extract entire series based on parentheses
                  gsub(pattern = "\\((.*)\\)",replacement =  "\\1")     # remove parentheses

# Removing series name from Title column
gr_full$Title <- gsub("\\((.*)\\)", "", gr_full$Title)

# Extracting number in series into new column
gr_full$Series <- gsub(",*[[:space:]]*#\\d.*$", "\\2", Series)     # keep just the series name

gr_full$SeriesNum <- str_extract(Series, ",*[[:space:]]*#\\d.*$") %>%
  gsub(pattern = ",*[[:space:]]*#", replacement = "")

# anomalies
gr_full[302,]   # Includes omnibus novellas/nevelettes
gr_full[213,]   # Book 3, Part 1
gr_full[130,]   # Prequel with unique series numbering
gr_full[252,]   # 6 Books stored as one entry, belonging to separate series 
                # row 252 earlier had genre issues as well
                # would probably excise this row, however series info is largely irrelevant

# Examine these rare genre books
subset(gr_full, gr_full$Genre %in% unlist(rare_genres)) %>%
  select(Title, Genre) %>%
  arrange(Genre) %>%
  print(n = Inf)

# Wow, women's fiction. Let's just rectify that to Fiction
gr_full[gr_full$Genre == "Womens Fiction", ]$Genre <- c("Fiction")

# Generalize specific nonfiction genres back to just nonfiction
nonfiction <- c("Philosophy", "Autobiography", "Biography", "Food and Drink", "Writing")
gr_full[gr_full$Genre %in% nonfiction, ]$Genre <- c("Nonfiction")

# Manually rectifying some other genres
gr_full[gr_full$Genre == "Gothic", ]$Genre <- c("Horror")
gr_full[gr_full$Genre %in% c("Paranormal", "Dungeons and Dragons", "Adventure"), ]$Genre <- c("Fantasy")
gr_full[gr_full$Genre == "Anthologies", ]$Genre <- c("Science Fiction")
gr_full[gr_full$Genre == "Sequential Art", ]$Genre <- c("Graphic Novel")

# On the other hand, de-generalizing Fiction books
gr_full[gr_full$Genre == "Fiction", ]%>%
  print(n = Inf)

# Respecify a few books by title
gr_full[gr_full$Title == "Stories for Men: An Anthology", ]$Genre <- c("Short Stories")
gr_full[gr_full$Title == "Syrup", ]$Genre <- c("Romance")

# Respecify several authors, some with multiple books
authors <- list()
authors[["Fantasy"]] <- c("J.R.R. Tolkien", "J.K. Rowling", "China Miéville", "George R.R. Martin")
authors[["Mystery"]] <- c("Stieg Larsson")
authors[["Thriller"]]<- c("Robert Ludlum", "Dan Brown", "Tom Clancy")
authors[["Romance"]] <- c("Emily Giffin")
authors[["Science Fiction"]] <- c("Ted Chiang", "Neal Stephenson", "Margaret Atwood", "Iain M. Banks", "William Gibson")

for (i in 1:length(authors)) {
  gr_full[gr_full$Author %in% authors[[i]] & gr_full$Genre == "Fiction", ]$Genre <- names(authors)[i]
  # condition is augmented so we don't reassign books outside of the Ficion genre by these authors
}

# Initial Plot: Still too many genres
ggplot(gr_full, aes(x = MyRating, y= AverageRating, color = Genre)) + 
  geom_jitter()


# Common Genres
common_genres <- gr_full %>%
  group_by(Genre) %>%
  tally() %>%
  filter(n > 4) %>%
  arrange(desc(n)) %>%
  print(n = Inf)


subset(gr_full, Genre%in% unlist(common_genres)) %>%
  ggplot(aes(x = MyRating, y= AverageRating, color = Genre)) + 
  geom_jitter()


lm_genre <- lm(formula = AverageRating~MyRating, 
               data = gr_full %>% group_by(Genre),
               subset = Genre %in% unlist(common_genres))

subset(gr_full, Genre%in% unlist(common_genres)) %>%
  ggplot(aes(MyRating, AverageRating, color = Genre)) + 
  geom_jitter() +
  geom_smooth(aes(MyRating, AverageRating))

# Reducing to 4 Genres
fic_genres <- c("Horror", "Young Adult", "Historical", "Thriller", 
                "Mystery", "Classics", "Short Stories", "Childrens", 
                "Graphic Novel", "Romance", "Adventure")
nonfic_genres <- c("History", "Poetry")

gr_4genre <- gr_full
gr_4genre[gr_4genre$Genre %in% fic_genres, ]$Genre <- c("Fiction")
gr_4genre[gr_4genre$Genre %in% nonfic_genres, ]$Genre <- c("Nonfiction")

# checking
gr_4genre %>%
  group_by(Genre) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print(n = Inf)

# regraphing
  ggplot(gr_4genre, aes(MyRating, AverageRating, color = Genre)) + 
  geom_jitter() +
  geom_smooth(aes(MyRating, AverageRating, fill = Genre))

  gr_4genre %>% group_by(Genre) %>%
    ggplot(aes(MyRating, AverageRating, color = Genre)) +
    geom_point() +
    geom_smooth(aes(MyRating, AverageRating, fill = Genre), alpha = 0.2) +
    facet_grid(. ~ Genre)

gr_4genre %>%
    ggplot(aes(MyRating, AverageRating)) +
      geom_boxplot() +
      facet_grid(. ~Genre)
    