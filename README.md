# goodreads

### Introduction

Goodreads is a social networking website focused on books; the basic idea is to allow users to rate and review books and discover new books and friends. 
My goal for this project is to analyze the books I've read to see if I can gather any meaningful insight on how my ratings compare based on year the book was read, average ratings for books, and the genre of book. 

### Project Status

In Progress

### Instructions

Project is being developed in Jupyter notebook for R. It will additionally be available as an R file.

### Reflection

This was an interesting exploration for me. Most of the data was available to me as a .csv exported directly from goodreads, but genre data had to be scraped directly from html. I did this in R, but while reading to solve some issues it became clear that this task would've been better suited for Python. 

Additionally, there's room for more sophisticated approaches in three areas:

1. I classified each book based only on its most popular genre tag. This was because goodreads presents both genres and sub-genres, and I had difficulty separating sub-genres in a meaningful way. Going forward, I'd prefer to work with this full information.
2. I manually reduced the number of genres. It was okay here because I was familiar with each book, and the data set was small. Going forward, I would definitely prefer to explore classifying books algorithmically. Ideally, I'd look at the number of genre tags and use that to train a supervised learning algorithm.
3. My analysis focuses primarily on use of linear regression. Based on the categorical nature of a 1-5 star rating system, it might be better to explore a supervised learning algorithm again, even if it's only based on three variables. 
