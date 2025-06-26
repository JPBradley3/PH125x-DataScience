##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#######################################################################################################



#######################################################################################################

### 4.1.1 Wrangling & Cleaning

#-   Verify userId, movieId, rating\
#-   Handle duplicates, missing\
#-   Check rating bounds\
#-   Parse title for year\
#-   Convert timestamp\
#-   Split genres \newpage

#######################################################################################################


### Verify userId, movieId, rating\
str(edx)
summary(edx)
sum(is.na(edx$userId))
sum(is.na(edx$movieId))
sum(is.na(edx$rating))

###   Handle duplicates, missing\
sum(duplicated(edx))

###   Check rating bounds\
edx <- edx %>% filter(rating >= 0.5 & rating <= 5)

###   Parse title for year\
edx <- edx %>%
  mutate(year_text = str_extract(title, "[0-9]{4}"),
         year = as.integer(year_text))

edx$year <- as.integer(
  ifelse(
    substr(edx$title, nchar(edx$title) - 5, nchar(edx$title) - 5) == "(",
    substr(edx$title, nchar(edx$title) - 4, nchar(edx$title) - 1),
    NA
  )
)

###   Convert timestamp\
library(lubridate)

edx <- edx %>%
  mutate(date = as_datetime(timestamp))

###   Split genres \newpage
edx_long <- edx %>%
  separate_rows(genres, sep = "\\|")


#######################################################################################################

### 4.1.2 Bias Audit



#-   Coverage by genre/time\
#-   Popularity bias\
#-   Underrepresented groups\
#-   User frequency analysis \newpage

#######################################################################################################

###   Rating distribution\
#-   Rating distribution\
library(ggplot2)

# Rating distribution plot
ggplot(edx, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "dodgerblue", color = "black") +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  labs(title = "Distribution of Movie Ratings",
       x = "Rating",
       y = "Count") +
  theme_minimal()

###   Coverage by genre/time\
#### Number of ratings per genre
edx_long %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(reorder(genres, count), count)) +
  geom_col(fill = "dark red") +
  coord_flip() +
  labs(title = "Ratings Count per Genre",
       x = "Genre", y = "Number of Ratings") +
  theme_minimal()
#### Ratings per Movie Release Year
edx %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line(color = "dark red") +
  labs(title = "Number of Ratings by Release Year",
       x = "Release Year", y = "Number of Ratings") +
  theme_minimal()
#### Ratings per rating year (when rated)
edx %>%
  mutate(rating_year = year(date)) %>%
  group_by(rating_year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rating_year, y = count)) +
  geom_line(color = "dark red") +
  labs(title = "Number of Ratings per Year (by Rating Date)",
       x = "Year", y = "Number of Ratings") +
  theme_minimal()

###   Popularity bias\
# Number of ratings per movie (popularity)
movie_popularity <- edx %>%
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Top 10 most-rated movies
head(movie_popularity, 10)

# Plot popularity distribution (log scale, as highly skewed)
ggplot(movie_popularity, aes(x = count)) +
  geom_histogram(bins = 50, fill="salmon", color="black") +
  scale_x_log10() +
  labs(title = "Distribution of Movie Popularity (Number of Ratings)",
       x = "Number of Ratings (log scale)", y = "Count of Movies") +
  theme_minimal()

###   Underrepresented groups\
# Underrepresented movies (e.g., less than 10 ratings)
low_count_movies <- movie_popularity %>% filter(count < 10)
nrow(low_count_movies)                # How many?
summary(low_count_movies$count)       # Rating count stats

# Underrepresented genres
edx_long %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(count) %>% head(10)

# Underrepresented/rare users
user_freq <- edx %>% group_by(userId) %>% summarise(count = n())
summary(user_freq$count)
sum(user_freq$count < 10)  # Number of users with <10 ratings

###   User frequency analysis \newpage
# Histogram of number of ratings by user
ggplot(user_freq, aes(x = count)) +
  geom_histogram(bins = 50, fill="steelblue", color="black") +
  scale_x_log10() +
  labs(title = "Distribution of Number of Ratings per User",
       x = "Number of Ratings (log scale)", y = "Count of Users") +
  theme_minimal()

# Users with the most ratings (top 10)
user_freq %>% arrange(desc(count)) %>% head(10)

###   Data Sparsity
# User-movie sparsity (matrix plot)
sample_users <- sample(unique(edx$userId), 100)
sample_movies <- sample(unique(edx$movieId), 100)
matrix_sample <- edx %>%
  filter(userId %in% sample_users, movieId %in% sample_movies)

ggplot(matrix_sample, aes(x = factor(movieId), y = factor(userId))) +
  geom_tile(fill = "grey20") +
  labs(title = "Sample User-Movie Rating Matrix (Sparse)",
       x = "Movie", y = "User") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())


#######################################################################################################

### 4.1.3 Exploratory Trends

#-   Rating by genre/year\
#-   Time-based changes\
#-   Activity trends\
#-   Genre vs decade\
#-   Age/variance scatter \newpage

#######################################################################################################

###   Rating by genre/year\
library(dplyr)
library(ggplot2)

# 1. Select top N genres by volume
topN <- 8
top_genres <- edx_long %>%
  count(genres, sort = TRUE) %>%
  slice_max(order_by = n, n = topN) %>%
  pull(genres)

# 2. Compute average rating and filter
genre_year_avg <- edx_long %>%
  filter(genres %in% top_genres, !is.na(year)) %>%
  group_by(genres, year) %>%
  summarize(mean_rating = mean(rating), count = n(), .groups = 'drop') %>%
  filter(count >= 500)

# 3. Plot, using smooth lines, readable colors
ggplot(genre_year_avg, aes(x = year, y = mean_rating, color = genres)) +
  geom_smooth(se = FALSE, span = 0.4, size = 1.2) +
  labs(title = "Average Rating by Genre Over Year (Top Genres, Smoothed)",
       x = "Release Year", y = "Average Rating", color = "Genre") +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  theme_minimal() +
  theme(legend.position = "right")

###   Time-based changes\
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Prepare the time-based summary
time_stats <- edx %>%
  mutate(rating_year = year(date)) %>%
  group_by(rating_year) %>%
  summarize(
    mean_rating = mean(rating),
    n_ratings = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(rating_year))

# Pivot for two-panel (facet) plotting
plot_data <- time_stats %>%
  select(rating_year, n_ratings, mean_rating) %>%
  pivot_longer(cols = c(n_ratings, mean_rating),
               names_to = "metric", values_to = "value")

# Human-friendly labels
plot_data$metric <- recode(plot_data$metric,
                           n_ratings = "Number of Ratings",
                           mean_rating = "Mean Rating")

# Two-panel plot
ggplot(plot_data, aes(x = rating_year, y = value)) +
  geom_line(size = 1.2, color = "steelblue") +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Rating Volume and Average Over Time",
    x = "Year",
    y = NULL
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

###   Activity trends\
activity_stats <- edx %>%
  mutate(rating_year = year(date)) %>%
  group_by(rating_year) %>%
  summarize(
    active_users = n_distinct(userId),
    active_movies = n_distinct(movieId),
    .groups = "drop"
  ) %>%
  filter(!is.na(rating_year))

activity_long <- activity_stats %>%
  pivot_longer(cols = c(active_users, active_movies),
               names_to = "activity", values_to = "count") %>%
  mutate(activity = recode(activity,
                           active_users = "Active Users",
                           active_movies = "Active Movies"))

ggplot(activity_long, aes(x = rating_year, y = count, color = activity)) +
  geom_line(size = 1.2) +
  labs(title = "Active Users and Movies per Year",
       x = "Year", y = "Count", color = "Activity") +
  theme_minimal()

###   Genre vs decade\
top_genres <- edx_long %>%
  count(genres, sort = TRUE) %>%
  slice_max(n, n = 8) %>%
  pull(genres)

genre_decade_stats <- edx_long %>%
  filter(genres %in% top_genres, !is.na(year)) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(genres, decade) %>%
  summarize(count = n(), .groups = 'drop')

ggplot(genre_decade_stats, aes(x = decade, y = count, fill = genres)) +
  geom_col(position = "dodge") +
  labs(title = "Top Genres Over Decades",
       x = "Decade", y = "Ratings Count", fill = "Genre") +
  theme_minimal()

###   Age/variance scatter \newpage






