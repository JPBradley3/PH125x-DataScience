# MOVIELENS CAPSTONE CONSOLIDATED CODE

# 1. Load libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(knitr)
library(lubridate)
library(tidyr)

# 2. Download and load MovieLens 10M data
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file)) unzip(dl, ratings_file)
movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file)) unzip(dl, movies_file)

ratings <- as.data.frame(stringr::str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))
movies <- as.data.frame(stringr::str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# 3. Split into edx and final_holdout_test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# 4. Create train/test split for modeling
set.seed(1, sample.kind="Rounding")  # Set seed for reproducibility (ensures same partition each run)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)  # Randomly select 20% of the data for test set
train_set <- edx[-test_index,]  # The remaining 80% forms the training set
test_set <- edx[test_index,]    # This is the test set
# Ensure test set only contains users and movies also present in train_set (no new/unseen users or movies)
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
# After test/train split rejoin genres to both sets:
genres_lkp <- edx %>% select(movieId, genres) %>% distinct()
train_set <- train_set %>% left_join(genres_lkp, by = "movieId")
test_set  <- test_set %>% left_join(genres_lkp, by = "movieId")

# 5. RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))  # Root of mean squared error, a common metric for accuracy
}

# 6. Baseline Model (Global Mean)
mu <- mean(train_set$rating)                     # Compute the overall average rating (global mean)
baseline_rmse <- RMSE(test_set$rating, mu)       # Predict this mean for all test cases, compute RMSE
rmse_results <- data.frame(
  method = "Just the average",                   # Save results in a table with method name and RMSE value
  RMSE = baseline_rmse
)

# 7. Movie Effect Model (regularized)
lambda <- 3  # Regularization parameter to penalize movies with few ratings
movie_avgs <- train_set %>%
  group_by(movieId) %>%  # Group data by movie
  summarize(
    b_i = sum(rating - mu) / (n() + lambda),  # Calculate average difference from global mean, regularized
    n_i = n()                                 # Number of ratings for the movie
  )
# Predict using movie effect (mu + movie bias) for each observation in test_set
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),   # If movie unseen, set effect to 0
         pred = mu + b_i) %>%                # Prediction is global mean plus movie effect
  pull(pred)
model_1_rmse <- RMSE(test_set$rating, predicted_ratings)
# Add model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie Effect (Regularized)",
                                     RMSE = model_1_rmse))

# 8. User Effect Model (regularized)
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(b_i = ifelse(is.na(b_i), 0, b_i)) %>%  # Ignore missing b_i by setting to 0
  group_by(userId) %>%
  summarize(
    b_u = sum(rating - mu - b_i) / (n() + lambda),  # Calculate average deviation for each user, adjusted for movie effect
    n_u = n()
  )
# Predict using movie and user effects (mu + movie bias + user bias)
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(b_i = ifelse(is.na(b_i), 0, b_i),    # Unseen movie => no effect
         b_u = ifelse(is.na(b_u), 0, b_u),    # Unseen user => no effect
         pred = mu + b_i + b_u) %>%
  pull(pred)
model_2_rmse <- RMSE(test_set$rating, predicted_ratings)
# Add model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + User Effects (Regularized)",
                                     RMSE = model_2_rmse))

# 9. Genre Effect Model (fast version with movie-level genre proxy)

# After your test/train split:
genres_lkp <- edx %>% select(movieId, genres) %>% distinct()
train_set <- train_set %>% left_join(genres_lkp, by = "movieId")
test_set  <- test_set %>% left_join(genres_lkp, by = "movieId")

genre_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  group_by(movieId, genres) %>%
  summarize(residual = mean(residual), .groups = "drop") %>%
  separate_rows(genres, sep = "\\|") %>%   # Split genres
  group_by(genres) %>%
  summarize(
    b_g = sum(residual) / (n() + lambda * 10), # genre effect
    n_g = n()
  )
# Compute average genre effect per movie (a movie may have more than one genre)
movie_genre_effects <- train_set %>%
  select(movieId, genres) %>%
  distinct() %>%
  separate_rows(genres, sep = "\\|") %>%
  left_join(genre_effects, by = "genres") %>%
  group_by(movieId) %>%
  summarize(b_g = mean(b_g, na.rm = TRUE))
# Predict using movie, user and genre effects
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_genre_effects, by='movieId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    b_g = ifelse(is.na(b_g), 0, b_g),           # Unseen genre => no effect
    pred = mu + b_i + b_u + b_g
  ) %>%
  pull(pred)
model_3_rmse <- RMSE(test_set$rating, predicted_ratings)
# Add model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + User + Genre Effects",
                                     RMSE = model_3_rmse))

# 10. Add Time (Movie Age, Rating Year) Effects
train_set <- train_set %>%
  mutate(
    date = as_datetime(timestamp),  # Convert UNIX timestamp to date
    rating_year = year(date),       # Extract the year of the rating
    release_year = as.numeric(substring(title, nchar(title) - 4, nchar(title) - 1)), # Extract release year from the last characters of the title
    movie_age = rating_year - release_year      # Compute how old the movie was when it was rated
  )
# Compute effect by movie age (bucketed/categorized into intervals)
time_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  filter(!is.na(movie_age)) %>%
  group_by(movie_age_cat = cut(movie_age, breaks = c(-Inf,0,5,10,20,30,Inf))) %>% # Age groups (e.g., <=0, 1–5, etc.)
  summarize(
    b_t = sum(residual) / (n() + 15),     # Regularized average effect for each age group
    n_t = n(),
    .groups = "drop"
  )
# Compute effect by year movie was rated
year_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  group_by(rating_year) %>%
  summarize(
    b_y = sum(residual) / (n() + 6),   # Regularized average for each rating year
    n_y = n(),
    .groups = "drop"
  )
# Apply these time-related transforms to test_set as well
test_set <- test_set %>%
  mutate(
    date = as_datetime(timestamp),
    rating_year = year(date),
    release_year = as.numeric(substring(title, nchar(title) - 4, nchar(title) - 1)),
    movie_age = rating_year - release_year,
    movie_age_cat = cut(movie_age, breaks = c(-Inf,0,5,10,20,30,Inf))
  )
# Predict using movie, user, movie-age effect and rating-year effect in addition to previous ones
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_effects, by='movie_age_cat') %>%
  left_join(year_effects, by='rating_year') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    b_t = ifelse(is.na(b_t), 0, b_t),   # Movie-age effect
    b_y = ifelse(is.na(b_y), 0, b_y),   # Rating-year effect
    pred = mu + b_i + b_u + b_t + b_y
  ) %>%
  pull(pred)
model_4_rmse <- RMSE(test_set$rating, predicted_ratings)
# Add model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + User + Time Effects",
                                     RMSE = model_4_rmse))

# 11. Final Ensemble: All Effects
# (Genre is averaged at the movie level for simplicity)
movie_genre_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  group_by(movieId) %>%
  summarize(
    b_g = mean(residual),   # Average residual per movie is used as genre effect
    .groups = "drop"
  )
# Predict using all available effects: movie, user, genre, movie-age, and rating-year
predicted_ratings_all <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_genre_effects, by='movieId') %>%
  left_join(time_effects, by='movie_age_cat') %>%
  left_join(year_effects, by='rating_year') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    b_g = ifelse(is.na(b_g), 0, b_g),
    b_t = ifelse(is.na(b_t), 0, b_t),
    b_y = ifelse(is.na(b_y), 0, b_y),
    pred = mu + b_i + b_u + b_g + b_t + b_y
  ) %>%
  pull(pred)
model_5_rmse <- RMSE(test_set$rating, predicted_ratings_all)
# Add the final ensemble model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="All Effects (Movie + User + Genre + Time)",
                                     RMSE = model_5_rmse))

# 12. Show Results Table
rmse_results %>% arrange(RMSE)  # Display all models and their RMSE, sorted from lowest RMSE (best) to highest

genre_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  group_by(movieId, genres) %>%
  summarize(residual = mean(residual), .groups = "drop") %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(
    b_g = sum(residual) / (n() + lambda * 10),
    n_g = n()
  )
# Compute average genre effect per movie (a movie may have more than one genre)
movie_genre_effects <- train_set %>%
  select(movieId, genres) %>%
  distinct() %>%
  separate_rows(genres, sep = "\\|") %>%
  left_join(genre_effects, by = "genres") %>%
  group_by(movieId) %>%
  summarize(b_g = mean(b_g, na.rm = TRUE))
# Predict using movie, user and genre effects
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_genre_effects, by='movieId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    b_g = ifelse(is.na(b_g), 0, b_g),           # Unseen genre => no effect
    pred = mu + b_i + b_u + b_g
  ) %>%
  pull(pred)
model_3_rmse <- RMSE(test_set$rating, predicted_ratings)
# Add model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + User + Genre Effects",
                                     RMSE = model_3_rmse))

# 10. Add Time (Movie Age, Rating Year) Effects
train_set <- train_set %>%
  mutate(
    date = as_datetime(timestamp),  # Convert UNIX timestamp to date
    rating_year = year(date),       # Extract the year of the rating
    release_year = as.numeric(substring(title, nchar(title) - 4, nchar(title) - 1)), # Extract release year from the last characters of the title
    movie_age = rating_year - release_year      # Compute how old the movie was when it was rated
  )
# Compute effect by movie age (bucketed/categorized into intervals)
time_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  filter(!is.na(movie_age)) %>%
  group_by(movie_age_cat = cut(movie_age, breaks = c(-Inf,0,5,10,20,30,Inf))) %>% # Age groups (e.g., <=0, 1–5, etc.)
  summarize(
    b_t = sum(residual) / (n() + 15),     # Regularized average effect for each age group
    n_t = n(),
    .groups = "drop"
  )
# Compute effect by year movie was rated
year_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  group_by(rating_year) %>%
  summarize(
    b_y = sum(residual) / (n() + 6),   # Regularized average for each rating year
    n_y = n(),
    .groups = "drop"
  )
# Apply these time-related transforms to test_set as well
test_set <- test_set %>%
  mutate(
    date = as_datetime(timestamp),
    rating_year = year(date),
    release_year = as.numeric(substring(title, nchar(title) - 4, nchar(title) - 1)),
    movie_age = rating_year - release_year,
    movie_age_cat = cut(movie_age, breaks = c(-Inf,0,5,10,20,30,Inf))
  )
# Predict using movie, user, movie-age effect and rating-year effect in addition to previous ones
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_effects, by='movie_age_cat') %>%
  left_join(year_effects, by='rating_year') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    b_t = ifelse(is.na(b_t), 0, b_t),   # Movie-age effect
    b_y = ifelse(is.na(b_y), 0, b_y),   # Rating-year effect
    pred = mu + b_i + b_u + b_t + b_y
  ) %>%
  pull(pred)
model_4_rmse <- RMSE(test_set$rating, predicted_ratings)
# Add model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + User + Time Effects",
                                     RMSE = model_4_rmse))

# 11. Final Ensemble: All Effects
# (Genre is averaged at the movie level for simplicity)
movie_genre_effects <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    residual = rating - mu - b_i - b_u
  ) %>%
  group_by(movieId) %>%
  summarize(
    b_g = mean(residual),   # Average residual per movie is used as genre effect
    .groups = "drop"
  )
# Predict using all available effects: movie, user, genre, movie-age, and rating-year
predicted_ratings_all <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_genre_effects, by='movieId') %>%
  left_join(time_effects, by='movie_age_cat') %>%
  left_join(year_effects, by='rating_year') %>%
  mutate(
    b_i = ifelse(is.na(b_i), 0, b_i),
    b_u = ifelse(is.na(b_u), 0, b_u),
    b_g = ifelse(is.na(b_g), 0, b_g),
    b_t = ifelse(is.na(b_t), 0, b_t),
    b_y = ifelse(is.na(b_y), 0, b_y),
    pred = mu + b_i + b_u + b_g + b_t + b_y
  ) %>%
  pull(pred)
model_5_rmse <- RMSE(test_set$rating, predicted_ratings_all)
# Add the final ensemble model's RMSE to the results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="All Effects (Movie + User + Genre + Time)",
                                     RMSE = model_5_rmse))

# 12. Show Results Table
rmse_results %>% arrange(RMSE)  # Display all models and their RMSE, sorted from lowest RMSE (best) to highest
