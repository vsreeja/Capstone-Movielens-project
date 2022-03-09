# Install the packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl,"ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),title = as.character(title),genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation data are also in edx data
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation data back into edx data
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Edit the edx and validation data for analysis
# split title to title and release year 
edx <- edx %>% mutate(title = str_trim(title)) %>%  
  extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
  select(-title_temp)

validation <- validation %>% mutate(title = str_trim(title)) %>% extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>% mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>% mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
  select(-title_temp)

# Convert timestamp to date-year-time format
edx <- edx %>% 
  mutate(review_date = round_date(as_datetime(timestamp), unit = "week")) %>% 
  mutate(time_rated = hour(as_datetime(timestamp)))

validation <- validation %>% 
  mutate(review_date = round_date(as_datetime(timestamp), unit = "week")) %>% 
  mutate(time_rated = hour(as_datetime(timestamp)))

#Preliminary Data Analysis

# Distribution of ratings
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  theme_light()+ 
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0,3000000,500000))) + 
  labs(x = "Ratings", y = "Count")

# Ratings by movie
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins=50,color = I("black")) + 
  theme_light()+
  scale_x_log10() + 
  scale_y_continuous(breaks = c(seq(0,500,100))) + 
  labs(x = "Number of ratings", y = "Count") 

# Average ratings by movie
edx %>% group_by(movieId) %>%
  summarise(ave_rating = sum(rating)/n()) %>%
  ggplot(aes(ave_rating)) +
  geom_histogram(bins=50, color = I("black")) +
  theme_light()+
  scale_y_continuous(breaks = c(seq(0,800,200))) +
  labs(x = "Average ratings", y = "Count")

# Ratings by user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins=30, color = I("black")) +
  theme_light()+
  scale_x_log10() +
  scale_y_continuous(breaks = c(seq(0,8000,2000)))+
  labs(x = "Number of ratings", y = "Count")

# Average ratings by user
edx %>% group_by(userId) %>%
  summarise(ave_rating = sum(rating)/n()) %>%
  ggplot(aes(ave_rating)) +
  geom_histogram(bins=30, color = I("black")) +
  theme_light()+
  scale_y_continuous(breaks = c(seq(0,12000,3000)))+
  labs(x = "Average ratings", y = "Number of users") 

# Ratings by genre
edx %>% 
  count(genres) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins=30, color = I("black")) +
  theme_light()+
  scale_x_log10() +
  scale_y_continuous(breaks = c(seq(0,80,20)))+
  labs(x = "Number of ratings", y = "Count")

# Average ratings by genre for genre combinations with at least 50,000 ratings
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 50000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Genre combinations", y = "Average Ratings") 

# Separate individual genres and rank by the total number of ratings in the edx data set
genre <- edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>% 
  summarize(n = n(), count = n_distinct(movieId), avg = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  arrange(desc(n)) 

# List individual genres by number of ratings and number of movies
Table <- genre %>% select(genres,n,count) 

# Average ratings by individual genre with ratings greater than 50000
genre %>% filter(n >= 50000) %>%
  mutate(genres = reorder(genres,avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar()+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1)) +
  scale_y_continuous(limit = c(3.25,4.25)) +
  labs(x = "Individual genre", y = "Average Rating")
rm(genre)

# Ratings by movie release year
edx %>% group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(aes(year, count)) +
  geom_line() +
  theme_light()+
  scale_y_continuous(limit = c(0,800000)) + 
  labs(x = "Release year", y = "Number of ratings")

# Average ratings by release year
edx %>% group_by(year) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(year,rating)) +
  geom_point() +
  geom_smooth()+
  theme_light()+
  scale_y_continuous(limit = c(3.2,4.2)) +
  labs(x = "Release Year", y = "Average ratings")

# Average ratings by review date
edx %>% group_by(review_date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(review_date, rating)) +
  geom_point() +
  geom_smooth() +
  theme_light()+
  scale_y_continuous(limit = c(3,4.5)) +
  labs(x = "Date of review", y = "Average ratings")

# Average ratings by review time
edx %>% group_by(time_rated) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(time_rated, rating)) +
  geom_point() +
  geom_smooth() +
  theme_light()+
  scale_y_continuous(limit = c(3.45,3.55)) +
  labs(x = "Time of review", y = "Average ratings")

#Algorithm development using edx data

# Create train and test sets from edx
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

#Make sure userId and movieId in test data are also in train data
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test data back into train data
removed <- anti_join(temp, test_set) 
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed) 

#Calculation of Root Mean Squared Error 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}

# Mean algorithm
Mu <- mean(train_set$rating)
rmse <- RMSE(Mu,test_set$rating)

rmse_results <- data.frame(Method = c("Mean Algorithm"),
                           RMSE = round(rmse,5))

# Incorporate movie effects
movie_avg <- train_set %>%
  group_by(movieId) %>%
  summarise(movie_m = mean(rating - Mu))
# Predict ratings adjusting for movie effects
predicted_movie <- Mu + test_set %>%
  left_join(movie_avg, by = "movieId") %>%
  pull(movie_m)
# Calculate RMSE based on movie effects model
movie_rmse <- RMSE(predicted_movie, test_set$rating)

rmse_results <- rmse_results %>% 
  rbind(c("Movie Effects", round(movie_rmse,5)))

rm(predicted_movie)
# Plot movie effects distribution
movie_avg %>%
  ggplot(aes(movie_m)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "Movie effect (b_i)", y = "Count")

# Incorporate user effects
user_avg <- train_set %>%
  left_join(movie_avg, by = "movieId") %>%
  group_by(userId) %>%
  summarise(user_u = mean(rating - Mu - movie_m))
# Predict ratings adjusting for movie and user effects
predicted_user <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  mutate(predict = Mu + movie_m + user_u) %>%
  pull(predict)
# Calculate RMSE based on user effects model
user_rmse <- RMSE(predicted_user, test_set$rating)

rmse_results <- rmse_results %>% rbind(c("Movie+User Effects", round(user_rmse,5)))
rm(predicted_user)
# Plot user effects distribution
user_avg %>%
  ggplot(aes(user_u)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "User effects (b_u)", y = "Count")

# Incorporate combined genre effects
genre_avg <- train_set %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  group_by(genres) %>%
  summarise(genre_g = mean(rating - Mu - movie_m - user_u))
# Predict ratings adjusting for movie, user and combined genre effects
predicted_genre <- test_set %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg, by = "genres") %>%
  mutate(predict = Mu + movie_m + user_u + genre_g) %>%
  pull(predict)
# Calculate RMSE based on genre effects model
genre_rmse_c <- RMSE(predicted_genre, test_set$rating)

rmse_results <- rmse_results %>% 
  rbind(c("Movie+User+Combined Genre Effects", round(genre_rmse_c,5))) 
rm(predicted_genre)

# Plot combined genre effects distribution
genre_avg %>%
  ggplot(aes(genre_g)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "Combined Genre effect (b_gc)", y = "Count")

#split combination genres to individual genre
test_set_i <- test_set %>%
  separate_rows(genres, sep = "\\|")
train_set_i <- train_set %>%
  separate_rows(genres, sep = "\\|")

# Incorporate individual genre effects
genre_avg_i <- train_set_i %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  group_by(genres) %>%
  summarise(genre_g = mean(rating - Mu - movie_m - user_u))
# Predict ratings adjusting for movie, user and individual genre effects
predicted_genre_i <- test_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  mutate(predict = Mu + movie_m + user_u + genre_g) %>%
  pull(predict)
# Calculate RMSE based on genre effects model
genre_rmse_i <- RMSE(predicted_genre_i, test_set_i$rating)

rmse_results <- rmse_results %>% 
  rbind(c("Movie+User+Individual Genre Effects", round(genre_rmse_i,5))) 

# Plot individual genre effects distribution
genre_avg_i %>%
  ggplot(aes(genre_g)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "Individual Genre effect (b_g)", y = "Count")

# Incorporate release year effects
year_avg <- train_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  group_by(year) %>%
  summarise(year_y = mean(rating - Mu - movie_m - user_u - genre_g))
# Predict ratings adjusting for movie, user, genre and year effects
predicted_year <- test_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  left_join(year_avg, by = "year") %>%
  mutate(predict = Mu + movie_m + user_u + genre_g + year_y) %>%
  pull(predict)
# Calculate RMSE based on year effects model
year_rmse <- RMSE(predicted_year, test_set_i$rating)

rmse_results <- rmse_results %>% 
  rbind(c("Movie+User+Individual Genre+Release year Effects", round(year_rmse,5))) 

# Plot release year effects distribution
year_avg %>%
  ggplot(aes(year_y)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "Release year effect (b_y)", y = "Count")

# Incorporate review date effects
date_avg <- train_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  left_join(year_avg, by = "year") %>%
  group_by(review_date) %>%
  summarise(date_r = mean(rating - Mu - movie_m - user_u - genre_g - year_y))
# Predict ratings adjusting for movie, user, genre, year and review date effects
predicted_review <- test_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  left_join(year_avg, by = "year") %>%
  left_join(date_avg, by = "review_date") %>%
  mutate(predict = Mu + movie_m + user_u + genre_g + year_y + + date_r) %>%
  pull(predict)
# Calculate RMSE based on review date effects model
date_rmse <- RMSE(predicted_review, test_set_i$rating)

rmse_results <- rmse_results %>% 
  rbind(c("Movie+User+Individual Genre+Release year+Review date Effects", round(date_rmse,5))) 

# Plot review date effects distribution
date_avg %>%
  ggplot(aes(date_r)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "Review date effect (b_r)", y = "Count")

# Incorporate review time effects
time_avg <- train_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  left_join(year_avg, by = "year") %>%
  left_join(date_avg, by = "review_date") %>%
  group_by(time_rated) %>%
  summarise(review_t = mean(rating - Mu - movie_m - user_u - genre_g - year_y - date_r))
# Predict ratings adjusting for movie, user, genre, year, review date and time effects
predicted_time <- test_set_i %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg_i, by = "genres") %>%
  left_join(year_avg, by = "year") %>%
  left_join(date_avg, by = "review_date") %>%
  left_join(time_avg, by = "time_rated") %>%
  mutate(predict = Mu + movie_m + user_u + genre_g + year_y + date_r + review_t) %>%
  pull(predict)
# Calculate RMSE based on review time effects model
time_rmse <- RMSE(predicted_time, test_set_i$rating)

rmse_results <- rmse_results %>% 
  rbind(c("Movie+User+Individual Genre+Release year+Review date+Review time Effects", round(time_rmse,5))) 

# Plot review time effects distribution
time_avg %>%
  ggplot(aes(review_t)) +
  geom_histogram(bins = 10, color = I("black")) +
  theme_light()+
  labs(x = "Review time effect (b_t)", y = "Count")

rm(time_avg,user_avg,year_avg,movie_avg,date_avg,genre_avg,genre_avg_i)
rm(predicted_genre_i,predicted_review,predicted_time,predicted_year)

# Regularization of user + movie effects (combined genres)
lambdas_mu <- seq(4, 6, 0.2)
rmses_mu <- sapply(lambdas_mu, function(l){
  movie_avg <- train_set %>%
    group_by(movieId) %>%
    summarise(movie_m = sum(rating - Mu)/(n()+l))
  user_avg <- train_set %>%
    left_join(movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_u = sum(rating - movie_m - Mu)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    mutate(predict = Mu + movie_m + user_u) %>%
    pull(predict)
  return(RMSE(predicted_ratings, test_set$rating))
})
# Find optimal lambda
lambda_mu <- lambdas_mu[which.min(rmses_mu)]
# Minimum achieved RMSE
regularised_rmse_mu <- min(rmses_mu) 
# Plot RMSE against each lambda
data.frame(lambdas_mu, rmses_mu) %>%
  ggplot(aes(lambdas_mu, rmses_mu)) +
  geom_point() +
  geom_hline(yintercept=min(rmses_mu), linetype='dotted', col = "red") +
  theme_light()+
  annotate("text", x = lambda_mu, y = min(rmses_mu), label = lambda_mu, vjust = -1, color = "red") +
  labs(x = "Lambda", y = "RMSE")

rmse_results <- rmse_results %>% 
  rbind(c("Regularized Movie+User Effects", round(regularised_rmse_mu,5))) 

# Regularization of all effects with combined genres
lambdas_c <- seq(4, 6, 0.2)
rmses_c <- sapply(lambdas_c, function(l){
  movie_avg <- train_set %>%
    group_by(movieId) %>%
    summarise(movie_m = sum(rating - Mu)/(n()+l))
  user_avg <- train_set %>%
    left_join(movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_u = sum(rating - movie_m - Mu)/(n()+l))
  genre_avg <- train_set %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    group_by(genres) %>%
    summarise(genre_g = sum(rating - movie_m - user_u - Mu)/(n()+l))
  year_avg <- train_set %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(genre_avg, by="genres") %>%
    group_by(year) %>%
    summarise(year_y = sum(rating - movie_m - user_u - genre_g - Mu)/(n()+l))
  review_avg <- train_set %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(genre_avg, by="genres") %>%
    left_join(year_avg, by="year") %>%
    group_by(review_date) %>%
    summarise(review_d = sum(rating - movie_m - user_u - genre_g - year_y - Mu)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(genre_avg, by="genres") %>%
    left_join(year_avg, by="year") %>%
    left_join(review_avg, by="review_date") %>%
    mutate(predict = Mu + movie_m + user_u + genre_g + year_y + review_d) %>%
    pull(predict)
  return(RMSE(predicted_ratings, test_set$rating))
})
# Find optimal lambda
lambda_c <- lambdas_c[which.min(rmses_c)]
# Minimum achieved RMSE
regularised_rmse_c <- min(rmses_c) 

rmse_results <- rmse_results %>% 
  rbind(c("Regularized Movie+User+Combined Genres+Release year+Review date Effects", round(regularised_rmse_c,5))) 

# Regularization of all effects with individual genres
lambdas_i <- seq(13, 15, 0.2)
rmses_i <- sapply(lambdas_i, function(l){
  movie_avg <- train_set_i %>%
    group_by(movieId) %>%
    summarise(movie_m = sum(rating - Mu)/(n()+l))
  user_avg <- train_set_i %>%
    left_join(movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_u = sum(rating - movie_m - Mu)/(n()+l))
  genre_avg <- train_set_i %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    group_by(genres) %>%
    summarise(genre_g = sum(rating - movie_m - user_u - Mu)/(n()+l))
  year_avg <- train_set_i %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(genre_avg, by="genres") %>%
    group_by(year) %>%
    summarise(year_y = sum(rating - movie_m - user_u - genre_g - Mu)/(n()+l))
  review_avg <- train_set_i %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(genre_avg, by="genres") %>%
    left_join(year_avg, by="year") %>%
    group_by(review_date) %>%
    summarise(review_d = sum(rating - movie_m - user_u - genre_g - year_y - Mu)/(n()+l))
  predicted_ratings <- test_set_i %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(genre_avg, by="genres") %>%
    left_join(year_avg, by="year") %>%
    left_join(review_avg, by="review_date") %>%
    mutate(predict = Mu + movie_m + user_u + genre_g + year_y + review_d) %>%
    pull(predict)
  return(RMSE(predicted_ratings, test_set_i$rating))
})

# Find optimal lambda
lambda_i <- lambdas_i[which.min(rmses_i)]
# Minimum achieved RMSE
regularised_rmse_i <- min(rmses_i) 

rmse_results <- rmse_results %>% 
  rbind(c("Regularized Movie+User+Individual Genre+Release year+Review date Effects", round(regularised_rmse_i,5))) 

#Matrix factorization for regularized movie+user algorithm
lambda_mu <- 4.8
Mu <- mean(train_set$rating)
movie_avg <- train_set %>%
  group_by(movieId) %>%
  summarise(movie_m = sum(rating - Mu) / (n()+lambda_mu))
user_avg <- train_set %>%
  left_join(movie_avg, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_u = sum(rating - movie_m - Mu) / (n()+lambda_mu))
predicted_user <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  mutate(predict = Mu + movie_m + user_u) %>%
  pull(predict)

# Residuals of the prediction
train_residual <- train_set %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(residual = rating - Mu - movie_m - user_u) %>% select(movieId, userId, residual)

# Transform to matrix format
train_MF <- as.matrix(train_residual)
test_MF <- test_set %>% 
  select(movieId, userId, rating)
test_MF <- as.matrix(test_MF)

# Write table
write.table(train_MF , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(test_MF, file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# Specify a data set
set.seed(1, sample.kind="Rounding") 
trainset <- data_file("trainset.txt")
testset <- data_file("testset.txt")

# Build a recommender object
r <-Reco()

# Tune the train set
opts <- r$tune(trainset, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2), costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))

# Train the recommender model
r$train(trainset, opts = c(opts$min, nthread = 1, niter = 20))

# Make prediction on test set
pred_file <- tempfile()
r$predict(testset, out_file(pred_file))  
predicted_residuals_MF <- scan(pred_file)
predicted_ratings_MF <- predicted_user + predicted_residuals_MF

# Calculate RMSE
MF_rmse_c <- RMSE(predicted_ratings_MF,test_set$rating)
rmse_results <- rmse_results %>% 
  rbind(c("Matrix Factorization", round(MF_rmse_c,5))) 

#Final validation
# Mean algorithm
Mu_edx <- mean(edx$rating)
rmse_edx <- RMSE(Mu_edx, validation$rating)

final_rmse <- data.frame(Method = c("Mean algorithm"),
                         RMSE = round(rmse_edx,5))

# Matrix factorization for validation data 
lambda_mu <- 4.8
Mu_edx <- mean(edx$rating)
movie_avg <- edx %>%
  group_by(movieId) %>%
  summarise(movie_m = sum(rating - Mu_edx) / (n()+lambda_mu))
user_avg <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_u = sum(rating - movie_m - Mu_edx) / (n()+lambda_mu))
predicted_user <- validation %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  mutate(predict = Mu_edx + movie_m + user_u) %>%
  pull(predict)

# Residuals of the prediction
edx_residual <- edx %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(residual = rating - Mu_edx - movie_m - user_u) %>% select(movieId, userId, residual)

# Transform to matrix format
edx_MF <- as.matrix(edx_residual)
validation_MF <- validation %>% 
  select(movieId, userId, rating)
validation_MF <- as.matrix(validation_MF)

# Write table
write.table(edx_MF , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_MF, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# Specify data set
set.seed(1, sample.kind="Rounding") 
edx_set <- data_file("trainset.txt")
valid_set <- data_file("validset.txt")

# Build a recommender object
r <-Reco()

# Tune the train set
opts <- r$tune(edx_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2), costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))

# Train the recommender model
r$train(edx_set, opts = c(opts$min, nthread = 1, niter = 20))

# Make prediction on test set
pred_file <- tempfile()
r$predict(valid_set, out_file(pred_file))  
predicted_residuals_MF <- scan(pred_file)
predicted_ratings_MF <- predicted_user + predicted_residuals_MF

# Calculate RMSE
validation_MF <- RMSE(predicted_ratings_MF, validation$rating)

final_rmse <- final_rmse %>% 
  rbind(c("Matrix Factorization", round(validation_MF,5))) 

validation_MF