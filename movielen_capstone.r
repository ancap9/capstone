################################
# Create edx set, validation set
################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


options(digits = 4)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)



dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# see the data
movielens %>% as_tibble()


# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


################################
# Naive basic method
################################

mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse



################################
# Method with just movie effet
################################

	
mu <- mean(edx$rating)
movie_avgs <- edx %>%
group_by(movieId) %>%
summarize(b_i = mean(rating - mu))


movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))


predicted_ratings <- mu + validation %>%
left_join(movie_avgs, by='movieId') %>%
pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
model_1_rmse
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)


################################
# Method with just movie effet + user effetc
################################


user_avgs <- edx %>%
left_join(movie_avgs, by='movieId') %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- validation %>%
left_join(movie_avgs, by='movieId') %>%
left_join(user_avgs, by='userId') %>%
mutate(pred = mu + b_i + b_u) %>%
pull(pred)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse

rmse_results <- bind_rows(rmse_results,
tibble(method="Movie + User Effects Model",
RMSE = model_2_rmse))


################################
# Method with just movie effet + user effetc + regularization
################################

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
mu <- mean(edx$rating)
b_i <- edx %>%
group_by(movieId) %>%
summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- edx %>%
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu)/(n()+l))

predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)

return(RMSE(predicted_ratings, validation$rating))})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
tibble(method="Regularized Movie + User Effect Model",
RMSE = min(rmses)))


################################
# Final contront and improvment of all method
################################

options(digits = 4)
rmse_results %>% knitr::kable()


































