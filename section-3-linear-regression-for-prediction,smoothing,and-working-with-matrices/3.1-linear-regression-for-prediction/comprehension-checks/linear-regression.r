# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@60c4e20003ed4cf8b2edb0d62f678996
# Q1
library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>%
  setNames(c("x", "y"))
# Q1: my answer (same as official answer)
y <- dat$y
rmse <- replicate(n, {
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse) # 2.4895
sd(rmse) # 0.125

# Q2: my answer
#
# create function
#
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
fun <- function(n) {
  #
  # build dataset of n observations
  #
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>%
    setNames(c("x", "y"))
  #
  # calculate RMSEs
  #
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  #
  # return mean and sd of RMSEs
  #
  c(n, mean(rmse), sd(rmse))
}

#
# apply created function
#
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind = "Rounding")
sapply(n, fun)
# Q2: official answer
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res


# Q4: my answer (same as official answer) (note: seed twice for reproducible results)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
sd(rmse)


# Q6
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# Q6: my answer (same as official answer)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
#
# partition into test and training sets
#
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
#
# predict y using x_1
#
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))
#
# predict y using x_2
#
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))
#
# predict y using x_1 and x_2
#
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))


# Q8
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# Q8: my answer
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
#
# partition into test and training sets
#
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
#
# predict y using x_1
#
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))
#
# predict y using x_2
#
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))
#
# predict y using x_1 and x_2
#
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))