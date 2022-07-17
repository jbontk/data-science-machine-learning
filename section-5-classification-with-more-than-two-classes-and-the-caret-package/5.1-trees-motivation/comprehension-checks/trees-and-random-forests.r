# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@78cfd6af400d452c9aa9f8070403e55c/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@dd792a69297c4a1fad5975a2ce5d8409
# Q1
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)

# Q2
plot(fit)
text(fit)

# Q3
dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

# Q4
library(randomForest)
fit <-  randomForest(y ~ x, data = dat)
dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")

# Q5
plot(fit)

# Q6
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>%
    mutate(y_hat = predict(fit)) %>%
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")