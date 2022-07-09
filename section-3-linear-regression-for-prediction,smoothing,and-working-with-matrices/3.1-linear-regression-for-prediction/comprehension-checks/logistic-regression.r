# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@a9b4a817b52c450e880cf66ea9794393

# Q1
library(tidyverse)
library(caret)

# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5,
                      mu_0 = 0, mu_1 = 2,
                      sigma_0 = 1,  sigma_1 = 1){

  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)

  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()


# Q1: my answer: guessed ok by looking at overall accuracy for both extremes (mu_1 = 0, mu_1 = 3)
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
mu_1 <- seq(0, 3, len=25)
all_dat <- lapply(mu_1, make_data, n=1000, p = 0.5,
       mu_0 = 0,
       sigma_0 = 1,  sigma_1 = 1)

dat0 <- make_data(mu_1 = 0)
fit_glm0 <- glm(y ~ x, data=dat0$train, family = "binomial")
p_hat_glm0 <- predict(fit_glm0, dat0$test)
y_hat_glm0 <- factor(ifelse(p_hat_glm0 > 0.5, 1, 0))

dat3 <- make_data(mu_1 = 3)
fit_glm3 <- glm(y ~ x, data=dat3$train, family = "binomial")
p_hat_glm3 <- predict(fit_glm3, dat3$test)
y_hat_glm3 <- factor(ifelse(p_hat_glm3 > 0.5, 1, 0))

fit_glm <- lapply(seq(1,25), function(seq) {glm(y ~ x, data=all_dat[[seq]]$train, family = "binomial")})
p_hat_glm <- sapply(seq(1,25), function(seq) {predict(fit_glm[seq], all_dat[[seq]]$test)})
y_hat_glm <- lapply(seq(1,25), function (seq) { factor(ifelse(p_hat_glm[[seq]] > 0.5, 1, 0)) })

# Accuracy:
confusionMatrix(y_hat_glm3, dat3$test$y)$overall[["Accuracy"]]


# Q1: official answer
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)