# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2faa56e611464cf4bfd42b13ac7fffa5/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@8d4023066f3b4fafa07b11abbcd4afad
# Q1
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)
# Q1: my answer (ok)
length(indexes[[1]][indexes[[1]] == 3])
length(indexes[[1]][indexes[[1]] == 4])
length(indexes[[1]][indexes[[1]] == 7])
# Q1: official answer

# Q2: my answer (ok)
i3 <- lapply(indexes, function (x) { x == 3 })
sum(unlist(i3))
# Q2: official answer
x <- sapply(indexes, function(ind){sum(ind == 3)})
sum(x)


# Q3
y <- rnorm(100, 0, 1)
qplot(y)
# Estimating the 75th quantile (qnorm(0.75)) using the sample quantile:
qnorm(0.75)
quantile(y, 0.75)
# Q3: my answer (same as official answer)
set.seed(1, sample.kind = "Rounding")
B <- 10^4
M_star <- replicate(B, {
  y_start <- rnorm(100, 0, 1)
  quantile(y_start, 0.75)
})
mean(M_star) # .6737512
sd(M_star)



# Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
# Q4: my answer
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10)
bootstraps <- sapply(indexes, function(x) quantile(y[x], .75))
mean(bootstraps)
sd(bootstraps)
# Q4: official answer
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)



# Q5: my answer (same as official answer)
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10^4)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

