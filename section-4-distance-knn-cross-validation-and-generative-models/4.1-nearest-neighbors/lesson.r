#
# Distance
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2b3a4f4bd3324c45aa8395f5c3bbfd1d/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@6ae8fcaba1dc495dac458080e86aea53
# https://rafalab.github.io/dsbook/large-datasets.html#distance
#

library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()

# Random sample of 2s and 7s
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

# The labels for the first 3 observations are:
y[1:3]  #> [1] 2 7 2

# The vectors of predictors for each of these observations are:
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

# y1 and y3 are the same number (7).
# Therefore we expect the distance between x1 and x3 to be smaller than between other numbers:
sqrt(sum((x_1 - x_2)^2)) # 3035.514
sqrt(sum((x_1 - x_3)^2)) # 1576.052: smallest distance
sqrt(sum((x_2 - x_3)^2)) # 3043.008

# Faster way using matrix algebra:
sqrt(crossprod(x_1 - x_2)) # 3035.514
sqrt(crossprod(x_1 - x_3)) # 1576.052
sqrt(crossprod(x_2 - x_3)) # 3043.008

# We can also compute all the distances at once relatively quickly using the function dist, which computes the distance
# between each row and produces an object of class dist:
d <- dist(x)
class(d)

# We can see the computed distances as a matrix:
as.matrix(d)[1:3,1:3]

# And as an image:
image(as.matrix(d))

# If we order this distance by the labels, we can see that, in general,
# the twos are closer to each other and the sevens are closer to each other:
image(as.matrix(d)[order(y), order(y)])


# Compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

#
# K-Nearest Neighbors
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2b3a4f4bd3324c45aa8395f5c3bbfd1d/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@a370be6ba10a4584ba8a38a053983f73
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#k-nearest-neighbors
#

# We will learn here our first Machine Learning Algorithm.
# K-nearest neighbors is related to Smoothing.
# K-nearest neighbors (kNN) estimates the conditional probabilities in a similar way to bin smoothing.
# However, kNN is easier to adapt to multiple dimensions.

# To demonstrate it, we're going to use the digits data with two predictors.
# We are going to predict 7s (Y = 1).

# Comparing accuracy of previously studied Logistic Regression
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1] # Accuracy 0.76

# Let's compare this to knn.
# We will use the function knn3 which comes with the caret package.
# 2 ways of calling knn3:
# 1. the first argument is a formula (i.e. outcome ~ predictor1 + predictor2 + predictor3)
# the second argument is a data frame
# Formula shortcut if we use all the predictors: 'outcome "tilde" "dot"' i.e. 'outcome ~ .'
knn_fit <- knn3(y~., data = mnist_27$train)

# 2. first argument is the matrix predictors and the second argument is the vector of outcomes:
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

# The signature 1. is quicker and simpler, but for large datasets, signature 2. is preferred.

# We also need to pass the number of neighbors to include, we'll use the default = 5.
knn_fit <- knn3(y~., data = mnist_27$train, k=5)
# The dataset is balanced (there are as many 2s as 7s), and we care as much about specificity as we do
# about sensitivity (both mistakes are equally bad).
# Therefore we will use accuracy to quantify performance.
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall[1] # Accuracy 0.815

# We can see that we already have an improvement over the logistic regression (0.815 > 0.76).



#
# Overtraining and Oversmoothing
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2b3a4f4bd3324c45aa8395f5c3bbfd1d/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@013de4e53dcb4e4fbee67e772103ea21
# https://rafalab.github.io/dsbook/cross-validation.html#over-training
# https://rafalab.github.io/dsbook/cross-validation.html#over-smoothing
#

# The understand overtraining, notice we have higher accuracy when we predict on a training set than on a test set:
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"] # Accuracy 0.8825

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"] # Accuracy 0.815

# We can see that the accuracy computed on the training side is higher. This is because we overtrained.
# Overtraining is at its worst when we set k = 1: in that case, the estimate for each point in the training set
# is obtained with just the y corresponding to that point because you are your closest neighbor.
# So in this case, we obtain practically perfect accuracy in the training set because each point is used to predict itself:
knn_fit_1 <- knn3(y~., mnist_27$train, k=1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn_1, reference = mnist_27$train$y)$overall["Accuracy"] # Accuracy 0.99625

# Accuracy is almost perfect (0.99625), however, whwn we check on the actual test set, the accuracy is worse
# than with logistic regression (0.735):
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn_1, reference = mnist_27$test$y)$overall["Accuracy"] # Accuracy 0.735

# Let's try a much larger k = 401:
y_hat_knn_401 <- predict(knn3(y~., mnist_27$train, k=401), mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn_401, reference = mnist_27$test$y)$overall["Accuracy"] # Accuracy 0.79

# The size of k is so large that it does not permit enough flexibility.
# We're almost including half of the data to compute each single estimated conditional probability.
# That is Oversmoothing.

# So how do we pick k? 5 seems too small, 401 seems too big. Something in the middle might be better.
# So we can repeat what we just did for different values of k, i.e. we can try all the odd numbers between 3 and 251.
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k) {
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)

  y_hat <- predict(fit, mnist_27$train, type = "class")
  train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall["Accuracy"] # not correct way using train set, but just doing it for comparison purposes

  y_hat <- predict(fit, mnist_27$test, type = "class")
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"] # correct way using test set

  tibble(train = train_error, test = test_error)
})

# ggplot cheatsheet: https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf
library(dplyr)
library(ggplot2)
df <- data.frame(ks, accuracy)
# Transform data using melt from reshape2 package
library(reshape2)
df.melt <- melt(df,  id.vars = 'ks', variable.name = 'Type.of.Set', value.name = 'Accuracy')
ggplot(data = df.melt) %>%
  +geom_line(mapping = aes(x = ks, y = Accuracy, col = Type.of.Set)) %>%
  +ylab("Accuracy") %>%
  +xlab("k")

# We see a general pattern: Low values of k give low test set accuracy but high train set accuracy,
# which is evidence of overtraining.
# Large values of k result in low accuracy, which is evidence of oversmoothing.
# The maximum is achieved somewhere between 25 and 41.
# In fact, the resulting estimate with k = 41 looks quite similar to the true conditional probability.
#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)

# But we actually broke  a golden rule of machine learning. We selected the k using the test set.
# So how do we select the k? In the next lessons, we introduce the important concept of cross-validation,
# which provides a way to estimate the expected loss for a given method using only the training set.