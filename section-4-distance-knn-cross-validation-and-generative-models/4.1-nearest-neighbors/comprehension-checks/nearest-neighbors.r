# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2b3a4f4bd3324c45aa8395f5c3bbfd1d/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@0ee8a5ac159b459580bc5a13c8508d56
# Q1: my answer (ok)
library(caret)
library(dslabs)
data(heights)
set.seed(1, sample.kind = "Rounding")
y <- heights$sex
x <- heights$height
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k) {
  knn_fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat_knn, reference = test_set$sex)
})
max(F_1)
ks[which.max(F_1)]
# Q1: official answer
library(dslabs)
library(tidyverse)
library(caret)
data("heights")
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k) {
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>%
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

# Q2: my answer (ok)
library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")
y <- tissue_gene_expression$y
tge <- as.data.frame(tissue_gene_expression$x)
tge['y'] <- y
# Make Valid Column Names
colnames(tge) <- make.names(colnames(tge))
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- tge[test_index,]
train_set <- tge[-test_index,]
ks <- seq(1, 11, 2)
accuracies <- map_df(ks, function(k) {
  fit <- knn3(y ~ ., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
})
# Q2: official answer
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})