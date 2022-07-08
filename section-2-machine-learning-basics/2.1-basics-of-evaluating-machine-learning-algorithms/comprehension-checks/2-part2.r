# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@6ece4b7276ca4fbbb53cad7932f5a1d3
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
# Q7: how to build test_index: the following he best answer because the createDataPartition() function
# has a number of parameters that allow the user to specify a test/training partition by the percentage of data (p)
# that goes to training
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Q8: my answer (ok)
# plot cutoff by increment of .1 for sepal.length, width and petal.length, width
iris %>% summarise(
  minsl = min(Sepal.Length), maxsl = max(Sepal.Length),
  minsw = min(Sepal.Width), maxsw = max(Sepal.Width),
  minpl = min(Petal.Length), maxpl = max(Petal.Length),
  minpw = min(Petal.Width), maxpw = max(Petal.Width))

cutoffsl <- seq(4.9, 7.9, .1)
cutoffsw <- seq(2, 3.8, .1)
cutoffpl <- seq(3, 6.9, .1)
cutoffpw <- seq(1, 2.5, .1)

accuracysl <- map_dbl(cutoffsl, function(x){
  y_hat <- ifelse(train["Sepal.Length"] > x, "virginica", "versicolor")
  mean(y_hat == train["Species"])
})
max(accuracysl)

accuracysw <- map_dbl(cutoffsw, function(x){
  y_hat <- ifelse(train["Sepal.Width"] > x, "virginica", "versicolor")
  mean(y_hat == train["Species"])
})
max(accuracysw)

accuracypl <- map_dbl(cutoffpl, function(x){
  y_hat <- ifelse(train["Petal.Length"] > x, "virginica", "versicolor")
  mean(y_hat == train["Species"])
})
max(accuracypl) # => best accuracy is Petal Length (0.96)

accuracypw <- map_dbl(cutoffpw, function(x){
  y_hat <- ifelse(train["Petal.Width"] > x, "virginica", "versicolor")
  mean(y_hat == train["Species"])
})
max(accuracypw)
# Q8: official answer (much simpler)
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)


# Q9: my answer (ok)
accuracypl <- map_dbl(cutoffpl, function(x){
  y_hat <- ifelse(train["Petal.Length"] > x, "virginica", "versicolor")
  mean(y_hat == train["Species"])
})
best_cutoffpl <- cutoffpl[which.max(accuracypl)]
data.frame(cutoffpl, accuracypl) %>%
  ggplot(aes(cutoffpl, accuracypl)) +
  geom_point() +
  geom_line()
y_hat <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor")
mean(y_hat == test$Species) # Overall Accuracy
# Q9: official answer
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


# Q10: my answer (can reuse official answer from Q8 with test set) (same as official answer)
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)


# Q11: my answer (ok)
plot(iris,pch=21,bg=iris$Species)
best_cutoffpl <- cutoffpl[which.max(accuracypl)]
best_cutoffpw <- cutoffpw[which.max(accuracypw)]
y_hat <- ifelse("|"(test["Petal.Length"] > best_cutoffpl, test["Petal.Width"] > best_cutoffpw), "virginica", "versicolor")
mean(y_hat == test$Species) # Overall Accuracy
# Q11: official answer
petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)