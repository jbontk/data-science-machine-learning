#
# Trees Motivation
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@78cfd6af400d452c9aa9f8070403e55c/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@dc919905045c41f99e01f245c1dd7376
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#the-curse-of-dimensionality
#

# Kernel methods such as kNN or local regression face a challenge when multiple predictors are used due to what is referred to as
# The Curse of Dimensionality.
# The Dimension here refers to the fact that when we have p predictors, the distance between two observations is computed in
# p-dimensional space.
# A useful way of understanding The Curse of Dimensionality is by considering how large we have to make a span/neighbor/window to
# include a given *percentage* of tha data.
# And remember that with larger neighborhoods, our methods lose flexibility.

# For example, suppose we have one continuous predictor with equally spaced points in the [0, 1] interval and we want to create
# windows that include 10% of data.
# Then our window have to be of size 0.1.
# Now, for two predictors, if we decide to keep the neighborhood just as small, 10% for each dimension, we include only one point.
# If we want to include 10% of the data, then we need to increase the size of each side of the square to sqrt(.10)

# Using the same logic, if we want to include 10% of the data in a three-dimensional space, then the side of each cube is
# nthroot(.10, 3).

# In genral, to include 10% of the data in p-dimensions, we need an interval with each side of sixe nthroot(.10, p) of the total.
# This proportion gets close to 1 quickly, and when it reaches 1 it means we include all the data and are no longer smoothing:
library(tidyverse)
p <- 1:100
qplot(p, .1^(1/p), ylim = c(0, 1))

# By the time we reach 100 predictors, the neighborhood is no longer very local, as each side covers almost the entire dataset.

# Here we look at a set of elegant and versatile methods that adapt to higher dimensions and also allow these regions to take
# more complex shapes while still producing models that are interpretable. These are very popular, well-known and studied methods.
# We will concentrate on regression and decision trees and their extension to random forests.


#
# Classification and Regression Trees (CART)
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@78cfd6af400d452c9aa9f8070403e55c/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@56c9ab44dbe84c2ea4a566a87a7f8ab3
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#cart-motivation
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#regression-trees
#

# Example for this lesson: dataset that includes the breakdown of the composition of olive into eight fatty acids.
data("olive")
head(olive)

# We'll try to predict the region using the fatty acid composition values as predictors.
table(olive$region)
# We remove the area region because we don't use it as predictor.
olive <- select(olive, -area)

# Using k-nearest neighbors, we can fit as follows:
library(caret)
fit <- train(region ~ ., method = "knn", tuneGrid = data.frame(k = seq(1, 15, 2)), data = olive)
ggplot(fit)
# Accuracy is pretty good
fit$results$Accuracy # 0.9735952

# However, a bit of data exploration shows that we should be able to do better.
# For example, if we look at the distribution of each predictor stratified by region, we see that one of the fatty acids is present
# only in Southern Italy, and then another one separates Northen Italy from Sardinia.
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>%
  ggplot(aes(eicosenoic, linoleic, color = region)) +
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) +
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# This implies that we should be able to build an algorithm that perfectly predicts.
# If the first predictor is larger than 0.065, B, predict Southern Italy.
# If not, then look at the second predictor.
# And if that's larger than 10.535, predict Sardinia, and Northern Italy otherwise.
# That is a decision tree.

# The genral idea is to define an algorithm that uses data to create trees such as the ones we've just showm.
# Regression and decision trees operate by predicting an outcome variable y by partitioning the predictor space.
# When the outcome is continous, we call these types of algorithms regression trees.

# Let's then use a continuous case, the 2008 poll data introduced earlier, to describe the basic idea of how we build these algorithms.
# We'll try to estimate the conditional expectation, f(x) = E(Y / X = x), with Y the poll margin and X the day.
# Here's a graph of the data:
data("polls_2008")
qplot(day, margin, data = polls_2008)
# The idea is to build a decision tree, and at the end of each node, we'll have a different prediction Y_hat.
# 1. partition the space into non-overlapping regions R1, R2, ..., Rj.
# 2. for every observation that that falls within region xi E Rj, predict Y_hat with the average of the training observations
# Yi in region xi E Rj.

# How to decide on the partitioning? With recursion.

# In R, we can use rpart:
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
# We can visually see where the splits were made as follows:
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
# We end up with eight partitions.
# The final estimate f_hat(x) looks like this:
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# Why the algorithm stopped at eight partitions?
# Every time we split and define two new partitions, our training set residual sum of squares decreases. This is because with more partitions,
# our model has more flexibility to adapt to the training data. In fact, if you split until every point is its own partition, then the
# residual sum of squares goes all the way down to zero since the average of one value is that same value.
# To avoid this overtraining, the algorithm sets a minimum for how much the residual sum of squares must improve for another partition to be added.
# This parameter is the Complexity Parameter = CP. I.e. the residual su, of squares must improve by a factor of CP for the new partition to be added.
# Another aspect of the algorithm is that it sets a minimum number of observations to be partitioned. In the rpart package, the rpart function
# has an argument called minsplit that lets you define this. The default is 20.
# The algorithm also sets a minimum on the number of observations in each partition. In the rpart function, this argument is called minbucket.
# So if the optimal split results in a partition with less observation than this minimum, it is not considered.
# The default minbucket is minsplit / 3 rounded to the closest integer.

# When we set CP to 0 and minsplit to 2, our prediction is as flexible as possible and our predictor is our original data,
# because we keep splitting and splitting until the RSS is minimized to zero:
fit <- rpart(margin ~ ., data = polls_2008,
             control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# Now, note that in this methodology, we can also prune trees by snipping off partitions that do not meet a CP criterion.
# So we can grow a tree very very big and then prune off branches to make a smaller tree:
pruned_fit <- prune(fit, cp = 0.01)
plot(pruned_fit)
# Here's the resulting estimate:
polls_2008 %>%
  mutate(y_hat = predict(pruned_fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# But now, is the default CP value the best one? How to pick CP?
# We can use cross-validation, just like with any other tuning parameter.
# We can use the train function in the caret package for example, to pick the best CP:
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
# To see the resulting tree that minimizes the Mean Squared Error, we can access it through the componoent finalmodel:
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
# And because we only have one predictor, we can actually plot f_hat(x):
polls_2008 %>%
  mutate(y_hat = predict(train_rpart)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# You can see that the fit looks reasonable.


#
# Classification (Decision) Trees
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@78cfd6af400d452c9aa9f8070403e55c/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@1e3c43896d7e434c89e6082d107a3278
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#classification-decision-trees
#

# When the outcome is categorical, we refer to these methods as classification trees or decision trees.
# We use the same partitioning principles, that we use for the continuous case, but with some slight differences to account for the fact
# that we are now working with categorical data.
# The first difference is that rather than taking the average at the end of each node, now in the partitions, we predict
# with the class that has the majority vote in each node. So the class that that appears the most in a node, that will be what we predict.
# The second difference is that we can no longer use residual sum of squares to decide on the partition because the outcomes are categorical.

# Well, we could use a naive approach, for example, four partitions that minimize training error.
# Better performing approaches use more sophisticated metrics.

# Two of the more popular ones are the Gini Index and Entropy:

# Gini Index = sum[p_hat_m,k x (1 - p_hat_m,k), k = 1, K]

# Entropy = sum[p_hat_m,k x log(p_hat_m,k), k = 1, K], with 0 x log(0) defined as 0

# Both of these metrics seek to partition observations into subsets that have the same class. They want what is called Purity.

# Let's see how Classification Trees perform on the 2s and 7s example we have examined in previous videos:
data(mnist_27)
train_rpart <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), data = mnist_27$train)
# We then look at Accuracy vs Complexity plot,
plot(train_rpart)
# and we can pick the best Complexity Parameter from this plot.
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"] # 0.805

# This is better than Logistic Regression but not as good as the kernel methods.
# If we plot the estimate of the conditional probability obtained with this tree, it shows is the limitations of
# Classification Trees.
# Note that with Decision Trees, the boundary can't be smoothed.

# Despite these limitations, Classification Trees have certain advantages that make them very useful.
# First, they're highly interoperable even more so than Linear Regression Models or Logistic Regression Models.
# Second, they're easy to visualize if they're small enough.
# Third, they sometimes model human decision processes.

# OTOH, the greedy approach via recursive partitioning is sometimes harder to train than, for example, Linear Regression or
# k-Nearest Neighbors.
# Also, it may not be the best performing method since it's not very flexible, and it's actually quite susceptible to changes
# in the training data.

# Random Forests, improve on several of these shortcomings.



#
# Random Forests
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@78cfd6af400d452c9aa9f8070403e55c/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@fe1e48451291419a9eb6fb2451e252a0
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#random-forests
#

# Random Forests are a very popular approach that address the shortcomings of Decision Trees using a clever idea.
# The goal is to improve prediction performance and reduce instability by averaging multiple Decision Trees,
# a forest of Trees constructed with Randomness.
# It has 2 features to accomplish this:
# ***1. Bootstrap Aggregation, or Bagging: build many Decision Trees, T1, ..., TB, using the training set.
# And For every observation j in the test set, we form a prediction y_hat j using tree Tj.

# Example for polls dataset:
library(randomForest)
fit <- randomForest(margin ~ ., data = polls_2008)
# Plotting Error vs Number of Trees => the algorithm improves as we add more trees:
plot(fit)
# By the time we get to 200 trees, the algorithm is not changing much.

# The resulting estimate for this random forest can be seen like this:
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col = "red")
# The Random Forest is much smoother than what we achieved with the Regression Tree in the previous section.
# This is possible because the average of many step functions can be smooth.


# Let's look at the example of 2s and 7s:
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"] # 0.8
# Notice that we have much more flexibility than just a single tree.
# This particular Random Forest is a little bit too wiggly. We want something smoother.
# Note that we do not have optimized the parameters in any way.
# So let's do that with the caret package, and a faster Random Forest Algorithm, Rborist:
fit <- train(y ~ ., method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = seq(3, 50)), data = mnist_27$train)

# The final result has much improved accuracy.

# So we can control smoothness of the Random Forest estimate in several ways. One is to limit the size of each node.
# We can require the number of points per node to be larger.

# Second Feature of Random Forest:
# ***2. Use a random selection of the features for the splits. This reduces correlation between trees in the forest,
# which in turn improves prediction accuracy. the argument for this tuning parameter in the randomforest function is mtry.

# A disadvantage of Random Forest is that we lose interpretability. However, there's a measure called Variable Importance that
# helps us interpret the results.