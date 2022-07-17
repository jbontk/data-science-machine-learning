#
# Caret Package
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@1ce4ff2f28024d5ca7682b23f6158940/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@8bdd439c5b9d482f94a860c4aec51139
# https://rafalab.github.io/dsbook/caret.html
#

# Many of the Machine Learning algorithms are implemented in R. However, they are distributed via different packages, developed
# by different authors, with different syntax.
# The caret package tries to consolidate these differences and provide consistency.
# It has more than 200 different models summarized in the following site:
# http://topepo.github.io/caret/available-models.html
# The required package for each method is included in this page:
# http://topepo.github.io/caret/train-models-by-tag.html

# The Caret Package also provides a function that does cross-validation for us.

# We'll use the 2s and 7s dataset as an example:
data(mnist_27)

# train function allows us to train different algorithms using similar syntax:
library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

# To predict, we can use the output of train without needing to look at the specifics of predict.glm:
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

# We can also quickly study the Confusion Matrix:
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"] # 0.75
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"] # 0.84


#
# Tuning Parameters with Caret
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@1ce4ff2f28024d5ca7682b23f6158940/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@d6f598f25c3d4fb9a2e48121ce27f922
# https://rafalab.github.io/dsbook/caret.html#caret-cv
#

# When an algorithm includes a tuning parameter, train automatically uses cross-validation to decide among a few default values.
# To find out what parameter(s) are optimized, you can refer to this page: https://topepo.github.io/caret/available-models.html
# Or study the output of the following code:
getModelInfo("knn")
# You can do a quick lookup using:
modelLookup("knn") # => output shows that the parameter that's optimized is k

# So if we run train with default values, you can quickly see the results of the cross-validation using ggplot function:
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE) # highlight the parameter that optimizes the algorithm

# By default, the cross-validation is performed by testing on 25 bootstrap samples comprised of 25% of the observations.
# Also, for the knn method, the default is to try out k=5, 7, and 9.
# 9 optimizes the algorithm in this case. But maybe there's an even better k. We can find it out using tunegrid parameter:
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train, tuneGrid = data.frame(k = seq(9, 71, 2))) # fitting 30 versions of knn to 25 bootstrap samples, so we're fitting 750 knn models.
ggplot(train_knn, highlight = TRUE) # highlight the k the maximizes accuracy
# best k can also be obtained as follows:
train_knn$bestTune # 43
# best performing model: that is what is used by the function predict to make predictions
train_knn$finalModel

# Note that the best model was obtained using the training set. The cross-validation as well.
# To see the accuracy we obrain on the test set, we can run:
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), mnist_27$test$y)$overall["Accuracy"] # 0.885

# Changing the way we perform cross-validation using trainControl: e.g. making previous example running faster by using 10 validation samples that
# use 10% of the observations each:
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", data = mnist_27$train, tuneGrid = data.frame(k = seq(9, 71, 2)), trControl = control)
# Accuracy estimates are more variables than in the previous example (expected since we changed the number of samples used to estimate accuracy):
ggplot(train_knn_cv, highlight = TRUE)

# Note that the train function provides standard deviation values for each parameter that was tested.
# So we can make a plot that shows the point estimates of the accuracy along with standard deviations.
train_knn$results %>%
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD))

# Finally, let's notice that the best-fitting knn model approximates the true condition of probability pretty well.
# Howver the boundary is somewhat wiggly. This is because knn, like the basic bin smoother, does not use a smooth kernel.
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks= 0.5, color="black")
}
plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])
# To improve this, we could try loess (Local Polynomial Regression Fitting).
# https://topepo.github.io/caret/available-models.html shows that we can use gamLoess model for this.

install.packages("gam") # https://topepo.github.io/caret/train-models-by-tag.html
modelLookup("gamLoess") # 2 parameters to optimize: span and degree

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1) # trying out different parameter values of span, but keeping a fixed degree parameter
# With default cross-validation parameters, we can train our model like this:
train_loess <- train(y ~ ., method = "gamLoess", tuneGrid = grid, data = mnist_27$train)
# It performs similarly to knn:
ggplot(train_loess, highlight = TRUE)
confusionMatrix(data = predict(train_loess, mnist_27$test), reference = mnist_27$test$y)$overall["Accuracy"] # 0.85

# But the conditional probability is smoother than what we get with knn:
p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

# Note that not all parameters in machine learning algorithms are tuned. We use the train() function to only optimize parameters that are tunable.
# So it won't be the case that, for example, in regression models, the caret package will optimize the regression coefficients that
# are estimated. Instead, it will just estimate using least squares. This is an important distinction to make when using the caret package,
# knowing which parameters are optimized, and which ones are not.