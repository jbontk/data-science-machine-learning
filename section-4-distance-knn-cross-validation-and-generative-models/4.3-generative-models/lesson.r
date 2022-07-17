#
# Generative Models
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@cf9e5492064f4e7e9787ed0d8c27d1e9/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@e3a0438535564d699253194824ecd768
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#generative-models
#

# Reusing example related to predicting sex from height:
library(tidyverse)
library(caret)

library(dslabs)
data("heights")

y <- heights$height
set.seed(1995)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#
# Naive Bayes
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@cf9e5492064f4e7e9787ed0d8c27d1e9/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@9f7296d2bd314a4fa92fa97d20042bc0
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#naive-bayes
#

# In this case, the Naive Bayes approach is particularly appropriate because we know that the normal distribution is a good approximation
# for the conditional distributions of height given sex for both classes Y = 1 (female) and Y = 0 (male).
# This implies that we can approximate the conditional distributions f(X / Y = 1) and f(X / Y = 0) by simply estimating averages and standard deviations from the data:
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params
# A tibble: 2 x 3
#   sex      avg    sd
#   <fct>  <dbl> <dbl>
# 1 Female  65.1  3.43
# 2 Male    69.2  3.55

# The prevalence, which we will denote with pi = Pr(Y = 1) can be estimated from the data with:
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi # 0.2137405

# Now we can use our estimates of average and standard deviation to get an actual rule:
x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

#
# Controlling Prevalence
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@cf9e5492064f4e7e9787ed0d8c27d1e9/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@128209f2ec7c45c3bae208efe9589c7f
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#controlling-prevalence
#

# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex)) # 0.3492063 => low sensitivity due to low prevalence of Females

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex)) #  0.945

# Changing the cutoff of the decision rule: forcing pi_hat to be 0.5 to balance specificity and sensitivity, instead of changing the cutoff in the decision rule:
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")

# Difference in sensitivity with a better balance:
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)


#
# Quadratic Discriminant Analysis (QDA) and Linear Discriminant Analysis (LDA)
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@cf9e5492064f4e7e9787ed0d8c27d1e9/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@f8791dc6c320400caf490f3e9b4b46e3
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#quadratic-discriminant-analysis
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#linear-discriminant-analysis
#

# Quadratic Discriminant Analysis (QDA) is a version of Naive Bayes in which we assume that the distributions
# p_x/y=1(x) and p_x/y=0(x) are mutlivariate normal. The previous heights example is actually QDA.
# Let's look at a more complicated case: the 2 or 7 example:
data("mnist_27")

# In this case, we have 2 predictors so we assume each one is bivariate normal. This implies that we need to estimate
# 2 averages, 2 standard deviations and a correlation for each case Y = 1 and Y = 0.
# Once we have these, we can approximate the distributions f_x1,x2/Y=1 and f_x1,x2/Y=0.
# Let's estimate parameters from the data:
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1= sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params

# Here we provide a visual way of showing the approach. We plot the data and use contour plots to give an idea
# of what the 2 estimated normal densities look like (we show the curve representing a region that includes 95% of the points):
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color=y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# We can use the train function from the caret package to fit the model and obtain predictors:
library(caret)
train_qda <- train(y ~ ., method = "qda", data = mnist_27$train)

# We see that we obtain relatively good accuracy:
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"] # 0.82

# But QDA does not work as well as the kernel methods because the assumption of normality does not quite hold.
# For 2s it seems to be reasonable, but it seems to be off for the 7s.
# Notice the slight curvature in the points for the 7s:
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color=y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

# QDA becomes harder to use as the number of predictors increases. Here we have 2 predictors and have to compute 4 means,
# 4 SDs and 2 correlations.
# Once the number of parameters approaches the size of our data, the method becomes impractical due to overfitting.

#
# Linear Discriminant Analysis
#

# A relatively simple solution to the problem of having too many parameters is to assume that the correlation structure
# is the same for all classes, which reduces the number of parameters we need to estimate.
# In this case, we would compute just one pair of standard deviations and one correlation.

# We can fit the LDA model using caret:
train_lda <- train(y ~ ., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"] # 0.75

# In the case of LDA, the lack of flexibility does not permit us to capture the non-linearity in the true conditional probability function.

#
# Case Study: More than Three Classes
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@cf9e5492064f4e7e9787ed0d8c27d1e9/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@0093815a2c8a40208206bac38db8ce06
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#case-study-more-than-three-classes
#

# 3 classes instead of 2: 1s, 2s and 7s
# We can generate the data set using the following code:
if(!exists("mnist")) mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
## get the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)
## binarize the values. Above 200 is ink, below is no ink
x <- x > 200
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x))
##save data
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1], x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1], x_2 = x[-index_train,2])

# Here's the training data:
train_set %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# We can use the caret package to train the QDA model:
train_qda <- train(y ~ ., method = "qda", data = train_set)

# Now we estimate the conditional probabilities:
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()

# The confusion matrix is a 3x3 table:
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"] # 0.7518

# The results for kNN are better:
train_knn <- train(y ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(15, 51, 2)))
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"] # 0.7569

# One of the limitations of generative models here is due to the lack of fit of the normal assumption, in particular for class 1:
train_set %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type = "norm")

# Generative models can be very powerful, but only when we are able to successfully approximate the joint distribution of predictors
# conditioned on each class.