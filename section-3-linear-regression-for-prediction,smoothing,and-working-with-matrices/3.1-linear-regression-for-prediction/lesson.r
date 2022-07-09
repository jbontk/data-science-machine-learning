#
# Linear Regression for Prediction
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@b8591cd84e0a4b7c97e63887f714cfbe
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#linear-regression
#

# Linear Regression: can be considered a machine learning algorithm

library(HistData)

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Guessing the son's height
m <- mean(train_set$son)
m
# Root mean squared error:
sqrt(mean((m-test_set$son)^2))

# Predicting the son's height using the father's height
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# Since the pair (Y, X) = (height_son, height_father) follows a bivariate normal distribution
# the conditional expectation (what we want to estimate) is equivalent to the following
# regression line:
# f(x) = E(Y / X = x) = beta_0 + beta_1x
# We can use least sqaures method for estimating the slope beta_1 and intercept beta_0:
fit <- lm(son ~ father, data = train_set)
# This gives us an estinate of the conditional expectation:
# f_hat(x) = 41.79 + 0.4126x

# This provides an improvement over guessing:
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
sqrt(mean((y_hat - test_set$son)^2)) # 2.54 < 2.80


#
# Predict function
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@4105d703225f4a19bf9a04be01245207
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#the-predict-function
#

# predict takes a fitted object from functions such as lm or glm and a data frame with the new
# predictors for which to predict:
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$son)^2)) # 2.54