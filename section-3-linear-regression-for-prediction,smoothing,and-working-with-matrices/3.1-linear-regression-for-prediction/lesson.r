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
sqrt(mean((y_hat - test_set$son)^2))


#
# Regression for a Categorical Outcome
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@fac1c6be48424c2cafcd47f4a45322c3
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#logistic-regression
#

# The regression approach can be extended to categorical data. In this section we first illustrate how,
# for binary data, one can simply assign numeric values of 0 and 1 to the outcomes y,
# and apply regression as if the data were continuous.
# This approach has a a limitation which can be solved using logistic regression.
library(dslabs)
data(heights)
y <- heights$sex
x <- heights$height
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
train_set %>%
  filter(round(height) == 66) %>%
  summarize(y_hat = mean(sex=="Female"))
#       y_hat
# 1 0.2727273

# Estimating the proportion of the population that is female for any given height X = x, i.e. Pr(Y = 1 / X = x)
heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

# Plot results (female proba vs height) look linear => trying regression
# p(x) = Pr(Y = 1 / X = x) = beta_0 + beta_1x
# If we convert the factors to 0s and 1s, we can estimate beta_0 and beta_1 with least squares
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)
# prediction = p_hat(x) = beta_0_hat + beta_1_hatx
# = intercept + slope * x
# = intercept + height * x

# Then, formulate the prediction alogorithm: it is a decision rule as follows:
# p_beta(x) > 0.5 => sex = female
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall[["Accuracy"]]


#
# Logistic Regression
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@296bd08c1a5041d6804903839f749d5e
# https://rafalab.github.io/dsbook/examples-of-algorithms.html#logistic-regression
#

# Plotting linear regression results along with female proba vs height
heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat) # [1] -0.3412072  1.0535595
# i.e. it can take values < 0 and > 1, even though it is a probability which should therefore be between 0 and 1

# Generalized Linear Models:
# 1) define a prediction consistent with its possible outcomes
# 2) find a function g so that g(Pr(Y = 1 / X = x) can be modeled as a linear combination of predictors
# Logistic Regression is the most commonly used GLM: it is an extension of linear regression
# that assures that the estimate of Pr(Y = 1 / X = x) is between 0 and 1.
# It uses the following Logistic Transformation:
#              p
# g(p) = log -----
#            1 - p
# That Transformation converts probabilities to log odds.
plot(function(p) { log(p / (1 - p)) }, 0, 1, ylab = "log(x / (1-x))")

# With logistic regression, we model the conditional probability directly with:
# g{Pr(Y = 1 / X = x)} = beta_0 + beta_1x

# With this model, we can no longer use least squares.
# Instead, we compute the Maximum Likelihood Estimate: MLE

# In R, we can fit the logistic regression model with the function glm:
# Generalized Linear Models
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
summary(glm_fit)

# We can obtain prediction using the predict function
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

# When using predict with a glm object, we have to specify that we want type="response" if we want the conditional probabilities,
# since the default is to return the logistic transformed values.
# This model fits the data slightly better than the line:
tmp <- heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female"))
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

# Because we have an estimate p_hat(x), we can obtain predictions:
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]] # [1] 0.8057143

#
# Case Study: 2 or 7
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@9068ddd62fb44046a713979687179718/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@80c5e0e79d844e0298b63fc82dbb294c
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#two-or-seven
#

library(tidyverse)
library(dslabs)
data("mnist_27")

# Plotting the two predictors and using colors to denote the labels:
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# We observe that when x_1 is very large, the digit is probably a 7
# And that for smaller values of x_1, the 2 appears to be in mid range values of x_2

# Let's try building an algorithm using regression, using these two predictors.
# Here's the model:
# p(x1, x2) = Pr(Y = 1 / X1 = x1, X2 = x2) = beta_0 + beta_1x1 + beta_2x2

# Fitting this model:
fit <- mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

# We can now build a decision rule based in the estiamte of p_hat(x1, x2):
library(caret)
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]] # [1] 0.75

# Can we do better than 75%?


mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()
# Drawing a curve that separates pairs (x1, x2) for which p(x1, x2) > 0.5
# and pairs for which p(x1, x2) < 0.5
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks= 0.5, color="black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks= 0.5, color="black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks= 0.5, color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)