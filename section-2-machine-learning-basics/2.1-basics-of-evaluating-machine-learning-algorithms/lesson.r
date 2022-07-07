library(tidyverse)
library(dslabs)
library(caret)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ] # the rows of heights which match the test_index
train_set <- heights[-test_index, ] # the rows of heights which don't match the ones in test_index

# insight: males slightly taller than females:
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# overall accuracy
mean(y_hat == test_set$sex)
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)


# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#
# The Confusion Matrix
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@af5e0fe30ef8460abf5c7474adb9c392
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#the-confusion-matrix
#
table(predicted = y_hat, actual = test_set$sex)

# Computing accuracy separately for each sex: predictions are ummuch more accurate for males than for females
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

# Why? Because there are much more males than females in the dataset (PREVALENCE)
# => If training data is biaised => likely to develop biaised algorithms
prev <- mean(y == "Male")
prev

# Evaluate an algorithm other than with accuracy, using SENSITIVITY and SPECIFICITY, in such way that PREVALENCE does not cloud the assessment
# High sensitivity => Y = 1 => Y_HAT = 1 = ability of an algorithm to predict a positive outcome when the outcome is positive
# High specificity => Y = 0 => Y_HAT = 0 = ability of an algorithm to not predict a positive outcome when the outcome is not positive
# High specificity (2) => Y_HAT = 1 => Y = 1 = proportion of positive calls that are actually positive
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

# 4 entries of the confusion matrix:
#                       Actually Positive   Actually Negative                  
# Predicted positive    True Positive (TP)  False Positive (FP)
# Predicted negative    False Negative (FN) True Negative (TN)

# Measure of  Name 1 Name 2     Definition      Probability Representation
# sensitivity TPR    Recall     TP / (TP + FN)  Pr(Y_HAT = 1 / Y = 1)
# specificity TNR    1-FPR      TN / (TN + FP)  Pr(Y_HAT = 0 / Y = 0)
# specificity PPV    Precision  TP / (TP + FP)  Pr(Y = 1 / Y_HAT = 1)         PPV = Positive Predictive Value

cm$overall["Accuracy"]
cm$byClass[c("Sensitivity", "Specificity", "Prevalence")]

#
# Balanced accuracy and F1 score
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@d17b671fe05944eb8ceeddeeecb3a71c
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#balanced-accuracy-and-f_1-score
#

# Balanced Accuracy = average of specificity and sensitivity
# These last 2 are rates, therefore it is preferred to compute the harmonic average

# F-1 score = harmonic average of precision and recall = 2 x precision x recall / (precision + recall)

# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>%
  ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

#
# Prevalence matters in practice
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@52d7dbddfbf24129a405e8ce63394f86
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#prevalence-matters-in-practice
#

#
# ROC and precision-recall curves
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@05a02927ef514050965711234433da1c
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#roc-and-precision-recall-curves
#

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <-
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
      factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x,
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index),
                  replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE,
                  prob=c(p, 1-p)) %>%
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()