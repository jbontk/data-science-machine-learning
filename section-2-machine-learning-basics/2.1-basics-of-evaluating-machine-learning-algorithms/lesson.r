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


# The Confusion Matrix
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@af5e0fe30ef8460abf5c7474adb9c392
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#the-confusion-matrix
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