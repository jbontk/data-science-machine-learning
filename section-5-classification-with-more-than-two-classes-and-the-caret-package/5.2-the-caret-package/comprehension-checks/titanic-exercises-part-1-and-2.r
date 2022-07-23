# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@0c52b960e83149df8d356387b278cd32/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@a1dacb5b335c4a968d54cf1f00d339d5
# Part 1
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1: my answer (same as official answer)
y <- titanic_clean$Survived
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

test_set <- titanic_clean %>% slice(test_index)
train_set <- titanic_clean %>% slice(-test_index)

nrow(train_set)
nrow(test_set)

mean(train_set$Survived == 1)


# Q2: my answer (ok)
set.seed(3, sample.kind = "Rounding")
y_hat <- sample(c(0, 1), length(test_index), replace = TRUE)

mean(test_set$Survived == y_hat)
# Q2: official answer
set.seed(3, sample.kind = "Rounding")
# guess with equal probability of survival
guess <- sample(c(0, 1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)


# Q3a: my answer (ok)
mean(train_set[train_set$Sex == "male", "Survived"] == 1)
# Q3a: official answer
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  #filter(Sex == "male") %>%
  pull(Survived)

# Q3b: my answer (same as official answer)
sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_model == test_set$Survived)


# Q4a: my answer (same as official answer)
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))


# Q4b: my answer (same as official answer)
Pclass_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(Pclass_model == test_set$Survived)


# Q4c: my answer (same as official answer)
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1))

# Q4d: my answer (same as official answer)
Pclass_and_sex_model <- ifelse(test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2), 1, 0)
mean(Pclass_and_sex_model == test_set$Survived)

# Q5a: my answer (same as official answer)
cm_sex <- confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
cm_Pclass <- confusionMatrix(data = factor(Pclass_model), reference = factor(test_set$Survived))
cm_Pclass_and_sex <- confusionMatrix(data = factor(Pclass_and_sex_model), reference = factor(test_set$Survived))


# Q5b: my answer (ok) (official answer is the same as the previous one, does not bring any value)
cm_sex$byClass["Balanced Accuracy"]
cm_Pclass$byClass["Balanced Accuracy"]
cm_Pclass_and_sex$byClass["Balanced Accuracy"]

# Q6: my answer
F_meas(data = factor(sex_model), reference = factor(test_set$Survived))
F_meas(data = factor(Pclass_model), reference = factor(test_set$Survived))
F_meas(data = factor(Pclass_and_sex_model), reference = factor(test_set$Survived))



# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@0c52b960e83149df8d356387b278cd32/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@4ac2f397cb024f5893744d4a8c3bc16e
# Part 2
# Q7: my answer (ok)
library(caret)
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_pred <- predict(fit_lda, newdata = test_set)
confusionMatrix(data = lda_pred, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_pred <- predict(fit_qda, newdata = test_set)
confusionMatrix(qda_pred, reference = test_set$Survived)$overall["Accuracy"] # official answer does this instead: mean(qda_pred == test_set$Survived)

# Q8: my answer (same as official answer)
set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds <- predict(fit_glm, test_set)
mean(glm_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_glm_4 <- train(Survived ~ Age + Sex + Pclass + Fare, method = "glm", data = train_set)
glm_4_preds <- predict(fit_glm_4, test_set)
mean(glm_4_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(fit_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

# Q9a: my answer (same as official answer)
set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))
fit_knn$bestTune

# Q9b: my answer (same as official answer)
ggplot(fit_knn)

# Q9c: my answer (same as official answer)
knn_preds <- predict(fit_knn, test_set)
mean(knn_preds == test_set$Survived)

# Q10: my answer (same as official answer)
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = (1 - .1))
fit_knn_k_fold_cross_validation <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)), trControl = control)
fit_knn_k_fold_cross_validation$bestTune
knn_preds_cv <- predict(fit_knn_k_fold_cross_validation, test_set)
mean(knn_preds_cv == test_set$Survived)

# Q11a: my answer (same as official answer)
set.seed(10, sample.kind = "Rounding")
fit_rpart <- train(Survived ~ ., method = "rpart", data = train_set, tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
fit_rpart$bestTune
mean(predict(fit_rpart, test_set) == test_set$Survived)

# Q11b: my answer (ok)
plot(fit_rpart$finalModel) # official answer adds margin parameter: plot(train_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel)

# Q12: my answer (same as official answer)
set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ ., data = train_set, method = "rf", tuneGrid = data.frame(mtry = 1:7), ntree = 100)
train_rf$bestTune
mean(predict(train_rf, test_set) == test_set$Survived)
varImp(train_rf)