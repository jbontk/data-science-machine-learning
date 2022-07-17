# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@1ce4ff2f28024d5ca7682b23f6158940/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@340a6a1977724190b128951c7261af48
# Q1: my answer (same as official answer)
library(rpart)
library(caret)
data(tissue_gene_expression)
set.seed(1991, sample.kind = "Rounding")
train_rpart <- with(tissue_gene_expression, train(x, y, method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, 0.01))))
ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune

# Q2: my answer (ok)
set.seed(1991, sample.kind = "Rounding")
train_rpart <- with(tissue_gene_expression, train(x, y, method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, 0.01))), control = rpart.control(minsplit = 0))
plot(train_rpart, highlight = TRUE)
max(train_rpart$results$Accuracy)
# Q2: official answer
library(caret)
library(rpart)
library(dslabs)
data("tissue_gene_expression")

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later

fit_rpart <- with(tissue_gene_expression,
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

# Q3
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# Q4: my answer (same as official answer)
library(randomForest)
set.seed(1991, sample.kind = "Rounding")
fit <- with(tissue_gene_expression, train(x, y, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1))
fit$bestTune

# Q5
imp <- varImp(fit)
imp

# Q6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
# Q6: my answer (ok)
imp
# Q6: official answer
data_frame(term = rownames(imp$importance),
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)