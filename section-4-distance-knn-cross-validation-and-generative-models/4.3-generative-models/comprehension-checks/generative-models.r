# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@cf9e5492064f4e7e9787ed0d8c27d1e9/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@e253df286aa54dc9b6fd67f3e09ead37
# Q1
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind,]
x <- x[, sample(ncol(x), 10)]
# Q1: my answer (same as official answer)
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]


# Q2: my answer (ok)
library(reshape2)
means <- as.matrix(fit_lda$finalModel$means)
means_melt <- melt(means, varnames = c('gene', 'part'), value.name = 'mean')

ggplot(means_melt, aes(gene, mean, fill = part)) +
  geom_bar(stat = "identity", position = "dodge")
# Q2: official answer
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


# Q3
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# Q3: my answer (same as official answer)
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]



# Q4: my answer
t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()



# Q5: my answer (same as official answer)
set.seed(1993, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind,]
x <- x[, sample(ncol(x), 10)]
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()
# Q5: further exploration
d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)




# Q6: my answer (ok)
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
# Q6: my answer
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]
# Q6: official answer
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]