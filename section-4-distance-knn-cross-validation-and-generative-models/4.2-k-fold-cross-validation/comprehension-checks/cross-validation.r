# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2faa56e611464cf4bfd42b13ac7fffa5/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@c03183789cca4593a6d3e17fb5099f13
# Q1
library(tidyverse)
library(caret)

set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

# Q2
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

# Q3: my answer (ok)
length(pvals[pvals < 0.01])
# Q3: official answer
ind <- which(pvals <= 0.01)
length(ind)


# Q4: my answer (same as official answer)
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results


# Q5
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)



# Q7: my answer
library(dslabs)
data(tissue_gene_expression)
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2))))
ggplot(fit)