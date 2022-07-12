# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@97e31641722a42f9b2726c80d9f05164

# Q1
x <- matrix(rnorm(100*10), 100, 10)


# Q2
dim(x)
nrow(x)
ncol(x)

# Q3
x <- matrix(0, 3,5)
x + seq(nrow(x))
sweep(x, 1, 1:nrow(x),"+")

# Q4
sweep(x, 2, 1:ncol(x), FUN = "+")

# Q5
rowMeans(x)
colMeans(x)

# Q6: my answer (ok)
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
# In these cases, it is convenient to save the predictors in a matrix and the outcome in a vector rather than using
# a dataframe.
class(mnist$train$images) # [1] "matrix" "array"

x <- mnist$train$images
x_sel <- x < 205 & x > 50
x_sel_mut <- ifelse(x_sel, 1, 0)
mean(rowMeans(x_sel_mut))
# Q6: official answer
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels