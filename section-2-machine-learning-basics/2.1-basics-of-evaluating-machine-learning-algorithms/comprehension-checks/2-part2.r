# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@6ece4b7276ca4fbbb53cad7932f5a1d3
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species