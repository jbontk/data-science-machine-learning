# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2b3a4f4bd3324c45aa8395f5c3bbfd1d/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@53c9b311fc5a4b5b91193614dc348c49

# Q1
library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)


# Q2
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
# We see that the samples from the same tissue type are closer to each other.

