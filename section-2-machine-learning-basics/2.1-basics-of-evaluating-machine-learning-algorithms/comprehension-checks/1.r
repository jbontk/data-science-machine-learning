# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@d169dc0f9f564bb5b2e4022bce896d2e
# How many features are available to us for prediction in the mnist digits dataset?
# You can download the mnist dataset using the read_mnist() function from the dslabs package.
library(dslabs)
mnist <- read_mnist()


str(mnist)
