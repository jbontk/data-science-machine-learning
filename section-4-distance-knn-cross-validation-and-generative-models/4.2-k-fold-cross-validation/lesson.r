#
# k-fold Cross-Validation
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2faa56e611464cf4bfd42b13ac7fffa5/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@1dcfc95bcce4409ca0e73d8f1d8cc639
# https://rafalab.github.io/dsbook/cross-validation.html#k-fold-cross-validation
#

# Goal of Machine Learning: find an algorithm that produces predictor y_hat for an outcome y, that minimizes Mean Square Error.
# Mean Square Error = MSE = E { SUM [(Y_hat_i - Y_i)^2, 1, N] / N } = **True Error**
# MSE = MSE_hat = SUM [(Y_hat_i - Y_i)^2, 1, N] / N = **Apparent Error**

# For each set of algorithm parameters being considered, we want an estimate of the MSE and then we will choose the parameters
# with the smallest MSE. Cross-Validation provides this estimate.

# There are 2 important characteristics of the **Apparent Error** to keep in mind:
# 1. it is a random variable since our data is random => an algorithm having lower Apparent Error than another
# may be due to luck

# 2. if we train an algorithm on the same dataset that we used to compute the apparent error, we might be overtraining.
# And we end up in general with an underestimate of the True Error.

# We saw an extreme example of this with the k-nearest neighbors when we said k equals to 1.
# Cross-validation is a technique that permits us to alleviate 1. and 2.

# To understand cross-validation, it helps to think of the true error, a theoretical quantity, as the average of many, many apparent errors obtained
# by applying the algorithm to, B, new random samples of the data, none of them used to train the alogrithm.

# We therefore can estimate the **True Error** as follows:
# Estimated MSE = SUM [SUM [(Y_hat_i_b - Y_i_b)^2, 1, N] / N, 1, B] / B

# There are several approaches to Cross-Validation.
# For all of them, the general idea is to randomly generate smaller data sets that are not used for training
# and instead are used to estimate the true error.
# One approach we see here is **k-fold Cross-Validation**.

# REMEMBER: do not use the test set at all for training

# For most Machine Learning algorithms, we need to select parameters, for example the number of neighbors k in k-nearest neighbors.
# We need to optimize those parameters without using our test set and we know that if we optimize and evaluate on the same dataset,
# we will overtrain. This is where Cross-Validation is most useful.

# Estimated MSE (lambda) = SUM [SUM [(Y_hat_i_b(lambda) - Y_i_b)^2, 1, N] / N, 1, B] / B
# Where lambda is set of parameters, such as the number of neighbors in k-nearest neighbors algorithm.
# With k-fold Cross-Validation, you apply this formula k times, under the hypothesis that data sets B are independent random samples.
# Then, we compute the average MSE and obtain an estimate of our loss. Finally, we can select the optimal parameter that minimized the MSE.

# In terms of how to select  for cross validation, larger values of  are preferable but they will also take much more computational time.
# For this reason, the choices of 5 and 10 are common.

# One way we can improve the variance of our final estimate is to take more samples.
# To do this, we would no longer require that training set be partitioned into non-overlapping sets.
# Instead we would just pick k sets of some size at random. One popular version of this technique, at each fold, picks observations at random with replacement,
# which means that the same observation can appear twice.
# This is generally referred to as the bootstrap approach. This is the default approach in the caret package.

########################################################################################################################################
########################################################################################################################################

#
# Bootstrap
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@2faa56e611464cf4bfd42b13ac7fffa5/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@a60537bd48fa4b93ab9c2825c3835979
# https://rafalab.github.io/dsbook/cross-validation.html#bootstrap
#

set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

# The population median income is:
m <- median(income)
m  #> [1] 44938.54

# Suppose we don't have access to the entire population, but want to estimate the median m. We take a sample of 100 and estimate the population median m
# with the sample median M:
N <- 100
X <- sample(income, N)
median(X) #> [1] 47432.95

# Can we construct a confidence interval? What is the distribution of M?
# Because we are simulating the data, we can use a Monte Carlo simulation to learn the distribution of M:
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") +
  geom_abline()
grid.arrange(p1, p2, ncol = 2)

# We can see that the 95% confidence interval based on Central Limit Theorem:
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1) # [1] 27858.11 66580.33
# is quite different from the confidence interval we would generate if we know the actual distribution of M:
quantile(M, c(0.025, 0.975)) #     2.5%    97.5%
                             # 34555.74 58599.42


# The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution. The general idea is relatively simple. We act as if the observed sample is the population.
# We then sample (with replacement) datasets, of the same sample size as the original dataset. Then we compute the summary statistic, in this case the median, on these bootstrap samples.
# Theory tells us that, in many situations, the distribution of the statistics obtained with bootstrap samples approximate the distribution of our actual statistic.
# This is how we construct bootstrap samples and an approximate distribution:
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

# Note a confidence interval constructed with the bootstrap is much closer to one constructed with the theoretical distribution:
quantile(M_star, c(0.025, 0.975)) #     2.5%    97.5%
                                  # 37933.74 64164.71