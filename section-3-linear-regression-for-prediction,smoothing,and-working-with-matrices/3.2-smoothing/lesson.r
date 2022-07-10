#
# Introduction to Smoothing
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@5e2f559f1188441fa6bd972e356994c3/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@d23c313d9ea2470e8a7e01f6801acd39
# https://rafalab.github.io/dsbook/smoothing.html
#

# Smoothing is a very powerful technique used all across data analysis
# Other names given to this technique are curve fitting and low pass filtering.
# It is designed to detect trends in the presence of noisy data in cases in which the shape of the trend is unknown.

# Concepts behind smoothing techniques are extremely useful in machine learning because conditional expectations/probabilities
# can be thought of as trends of unknown shapes that we need to estimate in the presence of uncertainty.

# To explain, let's start with a problem with just one predictor:
# let's try to estimate the time trend in the 2008 US popular vote margin
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# Scatter plot (previous qplot) along with regression line:
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(method = "lm")


#
# Bin Smoothing and Kernels
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@5e2f559f1188441fa6bd972e356994c3/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@178e776732fc4be0a3e473e297ee1df8
# https://rafalab.github.io/dsbook/smoothing.html#bin-smoothing
# https://rafalab.github.io/dsbook/smoothing.html#kernels
#

# f(x): could be the poll results from the previous example.
# Smoothing: group data points into strata in which the value of f(x) can be assumed to be constant

# Bin Smoothing: we assume that f(x) is a constant within a certain interval
# That interval is called: Window Size, Bandwidth or Span

# In the previous example, we could say that the poll results do not change much in a week, i.e.
# if we fix a day to be the center of our week, x0, then for any day x such that abs(x - x0) <= 3.5,
# f(x) is a constant mu
# => E(Yi / Xi = xi) ~= mu if abs(xi - x0) <= 3.5

# That assumption implies that a good estimate for f(x) is the average of the Yi values in the window:
# --------------------------------------------------------------------------------------------
# | f_hat(x0) = sum(Yi, i E A0) / N0, where A0 is the set if indexes i / abs(xi - x0) <= 3.5,|
# --------------------------------------------------------------------------------------------
# and N0 = |A0| (= number of indexes = cardinality)

# For Bin Smoothing, the idea is to calculate this average for each value of x as the center, i.e.:
span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


# Kernels
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@5e2f559f1188441fa6bd972e356994c3/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@178e776732fc4be0a3e473e297ee1df8
# https://rafalab.github.io/dsbook/smoothing.html#kernels
#

# Bin Smoothing shown by previous code is quite wiggly.
# One reason is that each time the window moves, two points change.
# That can be attenuated by taking weighted averages that give the center point more weight than far away points,
# with the two points at the edge receiving very little weight.

# Using this time kernel="normal" which uses the normal density to assign weights, result is less wiggly:
span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


# Local Weighted Regression (loess)
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@5e2f559f1188441fa6bd972e356994c3/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@af43f54714794279877b1296c61a45a0
# https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess
#

# One limitation of Bin Smoother is that we need *small* windows for the approximately constant assumptions to hold.
# As a result, we end up with a *small* number of data points to average and obtain imprecise estimates f_hat(x).

# Local Weighted Regression allows us to use larger window sizes.
# It leverages Taylor's Theorem: ~~if you look close enough at any smooth function f(x), it will look like a line~~

# Instead of a constant, we assume that the function is locally linear.
#  We can consider larger window sizes with the linear assumption than with a constant.

# In the previous example, we assumed that f(x) was approximately a constant in a 1-week window
# We now assume that f(x) is approximately linear in a 3-week window:
# E(Yi / Xi = xi) = beta_0 + beta_1(xi - x0) if abs(xi - x0) <= 21

# The final result is a smoother fit than the bin smoother since we use larger sample sizes to estimate our local parameters:
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")