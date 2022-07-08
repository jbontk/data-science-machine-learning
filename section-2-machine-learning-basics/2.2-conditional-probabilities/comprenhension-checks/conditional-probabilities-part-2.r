# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@e14a534ff09749ca9f21af86a841c8af/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@1df549bf5f254d378f29486b7ea3f48b
# Q6
library(dslabs)
data("heights")
# Q6: MISSING CODE
heights %>%
  mutate(height = round(height)) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)


# Q7
ps <- seq(0, 1, 0.1)
heights %>%
  # Q7: MISSING CODE
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


# Q8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0, 1, 0.1)
dat %>%
  # Q8: MISSING CODE
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)