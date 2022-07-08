# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@75feae7f54be4ae6ab2f0c8ac90f5e70/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@2fb8fd3450cd4b67ad243b2f5baa718b
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1: my answer (ok)
mean(y[(x == "online")] == "Female")
# Q1: answer
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))


# Q2: my answer (same as official answer)
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean(y == y_hat)


# Q3: my answer (same as official answer)
table(y_hat, y)


# Q4: my answer (same as official answer)
sensitivity(data = y_hat, reference = y)


# Q5: my answer (same as official answer)
specificity(data = y_hat, reference = y)


# Q6: my answer (ok)
dat %>% summarize(prop_female = mean(sex == "Female"))
# Q6: official answer
mean(y == "Female")