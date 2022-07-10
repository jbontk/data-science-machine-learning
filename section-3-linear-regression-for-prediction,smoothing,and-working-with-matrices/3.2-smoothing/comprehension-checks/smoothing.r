# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@5e2f559f1188441fa6bd972e356994c3/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@cd78b4eaa391404ea8d24395ef3dd1c8
# Q1
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>%
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

# Q1: my answer (ok)
dat %>% ggplot(aes(date, deaths)) +
  geom_point() +
  geom_smooth(color="red", span = 0.05, method = "loess", method.args = list(degree=1)) # 0.05 ~= 60 / 1205
# Q1: official answer:
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")


# Q2: my answer
#fit <- dat %>% mutate(x = yday(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
#dat %>%
#  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#  ggplot(aes(day, smooth, col = year)) +
#  geom_line(lwd = 2)

# Q3
library(broom)
library(dslabs)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

# Q3: my answer
mnist_27$train %>% ggplot(aes(x_2, ifelse(y == 2, 1, 0))) +
  geom_point() +
  geom_smooth(color="red", span = 0.05, method = "loess", method.args = list(degree=1))
# Q3: official answer
mnist_27$train %>%
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) +
  geom_smooth(method = "loess")
