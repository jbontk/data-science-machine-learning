# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@e14a534ff09749ca9f21af86a841c8af/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@fc21ea17f9b247af99817f63ea72c7a9
# Q1: my answer (same as official answer)
# P(dis / test+) = P(test+ / dis) x P(dis) / P(test+) (Bayes)
# and P(test+) = P(test+ / dis) x P(dis) + P(test+ / health) x P(health) (Law of total probability)
# => P(dis / test+) = P(test+ / dis) x P(dis) / [P(test+ / dis) x P(dis) + P(test+ / health) x P(health)]
# = P(test+ / dis) x P(dis) / [P(test+ / dis) x P(dis) + (1 - P(test- / health)) x P(health)]
# = 0.85 x 0.02 / [0.85 x 0.02 + (1 - 0.9) x 0.98]
0.85 * 0.02 / (0.85 * 0.02 + (1 - 0.9) * 0.98)

# Q2: getting started
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
# Q2: my answer
# P(test+) = P(test+ / dis) x P(dis) + P(test+ / health) x P(health)
.85*.02+.1*.98
# Q2: official answer
mean(test)


# Q3: my answer
# P(dis / test-) = 1 - P(health / test-) = 1 - P(test- / health) x P(health) / P(test-)
# = 1 - P(test- / health) x P(health) / [1 - P(test+)]
1-.9*.98/(1-.115)
# Q3: official answer
mean(disease[test==0])


# Q4: my answer (same as official answer)
mean(disease[test==1])


# Q5: my answer
mean(disease[test==1]) / mean(disease)
# Q5: official answer
mean(disease[test==1]==1)/mean(disease==1)