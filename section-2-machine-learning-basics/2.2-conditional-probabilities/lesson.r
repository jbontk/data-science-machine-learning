#
# Conditional Probabilities
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@e14a534ff09749ca9f21af86a841c8af/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@98874ddccba842929d24878cce353fcb
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#conditional-probabilities-1
#

# Conditional probabilities for each class:
# pk(x) = Pr(Y = k / X = x), for k = 1, ..., K

# In machine learning, this is referred to as Bayes' Rule. This is a theoretical rule because
# in practice we don't know p(x). Having a good estimate of the  will suffice for us to build
# optimal prediction models, since we can control the balance between specificity and sensitivity however we wish.
# In fact, estimating these conditional probabilities can be thought of as the main challenge of machine learning.


#
# Conditional expectations and loss function
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@e14a534ff09749ca9f21af86a841c8af/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@06cba42cb3a04aa0846acc08421ae132
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#conditional-expectations
# https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#conditional-expectation-minimizes-squared-loss-function
#