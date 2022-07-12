#
# Matrices
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@b108e10e50c64daa88a5f525ba4b3f02
# https://rafalab.github.io/dsbook/large-datasets.html#matrix-algebra
#

# Machine learning problems often involve datasets that are larger than the MNIST dataset.
# There is a variety of computational techniques and statistical concepts that are useful for the analysis of
# large datasets.

# In Machine Learning, situations in which all predictors are numeric, or can be converted to numeric in a
# meaningful way, are common.
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
# In these cases, it is convenient to save the predictors in a matrix and the outcome in a vector rather than using
# a dataframe.
class(mnist$train$images) # [1] "matrix" "array"

# Take the first 1000 predictors so that it is more manageable:
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# The main reason for using matrices is that certain mathematical operations needed to develop efficient code
# can be performed using techniques from a branch of mathematics called linear algebra.

# Linear algebra and matrix notation are key elements of the language used in academic papers
# describing machine learning techniques.

# To motivate the use of matrices, we will pose five questions/challenges:
# 1. Do some digits require more ink than others? Study the distribution of the total pixel darkness and how it varies
# by digits.
#
# 2. Are some pixels uninformative? Study the variation of each pixel and remove predictors (columns) associated
# with pixels that don’t change much and thus can’t provide much information for classification.
#
# 3. Can we remove smudges? First, look at the distribution of all pixel values. Use this to pick a cutoff to define
# unwritten space. Then, set anything below that cutoff to 0.
#
# 4. Binarize the data. First, look at the distribution of all pixel values. Use this to pick a cutoff to distinguish
# between writing and no writing. Then, convert all entries into either 1 or 0, respectively.
#
# 5. Scale each of the predictors in each entry to have the same average and standard deviation.

#
# Matrix Notation
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@b1259cbf53b247c59729c9756335c507
# https://rafalab.github.io/dsbook/large-datasets.html#notation-2
#

#  Column of the x matrix containing the 1st pixel:
length(x[,1]) # it is a vector of 1000 entries

# A matrix can be defined as a series of vectors of the same size joined together as columns:
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2) # combine by columns
rbind(x_1, x_2) # combine by rows

# Dimension of the matrix x:
dim(x) # [1] 1000 784 i.e. 1000 rows x 784 columns

# Vectors are N x 1 matrices. However, in R, a vector does not have a dimension:
dim(x_1) # NULL

# Yet we can explicitly convert a vector into a matrix using the function as.matrix:
dim(as.matrix(x_1)) # [1] 5 1


#
# Converting a Vector to a Matrix
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@19bafdecb33e43d6b8e924129f4c7519
# https://rafalab.github.io/dsbook/large-datasets.html#converting-a-vector-to-a-matrix
#

my_vector <- 1:15
# Converting to a matrix and specifying the number of rows and columns:
mat <- matrix(my_vector, 5, 3)
# We can fill by row using the byrow argument. So to transpose mat, we can use:
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
# The same is obtained using directly the function t:
identical(t(mat), mat_t)


# To put the pixel intensities of our 3rd entry, which is a 4, we can use:
grid <- matrix(x[3,], 28, 28) # (28 x 28 = 784, and y[3] = 4

# To confirm, image should draw a 4,
image(1:28, 1:28, grid)

# But need to flip the image back:
image(1:28, 1:28, grid[, 28:1])


#
# Row and Column Summaries and Apply
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@468172799ba645079d9b5d6fb5357839
# https://rafalab.github.io/dsbook/large-datasets.html#row-and-column-summaries
# https://rafalab.github.io/dsbook/large-datasets.html#apply
#

sums <- rowSums(x)
avg <- rowMeans(x)

# Generating a box plot with previous values.
# From the plot, we can see that 1s use less ink than the other digits:
tibble(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")
# That answers Challenge 1.

# We can compute the column sums and averages using the function colSums and colMeans, respectively.

# The matrixStats package adds functions that performs operations on each row or column very efficiently,
# including the functions rowSds and colSds.


# Apply:
# Using apply, rowMeans can be written as:
avgs <- apply(x, 1, mean)
# Where the first argument is the matrix, the second is the dimension, 1 for rows, 2 for columns,
# and the third is the function

# Cf.:
identical(avgs, avg) # [1] TRUE

# Same for standard deviations:
sds <- apply(x, 2, sd)

# The tradeoff for this flexibility is that these operations are not as fast as dedicated functions such as rowMeans.

#
# Filtering Columns Based on Summaries
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@41eccf3351c643e3968da8e71bdf06fb
# https://rafalab.github.io/dsbook/large-datasets.html#filtering-columns-based-on-summaries
#

# We now turn to Challenge 2: studying the variation of each pixel and removing columns associated with pixels
# that don’t change much and thus do not inform the classification.

library(matrixStats)
sds <- colSds(x)

# A quick look at the distribution of these values shows that some pixels have very low entry to entry variability:
qplot(sds, bins = "30", color = I("black"))

# This makes sense since we don’t write in some parts of the box. Here is the variance plotted by location:
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

# We see little variation in the corners.
# We could remove features that have no variation since they can't help us predict.
new_x <- x[ ,colSds(x) > 60] # Keeping only columns which have "high enough" standard deviation
dim(new_x) # [1] 1000 314 => more than half of the predictors are removed

class(x[,1]) #> [1] "integer"
dim(x[1,]) #> NULL => the result of subsetting a matrix is no longer a matrix but a vector

# Preserving the matrix structure via drop=FALSE:
class(x[ , 1, drop=FALSE]) #> [1] "matrix" "array"
dim(x[, 1, drop=FALSE]) #> [1] 1000    1



#
# Indexing with Matrices and Binarizing the Data
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@f30a73e1b0854a6cb8594a3dbb1c79ee
# https://rafalab.github.io/dsbook/large-datasets.html#indexing-with-matrices
# https://rafalab.github.io/dsbook/large-datasets.html#binarizing-the-data
#

# Now turning a mtrix into a vector:
mat <- matrix(1:15, 5, 3)
as.vector(mat)

qplot(as.vector(x), bins = 30, color = I("black"))

# Challenge 3: zeroing smudges:
new_x <- x
new_x[new_x < 50] <- 0

# To see what it does, let's look at a smaller matrix:
mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat
#>      [,1] [,2] [,3]
#> [1,]    0    6   11
#> [2,]    0    7   12
#> [3,]    3    8   13
#> [4,]    4    9   14
#> [5,]    5   10   15

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat
#>      [,1] [,2] [,3]
#> [1,]    1    6    0
#> [2,]    2    0   12
#> [3,]    3    0   13
#> [4,]    4    0   14
#> [5,]    5    0   15


# Binarizing the Data
# The histogram above seems to suggest that this data is mostly binary. A pixel either has ink or does not.
# Binarizing the data using matrix operations:
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

# We can also convert to a matrix of logicals and then coerce to numbers like this:
bin_X <- (x > 255/2)*1

#
# Vectorization for Matrices and Matrix Algebra Operations
# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+1T2022/block-v1:HarvardX+PH125.8x+1T2022+type@sequential+block@296b63c21a8e4ef0a3f642c761d521ea/block-v1:HarvardX+PH125.8x+1T2022+type@vertical+block@3fff7246fe5c46db81361355ef72df4d
# https://rafalab.github.io/dsbook/large-datasets.html#vectorization-for-matrices
# https://rafalab.github.io/dsbook/large-datasets.html#matrix-algebra-operations
#

# We can scale each row of a matrix like this:
(x - rowMeans(x)) / rowSds(x)

# For columns, we need to transpose first:
t(t(X) - colMeans(X))

# We can also use a function called sweep that works similarly to apply.
# It takes each entry of a vector and subtracts it from the corresponding row or column.
x_mean_0 <- sweep(x, 2, colMeans(x))

# The default arithmetic operation is subtract. To define another operation, use the last argument FUN:
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

# Matrix Algebra Operations:
# Matrix multiplication:
t(x) %*% x

# Cross product:
crossprod(x)

# Computing the inverse of a function:
solve(crossprod(x))

# QR decomposition (https://fr.wikipedia.org/wiki/D%C3%A9composition_QR):
qr(x)