myVector <- c(7, 11, 13)

# we access elements in a vector like we do with lists
# however, like matlab, they are indexed from 1 :c
print(myVector[1])
print(myVector[3])
print(typeof(myVector))

x <- c(1, 3, 6, 2, 7, 4, 8, 1, 0, 8)
length(x)
sort(x)
sort(x, decreasing = TRUE)
rev(x) # reverse
rank(x) # statistical rank (i.e., position in a sorted vector)
head(x) # return the first (by default, 6) few elements

head(x, 0)
head(x, 2)
head(x, -1) # return up to the last
tail(x) # return the last few elements

A <- c(4, 5, 2, 7)
B <- c(2, 1, 7, 3)
C <- c(2, 3, 7)
is.element(C, A) # for each element in C, is it in A?
is.element(A, C) # for each element in A, is it in C?

# very much like python set operators
intersect(A, B)
union(A, B)
setdiff(A, B)
setdiff(B, A)

# matrices are 2d vectors
# arrays are nd vectors
# matrices and arrays are 'data structures' (only could a 1d array, i.e., list, be considered a data type)

(X <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)) # do we fill in these numbers over the rows before going by columns
(X <- matrix(1:12, nrow = 4, ncol = 3, byrow = FALSE)) # we fill down the columns before going over the rows

(X <- array(1:24, dim = c(2, 3, 4)))

# r will recycle (repeat elements from the beginning) arrays, if one is shorter:
x <- c(1, 2, 3, 4, 5, 6)
y <- c(1, 2, 3)
x + y

# we can take advantage of this when declaring arrays and matrices
matrix(1:4, ncol = 3, nrow = 4)

B = cbind(cbind(1:4, 5:8), 9:12) # column concatenate
C = rbind(rbind(1:3, 4:6), 7:9) # row concatenate

print("")

(A <- matrix(c(2, 3, 4, 5), nrow = 2, ncol = 2, byrow = TRUE))
(B <- matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2, byrow = FALSE))
(I2 <- diag(nrow = 2)) # identity matrix

A + B
A * B # goes element-wise, would be .* in matlab
A / B
A %*% I2 # matrix multiplication (* in matlab)
A %*% B
t(B) # transpose

(A <- matrix(1:4, ncol = 2))
(b <- c(1, 1))
(x <- solve(A, b)) # solve Ax = b

solve(A) # find the inverse of A
# solve(matrix(0, nrow = 3, ncol = 3)) # if the matrix is not invertible, we get "Error: system is exactly singular"

print("")

# we can apply a function to the rows or columns of a matrix with apply()
# MARGIN = 1 means by row, MARGIN = 2 means by column
(X <- matrix(c(1:4, 1, 6:8), nrow = 2))
apply(X, MARGIN = 1, FUN = sum) # equivalent to rowSums(X)
apply(X, MARGIN = 2, FUN = mean) # equivalent to colMeans(X)
sum(X)

# r will automatically convert elements in a vector, matrix or array to the same data type
(myVector <- c(1, 2, "A", TRUE))

# lists can hold different data types
(A <- list(TRUE, my.matrix=matrix(1:4, nrow=2), c(1+2i,3), "R is my friend"))
# the second element is named my.matrix
# this means we can access elements like a dictionary with $
A$my.matrix
A[2]

print("")


# dataframes are tables (in the sense that each column must contain the same type of observation)

BMI <- data.frame(
    Gender = c("M", "F", "M", "F"),
    Height = c(1.83, 1.78, 1.80, 1.60),
    Weight = c(77, 68, 66, 48),
    row.names = c("Ben", "Kate", "Anthony", "Julia")
)
BMI
str(BMI)
str(1)
# and as before we can access an a row like with lists
BMI$Gender

print("")


# we can merge dataframes (e.g., if we are using data from multiple datasets)
X <- data.frame(
    GENDER = c("F", "M", "M", "M"),
    ID = c(123, 234, 345, 456),
    NAME = c("Mary", "James", "James", "Peter"),
    Height = c(170, 180, 185, 160)
)
Y <- data.frame(
    GENDER = c("M", "M", "F", "M"),
    ID = c(345, 456, 123, 234),
    NAME = c("James", "Peter", "Mary", "James"),
    Weight = c(80, 50, 70, 60)
)
Z <- data.frame(
    GENDER = c("M", "M", "F", "F"),
    ID = c(345, 456, 123, 999),
    NAME = c("James", "Peter", "Mary", "Jennifer"),
    Age = c(21, 19, 23, 99)
)

# cbind() would be unhelpful here
cbind(X, Y)

# instead, we use merge()
merge(X, Y)

# any row not present in both dataframes will be lost
merge(X, Z)

# we can, however, force the inclusion of elements from either the first or second dataframe with all.x (the first dataframe)or all.y (the second dataframe)
merge(X, Z, all.x = TRUE)
merge(X, Z, all.y = TRUE)


print("")

# factors exist. they are like enums!
x <- factor(c("blue", "green", "blue", "red", "blue", "green", "green"))
# levels() shows all the unique elements in a factor
levels(x)

# class() is distinct from type in that it shows the type of the object, while typeof() shows how the data is stored in memory
class(x)
typeof(x)

# the data structures ar # e:
c() # vector
matrix() # matrix
array() # array
list() # list
data.frame() # dataframe
factor() # vector of strings associated with an index
as.Date(0) # vector of dates
ts() # time series

print("")


# we can read and write data
# data is read into dataframes
my_data <- read.csv(file = "./data/video-games.csv")
# read tsv
my_data <- read.delim(file = "./data/video-games.txt")
# read.table is a more general form of read.csv (in a sense, because it fails for quoted strings and other csv-specific things)
my_data <- read.table(file = "./data/video-games.txt", sep = "\t")
my_data <- read.table(file = "./data/video-games.txt", header = TRUE, sep = "\t")

# like with vectors, we can use head() and tail()
head(my_data)
tail(my_data)

# because data was read into a dataframe, we can do operations on the columns
mean(my_data$metascore) # find the mean of a variable
cor(my_data$metascore, my_data$metascore) # find the correlation between two variables

# we can read characters (much like file.readlines()) with readLines
raw_data <- readLines("./data/video-games.txt", n = 5)
raw_data <- readLines("./data/video-games.txt")

# we getcwd with:
getwd()

# we can write data with
write.table(my_data, file = "./output/myfile.txt", sep = "\t")
write.csv(my_data, file = "./output/myfile.csv")
