sin(2*pi/3)
# i am a comment!

print(sin(2*pi/3))
print(5^2) # actual power, not xor
print(sqrt(4))
print(log(1)) # ln
print(c(1, 2, 3, 4, 5)) # collection of the first five integers
print(c(1, 2, 3, 4, 5) * 2) # collection of the first five even integers

print(8^(1/3))
print(exp(2))

print("")

x <- 1 # left assignment
x # display
2 -> x # right assignment
x # display
x = 3 # this is bad style
x
(x <- 1) # print-assign (walrus)

# alt + - is the shortcut for left assign in rstudio

var1 <- sum(c(1, 2, 3, 4))
var2 <- exp(1) * pi
var3 <- var1 + var2
var3

print("")

# a collection is a vector -- a sequence of data points of the same type
# vector operations are done element-wise
print(c(1, 2, 3))
print(c(1, 2, 3) + c(4, 5, 6))
print(c(1, 2, 3) * c(4, 5, 6))

print(seq(0, 1, 0.1)) # like linspace
print(seq(from = 0, to = 1, by = 0.1))
print(seq(from = 0, to = 20, length = 5))
print(2:10) # like range
print((2:10) * 2)

seq(4, 5, by = 0.3)
seq(4, 5, length = 5)

print("")

print(2 < 3)
print(2 <= 3)
print(2 == 2)
print(2 != 2)
print(c(2 > 1, 4 > 2, pi == 3))
print(c(1, 2, 3) * 2 == c(2, 5, 6))

print(c(2 > 1, 3 > 1) & c(1 < 0, 3 > 0)) # and
print(2 > 1 && 3 > 1) # short-circuit and
print(c(2 > 1, 3 > 1) | c(1 < 0, 3 > 0)) # and
print(1 < 0 || 3 > 0) # short-circuit or

!(2 > 3) # not
!c(TRUE, TRUE, FALSE)

# operators have precedence
c(2 > 1, 3 < 1) | c(0 > 1, 4 < 1) # note that we cannot short circuit this
TRUE && FALSE && TRUE
0 > 2 || 3 < 4 || 1 < 0

# avoid writing T or F (instead write TRUE or FALSE)
print(any(c(TRUE, FALSE)))
print(all(c(TRUE, FALSE)))

# we can use a trick (which also works in some other languages) to count the number of true statements
TRUE + TRUE + TRUE + FALSE + FALSE
sum(c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
sum(c(1, 2, 3, 4) > 2.5) # number of TRUE
mean(c(1, 2, 3, 4) > 2.5) # proportion of TRUE
sum(c(1, 2, 3, 4) > 2.5) > 0 # equivalent to any()
sum(c(1, 2, 3, 4) > 2.5) == 4 # equivalent to all()

# variable names can include alphanumericals, underscores and full stops (but may not start with a digit)

print("")

print(class(1)) # double
print(class(as.integer(1))) # int
print(class(3+2i)) # complex
print(class(TRUE)) # logical
print(class(NA)) # logical
print(class("cat")) # character (string)
print("cat" == 'cat')

print(is.numeric(1))
print(is.integer(1))
print(as.double(as.integer(1)))

x <- c(3, NA, 6)
print(is.na(x))
print(mean(x))
print(mean(x, na.rm = TRUE)) # remove NA
print(sum(x, na.rm = TRUE))
print(sum(na.omit(x)))

print("")

# in r, it is expected to have spaces around the equals in kwargs
# r can generate probability distributions!
dnorm(1.96, mean = 0, sd = 1) # pdf for the normal distribution
pnorm(1.06, mean = 0, sd = 1) # cdf for the normal distribution
qnorm(0.9756, mean = 0, sd = 1) # quantile (inverse of cdf) for the normal distribution
rnorm(1, mean = 0, sd = 1) # random variables that follow a normal distribution

dexp(1.96, rate = 1) # density
pexp(1.06, rate = 1) # probability
qexp(0.9756, rate = 1) # quantile
rexp(1, rate = 1) # random

dbinom(1, size = 10, prob = 0.5)
pbinom(5, size = 10, prob = 0.5)
qbinom(0.9756, size = 10, prob = 0.5)
rbinom(1, size = 10, prob = 0.5)

# r can also generate random observations!
runif(n = 2)
runif(n = 4, min = 2, max = 7)

rnorm(4)
rnorm(4, mean = 0, sd = 1)

rgamma(4, 1)
rgamma(n = 4, shape = 1, rate = 0.5)

rbinom(4, 100, 0.1)

# semicolons work much like as in python

x <- 2; x <- 2 * x; x

TRUE + T + FALSE * F + T * FALSE + F
1 + 1 + 0 * 0 + 1 * 0 + 0

# we get help by prepending ? to a function
# ?sqrt
# if we have forgotten which package a function is in, we can prepend ?? to search for something in the documentation
# ??read_excel
