


Y <- data.frame(
    Gender = c("F", "M", "M", "F"),
    Height = c(165, 182, 178, 160),
    Weight = c(50, 65, 67, 55),
    Income = c(80, 90, 60, 50)
)
row.names(Y) <- c("Aiyana", "James", "John", "Abbey")
Y
Y[3, 2]
Y["John", "Height"]
Y$Weight
Y["John", ]
Y[, "Height"]
mean(Y$Income[Y$Gender == "M"])


# we can attach() a dataframe, that is, extract all of its headered columns into variables
# this is dangerous if the matrix is very large
# changes do not propagate to the created variables
attach(Y)
Gender
Y$Gender <- c("F", "F", "F", "F")
Y$Gender
Gender

sample_data <- read.csv(file = "./data/video-games.csv")
attach(sample_data)
head(sample_data)
mean(metascore)
median(metascore)
sd(metascore)
max(metascore) - min(metascore)
IQR(metascore)
mean(abs(metascore - mean(metascore)))
quantile(metascore, probs = c(0.10, 0.90))

# table() creates a frequency table for a variable
table(metascore)
table(metascore) / length(metascore)
(mytable <- table(console, metascore)) # 2d frequency table
(mytable_complete <- addmargins(mytable, FUN = sum, quiet = TRUE))

# cars is a built in dataset
# cor is the pearson correlation coefficient r (not r squared)
cor(cars$speed, cars$dist)
plot(cars$speed, cars$dist)
plot(log(cars$speed), log(cars$dist))

# to import libraries, we use library()
# dplyr has many functions, of which we will focus on six: filter(), select(). arrange(), mutate(), group_by() and summarise()
# also r comes from new zealand
# library(dplyr)
suppressMessages(library(dplyr))
head(mtcars)
filter(mtcars, mpg > 30)
select(head(mtcars), mpg, cyl, wt)
select(filter(mtcars, wt > 3, cyl >= 6), mpg, cyl, wt)

# the pipe operator %>% passes the output from the left to the right like normal pipes
# can be thought of as 'and then' in spoken language
# usually used to avoid cluttering the environment with intermediate variables generated when calculating a result, and is also more readable

# mtcars is also a built in dataset
mtcars %>% filter(mpg > 30)
mtcars %>%
    filter(mpg > 30) %>%
    select(mpg, wt)
mtcars %>%
    filter(mpg > 30) %>%
    arrange(wt)
mtcars %>%
    filter(mpg > 30) %>%
    arrange(desc(wt))

# mutate creates new columns as functions of other columns
mtcars %>%
    mutate(double_wt = wt * 2,
           kpg = mpg * 1.609,
           name = row.names(mtcars)) %>%
    select(name, double_wt, kpg)

# compare these two groups of operations
# which is more readable?
arrange(select(filter(mtcars, mpg > 30), mpg, wt, cyl), desc(wt))
mtcars %>%
    filter(mpg > 30) %>%
    select(mpg, wt, cyl) %>%
    arrange(desc(wt))

# group by (like in file explorer)
mtcars %>%
    group_by(cyl)

# generate a summary of statistics for something
mtcars %>%
    group_by(cyl) %>%
    summarise(avg_mpg = mean(mpg))

mtcars %>%
    summarise(mean_mpg = mean(mpg),
              num_cars = n(),
              sd_wt = sd(wt))

# quakes is another built in dataset
quakes %>% filter(mag >= 5.5) %>% select(depth, mag, stations)
mtcars %>% mutate(prod = mpg * wt) %>% group_by(cyl) %>% summarise(mean = mean(prod), median = median(prod))


Y <- data.frame(
    Gender = c("F", "M", "M", "F"),
    Height = c(165, 182, 178, 160),
    Weight = c(50, 65, 67, 55),
    Income = c(80, 90, 60, 50)
)
mean(Y$Height[Y$Weight > 60])


# we can extract values from matrices using logical masks
M <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
MLogical = matrix(c(TRUE, FALSE), nrow = 4, ncol = 3)
M
MLogical
M[MLogical]

N <- matrix(1:3, nrow = 3, ncol = 3)
N
which(N == 1) # N is processed as its concatenated columns
which(N == 1, arr.ind = TRUE) # N is processed as a matrix



