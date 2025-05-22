x <- 2
y <- 3

if (x <= y) {
    print("x <= y")
} else {
    print("x > y")
}


v <- c(1, 3, 7, 2)
for (i in 1:4) {
    print(v[i])
}

for (i in 2:4) {
    v[i - 1] <- v[i]
}
print(v)


x <- 2
y <- 1
while (x + y < 6) {
    x <- x + y
    print(x + y)
}
print(x)

# r uses break and next (continue)


# vectorisation (doing operations on vectors) is generally faster than using loops

x <- runif(5000000) # generate five million random elements
s1 <- 0;
system.time(for(i in 1:5000000) { s1 <- s1 + x[i] }) # see how long it takes to sum these elements with a loop
system.time(s2 <- sum(x))
print(s1)
print(s2)


print("")

factorial(6)
date()
log(100, base = 10)


hello <- function(name) {
    cat("Hello, my dear", toupper(name), "!")
}

hello(name = "Joseph")
hello("Joseph")
# in r, it is possible to specify a kwarg without typing out the full name of the keyword

hello(na = "Perry")
hello(n = "Dom")


print("")

poisscdf <- function(x, lambda) {
    cdf = 0
    for (k in 0:x) {
        cdf <- cdf + exp(-lambda) * lambda ^ k / factorial(k)
    }
    return(cdf)
}

poisscdf(3, 4)

# if there is no return statement, the function will return the result of the last evaluated expression
binomial <- function(n = 5, p = 3) {
    factorial(n) / (factorial(p) * factorial(n - p))
}

binomial()
binomial(6)
binomial(8, 6)

# %% is the modulo operator
4 %% 3
3 %% 4
4 %% 2

# paste is vaguely like join in python
# it concatenates strings with a separator, which is by default a space
paste("hello", "world")
paste("hello", 1234, "world", sep = " - ")
paste(c("alice", "bob"), "scored", c(84, 23), "points.")
paste(1:5, collapse = "-")


# there are multiple ways to see relevant documentation in r (? does the same thing as help())
# ?paste
# help(paste)
# example(paste)


balance <- 500
for (i in 1:10) {
    balance <- balance * (1 + i / 100)
}
print(balance)

state <- rpois(1, 2)
values <- c(state)
while (state < 5) {
    state <- rpois(1, 2)
    values <- c(values, state)
}
print(values)

# yes, in r, there are truthy falsies like c, though it is risky to do comparisons like this
annuity_value <- function(years = 1, arrears = TRUE, rate = 0.06) {
    if (arrears) {
        return ((1 - (1 + rate) ^ -years) / rate)
    } else {
        return ((1 + rate) * (1 - (1 + rate) ^ -years) / rate)
    }
}

annuity_value()
annuity_value(arrears = FALSE)
annuity_value(years = 5, arrears = FALSE, rate = 0.07)

plot_norm <- function(mean = 0, variance = 1, plot_pdf = TRUE) {
    x <- seq(mean - 4 * sqrt(variance), mean + 4 * sqrt(variance), by = sqrt(variance) / 50)
    if (plot_pdf) {
        plot(x, dnorm(x, mean, sqrt(variance)))
    } else {
        plot(x, pnorm(x, mean, sqrt(variance)))
    }
}
plot_norm()
plot_norm(1, 4, TRUE)
plot_norm(1, 4, FALSE)


