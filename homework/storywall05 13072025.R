

# dbazig <- function(x, q0 = 0.6, p = 0.3, b = 30) {
#     if (x == 0) {
#         return(q0)
#     } else {
#         return((1 - q0) * p * (1 - p)^(x - 1) / (1 - (1 - p)^b))
#     }
# }


# upper bound at b, inclusive
# dbazig <- function(x, q0 = 0.6, p = 0.3, b = 30) {
#     ifelse(x == 0, q0, (1 - q0) * p * (1 - p)^(x - 1) / (1 - (1 - p)^b))
# }


dbazig <- function(x, q0 = 0.6, p = 0.3, b = 30) {
    ifelse(x < 0 | x > b, 0, 
    ifelse(x == 0, q0, (1 - q0) * (p) * (1 - p)^(x - 1) / (1 - (1 - p)^b))
    )
}


# b <- 30
# sum <- 0
# for (i in (0 - 1):(b + 1)) {
#     print(dbazig(i))
#     sum <- sum + dbazig(i)
# }
# sum

# dbazig(-1:31)
# sum(dbazig(-1:31))



dbazig <- function(x, q0 = 0.6, p = 0.3, b = 30) {
    ifelse(x < 0 | x > b, 0, 
    ifelse(x == 0, q0, (1 - q0) * (p) * (1 - p)^(x - 1) / (1 - (1 - p)^b))
    )
}

q0 <- 0.6
p <- 0.3
b <- 30


x <- 0:b
expectation <- sum(x * dbazig(x))
variance <- sum(x^2 * dbazig(x)) - expectation^2
stdev <- sqrt(variance)
sprintf("Expecation: %f, Variance: %f, StDev: %f", expectation, variance, stdev)

alpha <- 0.5
premium <- expectation + alpha * stdev
sprintf("Premium: %f for loading: %f", premium, alpha)


alpha = 0.2
premium <- 1 / alpha * log(sum(exp(alpha * x) * dbazig(x)))
sprintf("Premium: %f", premium)


exp.utility <- function(x, alpha = 0.2) {
    return((1 - exp(-alpha * x)) / alpha)
}
# exp.utility(1)
# exp.utility(1:4)


w0 <- 50
roots <- uniroot(function(p) sum(exp.utility(w0 + p - x) * dbazig(x)) - exp.utility(w0), c(0, 30))
sprintf("Premium: %f", roots$root)

