
airbnbSydneyData <- read.csv(file = "./data/AirbnbSydney.csv")

prices <- airbnbSydneyData$price

mu <- mean(prices)
median <- median(prices)
sd <- sd(prices)


# ((mu + 2 * sd) - mu) / sd


sum(mu + 2 * sd < prices) / length(prices)


normal.table <- function(upper = 3, increments = 0.01, decimals = 4) {
    if (round(0.1 / increments) != 0.1 / increments || increments < 0) {
        return("oh no an invalid increment!")
    }
    # print(seq(from = 0, to = upper, by = 0.1))
    # print(seq(from = 0, to = 0.1 - increments, by = increments))
    # print(seq(from = 0, to = upper + 0.1 - increments, by = increments))

    normaltableMatrix = matrix(round(pnorm(seq(from = 0, to = upper + 0.1 - increments, by = increments)), decimals), ncol = 0.1 / increments, byrow = TRUE)
    colnames(normaltableMatrix) <- seq(from = 0, to = 0.1 - increments, by = increments)
    rownames(normaltableMatrix) <- seq(from = 0, to = upper, by = 0.1)
    return(normaltableMatrix)
}

normal.table()
normal.table(upper = 2.5, increments = 0.02, decimals = 3)
# normal.table(upper = 2.5, increments = -0.02, decimals = 3)
# normal.table(upper = 2.5, increments = 0.03, decimals = 3)
# normal.table(upper = 2.5, increments = 0.05, decimals = 3)
# normal.table(upper = 9, increments = 0.1, decimals = 10)
# normal.table(upper = 0.0, increments = 0.1, decimals = 10)

