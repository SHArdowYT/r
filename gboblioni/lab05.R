
# install.packages("datasauRus")
# remove.packages("datasauRus")

library(ggplot2)
library(datasauRus)


ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
    geom_point() +
    coord_fixed(ratio = 0.6) +
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol = 3)

# here are the plots of the famous datasaurus dozen, 
# all of which follow the statistics

# | Property    | Value       |
# | :---------- | :---------- |
# | Count       | 142         |
# | X Mean      | 54.26       |
# | X Variance  | 16.76       |
# | Y Mean      | 47.83       |
# | Y Variance  | 26.93       |
# | Correlation | -0.06       |
# | LOBF        | y = 53 - 0.1x |
# | R^2         | 0.004       |

# this is used to illustrate that statistics do not paint a complete picture


# humans spend a lot of time communicating
# activating different parts of the brain in audience members makes your point more memorable and impactful
# data also means nothing without context -- it requires a story around it.
# statistics are rarely enough to tell a story
# analyse, confirm, inform, persuade

# a good story requires:
# an understanding of the audience -- what they want to hear (e.g., formulas, outputs, a description of the data), which will be dependent on their background (e.g., a junior actuary, senior director, old person)
# there are many different ways to build a story, but it is usually good to build your arguments from the bottom up (i.e., starting with a general motivation.context and then moving on to specific findings/conclusions)

# usually this can be done in several steps:
# identify your target audience, and understand that they may not be as good at maths as you (and as such you should avoid technicalities)
# identify your one point (what you want to say), but do not analyse or explore yet (which you will need explanatory tools for)
# identify the data/visualisation tool(s) you will use to make your point
# make your point (e.g., australia has a positive net migration)
# use a relevant visualisation (e.g., a map)
# add an animation (if you want to be fancy)
# make sure your audience is convinced about your point

# when choosing a visualisation, first understand what you are trying to present -- the variables themselves, relationships, trends, etc.
# pick an appropriate tool to present your data, e.g., a scatter plot, bar graph, histogram, box plot
# choose an appropriate scale to make your figure clear
# emphasises the key features and avoid unncessary details
# refine your presentation (i.e., name the axes, use colours, adjust the size of points and lines, add a legend)



# to create a scatterplot, we can use plot()
plot(longley$Year, longley$Armed.Forces)

# a more appropriate graph would be a line graph, so we should instead use plot(type = "l")
plot(longley$Year, longley$Armed.Forces, type = "l")

# we can also add more things, namely, a title, subtitle and x and y labels
plot(longley$Year, longley$Armed.Forces, type = "l",
     main = "armed forces (1947--1962)", sub="i am a subtitle",
     xlab = "year", ylab = "number")

# we append data to our graph by using points()
points(longley$Year, longley$Unemployed, type = "l")



# oh dear. our y-axis has not rescaled itself.
# we shall fix that with ylim (much like matlab)
plot(longley$Year, longley$Armed.Forces, type = "l",
     main = "armed forces (1947--1962)", sub="i am a subtitle",
     xlab = "year", ylab = "number",
     ylim = c(0, 600))

points(longley$Year, longley$Unemployed, type = "l")


# it is still a bit difficult to distinguish which line corresponds to which set of data
# we can remedy this by adjusting the line width and colour
points(longley$Year, longley$Unemployed, type = "l",
       lwd = 3, col = "red")

# we can see all the colours that r supports with colours()
head(colors(), 20)




# our graph is looking reasonably nicer, but we still cannot distinguish which line is which
# we can use a legend to show this
plot(longley$Year, longley$Armed.Forces, type = "l",
     lwd = 3, col = "red3",
     main = "armed forces (1947--1962)", sub="i am a subtitle",
     xlab = "year", ylab = "number",
     ylim = c(0, 600))

points(longley$Year, longley$Unemployed, type = "l",
       lwd = 3, col = "skyblue3")

legend("bottomright", legend = c("Armed Forces", "Unemployed"), col = c("red3", "skyblue3"), lwd = c(3, 3), lty = c(1, 1))
# note that lty here is the linetype



school_data <- read.csv("./data/dv279-schoollocations2019.csv", header = TRUE)
attach(school_data)

# we create histograms with hist()
hist(Postal_Postcode, breaks = 50, xlim = c(3000, 4000))

# if we want to normalise this data, set proba = TRUE
# we can also alter the number of intervals (bins) with breaks
hist(Postal_Postcode, proba = TRUE, breaks = 100, col ="lightblue", xlim = c(3000, 4000))


# a boxplot (vertical box and whisker plot) shows the quartiles of data, and the smallest and largest observations, provided that these observations are not outliers (as given by Q1 - 1.5 * IQR and Q3 + 1.5 * IQR)
boxplot(decrease ~ treatment, data = OrchardSprays, col = "coral")
# the tilde here is formula notation, which regresses y on x (y ~ x). usually, x is categorical / a grouping



# we all know what a pie chart is
my_colours = c("lightblue", "lightgreen", "brown3", "purple1", "lightgoldenrod1", "pink", "azure4", "orange")
pie(table(LGA_Name), col = my_colours, cex = 1.25, main = "schools by lga")
# remember, table() generates a frequency table
# cex means character expansion, essentially font scaling


# weve all seen a bar plot as well
barplot(table(LGA_Name), col = my_colours, xlab = "lga", ylab = "number of schools")
barplot(prop.table(table(LGA_Name)), col = my_colours, xlab = "lga", ylab = "percentage of schools")
# prop.table means to take the tabls as a proportion of values (probabilities)


table(Education_Sector, LGA_Name)
sector_by_lga_col <- prop.table(table(Education_Sector, LGA_Name), margin = 2)
sector_by_lga_row <- prop.table(table(Education_Sector, LGA_Name), margin = 1)
# margin determines which axis sums to 1, the rows (1) or columns (2). if not specified, everything will sum to 1.

barplot(sector_by_lga_col, beside = TRUE, legend = TRUE, col = my_colours[1:3], ylim = c(0, 1.25))
barplot(sector_by_lga_row, beside = TRUE, legend = TRUE, col = my_colours[1:3])
# note that this distinction means these two graphs communicate different ideas!
# we have also adjusted ylim so that the data does not get obscured by the legend


# of course, if we input the data backwards, the axes will be swapped
lga_by_sector_col <- prop.table(table(LGA_Name, Education_Sector), margin = 2)
lga_by_sector_row <- prop.table(table(LGA_Name, Education_Sector), margin = 1)
# margin determines which axis sums to 1, the rows (1) or columns (2). if not specified, everything will sum to 1.

barplot(lga_by_sector_col, beside = TRUE, legend = TRUE, col = my_colours[1:8], ylim = c(0, 1.25))
barplot(lga_by_sector_row, beside = TRUE, legend = TRUE, col = my_colours[1:8])


# if we do not specify beside, we will get a (100%) stacked bar chart
barplot(sector_by_lga_col, beside = FALSE, legend = TRUE, col = my_colours[1:3])



# we can plot a function with curve(). usually we specify from and to.
curve(dnorm(x, 0, 1), from = -4, to = 4)
curve(x * sin(1 / x), from = -0.1, to = 0.1)

# oh dear. our temporal resolution is quote bad.
# as per our expectations, we have also produced nans
# anyhow, we can improve the termporal resolution by taking more samples (and we shall also arbitrarily double the axis label font scale)
curve(x * sin(1 / x), from = -0.1, to = 0.1, col = "firebrick", n = 1000, cex.lab = 2)


# of course, like matlab, we can show multiple plots in one plot
par(mfcol = c(3, 2))
# graphical parameters multifigure by column, 3 by 2 (we can also choose to use mfrow)

plot(1:3, 1:3, col = "firebrick", type = "l", lwd = 2, xlab = "", ylab = "")
plot(1:5, 1:5, col = "red", type = "b", lwd = 2, xlab = "", ylab = "")
plot(1:7, 1:7, col = "orangered", type = "p", lwd = 2, xlab = "", ylab = "")
plot(ts(rnorm(100)), col = "skyblue", lwd = 2, xlab = "", ylab = "") # timeseries
curve(sqrt(x^2 - x^4), col = "slateblue", from = -1, to = 1, lwd = 2, xlab = "", ylab = "")
hist(rexp(100, 1 / 2), main = "", col = "purple", xlab = "", ylab = "")



# ggplot is a powerful tool to create visualisations
library(ggplot2)
# library(ggExtra)

ggplot(mpg, aes(cty, hwy)) # 'aesthetic' mapping, with positional arguments for x and y
ggplot(mpg, aes(cty, hwy)) + geom_count() # add the scatter plot based layer, with the size of the bubbles depending on the counts
ggplot(mpg, aes(cty, hwy)) + geom_count() + geom_smooth(method = "lm", se = FALSE) # linear regression without standard error shading
ggplot(mpg, aes(cty, hwy)) + geom_count() + geom_smooth(method = "lm", se = TRUE)
# ggMarginal(g, type = "histogram", fill = "transparent")



# hopefully it is clear that one of these graphs is more appropriate
team_members <- 1:5
sales <- 10000 * c(10, 8, 6, 2, 12)
plot(team_members, sales, type = "b", lwd = 5, col = "deeppink", xlab = "team member")
barplot(sales, names.arg = team_members, col = "deeppink", xlab = "team member")


# here, the axes make this graph very difficult to read. we can, however, remedy this.
library(MASS)
plot(mammals$body, mammals$brain)
plot(log(mammals$body), log(mammals$brain), xlab = "ln of body mass", ylab = "ln of brain mass", main = "brain mass against body mass of mammals on a ln-ln scale")
abline(lm(log(mammals$brain) ~ log(mammals$body)), col = "mediumorchid", lwd = 2) # add a y = ax + b line, using a linearised linear model


# we can also get help on datasets in the same way (by prepending ?)
head(Seatbelts)
# This is a multiple time series (which is a data structure in itself)
plot(Seatbelts[, "DriversKilled"], type = "l", lwd = 2, col = "darkgoldenrod", main = "Drivers killed every month from 1969--1984", xlab = "Month", ylab = "Drivers Killed")
abline(v = 1983, lwd = 2, col = "red")

plot(Seatbelts[, "front"], type = "l", lwd = 2, col = "firebrick", main = "Occupants killed every month from 1969--1984 by position", xlab = "Month", ylab = "Occupants Killed", ylim = c(0, 1200))
points(Seatbelts[, "rear"], type = "l", lwd = 2, col = "dodgerblue")
abline(v = 1983, lwd = 2, col = "olivedrab")
legend("topright", legend = c("front", "rear"), col = c("firebrick", "dodgerblue"), lwd = c(2, 2))

boxplot(Seatbelts[, "DriversKilled"] ~ Seatbelts[, "law"], col = "coral", main = "Boxplot of drivers killed every month, by the requirement of seatbelts" , xlab = "Month", ylab = "Drivers Killed")



suppressMessages(library(dplyr))

# because seatbelts is a timeseries, it needs to be converted to a dataframe first (as in pandas)
as.data.frame(Seatbelts) %>%
    group_by(law) %>%
    summarise(mean = mean(DriversKilled), sd = sd(DriversKilled), count = n()) %>% # n() counts the number of rows, only in some dplyr functions
    mutate(ci_low = mean - 1.96 * sd / sqrt(count), ci_high = mean + 1.96 * sd / sqrt(count)) # calculate 95% confidence intervals because why not







