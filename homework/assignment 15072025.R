
# ======== SETUP ========

# Install required packages if they do not exist.
# if (!requireNamespace("corrplot")) {
#     install.packages("corrplot")
# }

# Include libraries.
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

options(digits = 8)
options(pillar.sigfig = 7)

# Specify paths to data files.
airbnb.data_path <- "./data/AirbnbSydney.csv"
parameters_by_student_path <- "./data/parameters_by_student.csv"

airbnb.data <- read.csv(airbnb.data_path)

export_graphs_flag <- FALSE

# Define a function to display, and optionally save, plots.
# Takes the parameters:
# - plot_function: A function that generates the plot to be displayed.
# - save: Whether the plot should be saved.
# - filename: The path to save the plot to (if save is specified).
# - width: The width of the svg to which the plot is saved, in inches.
# - height: The height of the svg to which the plot is saved, in inches.
# Displays and saves plots as a side effect.
display_and_save_plot <- function(plot_function, save = export_graphs_flag, filename = NULL, width = 8, height = 6, ...) {
    # Generate and display the plot.
    print(plot_function(...))

    # Generate and save the plot if desired.
    if (save) {
        if (is.null(filename)) {
            stop("Filename not provided, even though save was specified.")
        }
        svg(filename = filename, width = width, height = height)
        print(plot_function(...))
        dev.off()
    }
}




# ======== QUESTION 1.1 ========

# Define price.per.guest as price/accommodates.
price.per.guest <- airbnb.data$price / airbnb.data$accommodates

# Display statistics and information for price.per.guest.
sprintf("Length: %f", length(price.per.guest))
sprintf("Type: %s", typeof(price.per.guest))
sprintf("Minimum: %f", min(price.per.guest))
sprintf("Maximum: %f", max(price.per.guest))
sprintf("Mean: %f", mean(price.per.guest))
sprintf("Median: %f", median(price.per.guest))
sprintf("Standard Deviation: %f", sd(price.per.guest))



# ======== QUESTION 1.2 ========

# Define a function to plot the price per guest night against longitude.
# Takes the parameter max_rows, the maximum allowable number of rows to plot.
# Generates a plot as a side effect.
# Note: this runs very slowly if alpha is not 1.
plot_price.per.guest_longitude <- function(max_rows = 1000) {
    length = min(max_rows, length(price.per.guest))
    ggplot(airbnb.data[1:length, ], aes(x = longitude, y = price.per.guest[1:length])) + 
        geom_point(colour = "aquamarine3", shape = 4, size = 0.5) + 
        # geom_point(colour = "aquamarine3", alpha = 0.5, shape = 4, size = 0.5) + 
        geom_smooth(method = "loess", se = TRUE, colour = "red2", linewidth = 1.5) +
        scale_x_continuous(breaks = seq(150.6, 151.4, by = 0.1)) + 
        labs(title = "Scatterplot of the Price per Guest Night against Longitude", x = "Longitude (\u00B0)", y = expression("Price per Guest Night ($ day"^-1 ~ ")")) +
        theme_minimal() + 
        theme(panel.grid.minor = NULL)
}

# Display the plot of price per guest night against longitude.
# Save this to an svg, depending on export_graphs_flag.
display_and_save_plot(
    plot_price.per.guest_longitude,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 2 26072025.svg",
    width = 8,
    height = 7,
    max_rows = length(price.per.guest)
)

# Find the easternmost listing.
max(airbnb.data$longitude)



# ======== QUESTION 1.3 ========

# Calculate estimates of alpha and beta (shape and rate) in the gamma distribution.
# Then display them.
beta_estimate <- (mean(price.per.guest * log(price.per.guest)) - mean(price.per.guest) * mean(log(price.per.guest)))^(-1)
alpha_estimate <- mean(price.per.guest) * beta_estimate
cat(sprintf("betastar: %f, \t alphastar: %f", beta_estimate, alpha_estimate))

# Define a function to plot a histogram of the price per guest night, 
# with a gamma curve overlaid on top.
# Generates a plot as a side effect.
plot_price.per.guest_hist <- function() {
    hist(price.per.guest, breaks = 50, probability = TRUE, col ="lightblue", main = "Normalised Histogram of Price per Guest with Gamma Curve", xlab = expression("Price per Guest Night ($ day"^-1 ~ ")"))
    grid(lty = "solid")
    curve(dgamma(x, alpha_estimate, beta_estimate), n = 1000, add = TRUE, lwd = 4, col = "orangered")
    legend("topright", legend = c("Prices", "Gamma Curve"), col = c("lightblue", "orangered"), fill = c("lightblue", NA), border = c("black", NA), lwd = c(NA, 3), lty = c(1, 1))
}

# Display the histogram with the gamma curve.
# Save this to an svg, depending on export_graphs_flag.
display_and_save_plot(
    plot_price.per.guest_hist,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 3 26072025.svg",
    width = 8,
    height = 5
)



# ======== QUESTION 1.4 ========

# Define the boundaries for the quantiles we want to set.
# Then calculate the values (endpoints) for each quantile.
quantile_bounds <- c(0.15, 0.40, 0.60, 0.85)
quantile_endpoints <- quantile(airbnb.data$reviews, quantile_bounds, names = FALSE)

# Create a new column, Review.Bracket, to the dataframe, 
# depending on whether a listing meets the thresholds for each quantile.
airbnb.data <- airbnb.data %>%
    mutate(Review.Bracket = if_else(reviews_per_month < quantile_endpoints[1], "Low",
                            if_else(reviews_per_month < quantile_endpoints[2], "Medium-Low",
                            if_else(reviews_per_month < quantile_endpoints[3], "Medium",
                            if_else(reviews_per_month < quantile_endpoints[4], "Medium-High", "High")))))

# Convert that column into a factor.
airbnb.data$Review.Bracket <- factor(airbnb.data$Review.Bracket, levels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"), ordered = TRUE)
# Add price.per.guest into the dataframe as a column.
airbnb.data$price.per.guest <- price.per.guest
# Group the data by Review.Bracket and then calculate, and display,
# the mean and median price.per.guest for each bracket.
airbnb.data_review.bracket_summary <- airbnb.data %>%
    group_by(Review.Bracket) %>%
    summarise(
        mean_price.per.guest = mean(price.per.guest),
        median_price.per.guest = median(price.per.guest)
    )
airbnb.data_review.bracket_summary

# Plot the mean and median price.per.guest for each review bracket as various bar charts.
# Takes the parameter mode, which determines what type of plot to generate.
# Generates a plot as a side effect.
plot_review.bracket_price.per.guest <- function(mode = "mean") {

    if (mode == "mean") {
        ggplot(airbnb.data_review.bracket_summary, aes(x = Review.Bracket, y = mean_price.per.guest)) +
            geom_bar(stat = "identity", fill = "turquoise", width = 0.8) +
            scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
            labs(title = "Mean Price per Guest by Review Frequency Brackets", x = "Review Bracket", y = "Price per Guest ($ day^-1)") +
            theme_minimal() +
            coord_flip() +
            theme(panel.grid.minor = element_blank(), aspect.ratio = 0.3333)
    } else if (mode == "median") {
        ggplot(airbnb.data_review.bracket_summary, aes(x = Review.Bracket, y = median_price.per.guest)) +
            geom_bar(stat = "identity", fill = "maroon", width = 0.8) +
            scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
            labs(title = "Median Price per Guest by Review Frequency Brackets", x = "Review Bracket", y = "Price per Guest ($ day^-1)") +
            theme_minimal() +
            coord_flip() +
            theme(panel.grid.minor = element_blank(), aspect.ratio = 0.3333)
    } else if (mode == "grouped") {
        combined_data <- rbind(
            data.frame(
                Review.Bracket = airbnb.data_review.bracket_summary$Review.Bracket,
                price = airbnb.data_review.bracket_summary$mean_price.per.guest,
                statistic = "Mean"
            ),
            data.frame(
                Review.Bracket = airbnb.data_review.bracket_summary$Review.Bracket,
                price = airbnb.data_review.bracket_summary$median_price.per.guest,
                statistic = "Median"
            )
        )

        ggplot(combined_data, aes(x = Review.Bracket, y = price, fill = statistic)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
            labs(title = "Mean and Median Price per Guest by\nReview Frequency Brackets", x = "Review Frequency Bracket", y = expression("Price per Guest ($ day"^-1 ~ ")"), fill = "Statistic") +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(), aspect.ratio = 1.5, axis.text.x = element_text(angle = 45, hjust = 1))
    }

}

# Display and save the plots for the mean, median and grouped graphs.
display_and_save_plot(
    plot_review.bracket_price.per.guest,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 4 mean 26072025.svg",
    width = 8,
    height = 3,
    mode = "mean"
)
display_and_save_plot(
    plot_review.bracket_price.per.guest,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 4 median 26072025.svg",
    width = 8,
    height = 3,
    mode = "median"
)
display_and_save_plot(
    plot_review.bracket_price.per.guest,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 4 grouped 26072025.svg",
    width = 4,
    height = 5,
    mode = "grouped"
)




# ======== QUESTION 1.5 ========

airbnb.data_property_types <- airbnb.data %>%
    count(property_type) %>%
    arrange(desc(n))

# Create a dataframe with numeric data from airbnb.data, 
# and append four columns, converted to numeric variables.
airbnb.data_numeric <- airbnb.data %>%
    select(where(is.numeric)) %>%
    mutate(
        host_response_time = as.numeric(factor(airbnb.data$host_response_time, levels = c("within an hour", "within a few hours", "within a day", "a few days or more", "N/A"), ordered = TRUE)),
        market_time = as.numeric(max(as.Date(airbnb.data$last_review, format = "%d/%m/%Y")) - as.Date(airbnb.data$first_review, format = "%d/%m/%Y")),
        host_response_rate = as.numeric(sub("%", "", airbnb.data$host_response_rate)) / 100,
        host_acceptance_rate = as.numeric(gsub("%", "", airbnb.data$host_acceptance_rate)) / 100,
        property_type = as.numeric(factor(airbnb.data$property_type, levels = airbnb.data_property_types$property_type[1:10], ordered = TRUE))
    )
# Replace NAs with 0 to prevent errors when producing the correlation matrix.
airbnb.data_numeric$host_response_rate <- ifelse(is.na(airbnb.data_numeric$host_response_rate), 0, airbnb.data_numeric$host_response_rate)
airbnb.data_numeric$host_acceptance_rate <- ifelse(is.na(airbnb.data_numeric$host_acceptance_rate), 0, airbnb.data_numeric$host_acceptance_rate)
airbnb.data_numeric$property_type <- ifelse(is.na(airbnb.data_numeric$property_type), 0, airbnb.data_numeric$property_type)

# Calculate the correlation matrix for airbnb.data_numeric.
airbnb.data_correlation_matrix <- cor(airbnb.data_numeric)
# airbnb.data_correlation_matrix <- cor(airbnb.data_numeric, method = "spearman")


# Define a function to plot a correlogram of numeric airbnb data.
# Generates a plot as a side effect.
plot_airbnb.data_correlogram <- function() {
    colour_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
    # Create a dummy plot so the result has the correct margins.
    corrplot(airbnb.data_correlation_matrix,
             method = "number",
             type = "upper",
             addgrid.col = NA,
             number.cex = 0.01,
             tl.col = "white",
             tl.cex = 0.7,
             cl.pos = "n"
            )

    # Show the numbers in the lower triangle.
    corrplot(airbnb.data_correlation_matrix,
             method = "number",
             type = "lower",
             col = "black",
             addgrid.col = NA,
             number.cex = 0.4,
             tl.col = "black",
             tl.pos = "l",
             tl.cex = 0.7,
             diag = FALSE,
             add = TRUE,
            )
    # Show the ellipses in the upper triangle.
    corrplot(airbnb.data_correlation_matrix,
             method = "ellipse",
             type = "upper",
             col = colour_palette,
             outline = FALSE,
             addgrid.col = NA,
             tl.col = "black",
             tl.pos = "t",
             tl.cex = 0.7,
             cl.pos = "n",
             add = TRUE,
            )
}

# Display and export the correlogram for the numeric airbnb data.
display_and_save_plot(
    plot_airbnb.data_correlogram,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 5 alternate 26072025.svg",
    width = 8,
    height = 8
)


# Plot the availability over 365 days against the availability over 90 days, 
# coloured by review scores rating.
# Generates a plot as a side effect.
plot_airbnb.availability_365_availability_90 <- function() {
    colour_palette <- colorRampPalette(brewer.pal(8, "Spectral"))(100)
    ggplot(airbnb.data, aes(x = availability_90, y = availability_365, colour = review_scores_rating)) +
        geom_point(alpha = 0.3, size = 1.125, shape = 16) +
        labs(title = "Scatterplot of Availability over 365 days against 90 days",
                x = "Availability over the next 90 days (days)", 
                y = "Availability over the next 365 days (days)", 
                colour = "Rating score") +
        scale_x_continuous(breaks = seq(0, 90, by = 30)) +
        scale_y_continuous(breaks = seq(0, 360, by = 90)) +
        scale_colour_gradientn(colours = colour_palette, limits = c(4.5, 5), guide = guide_colourbar(barwidth = unit(0.4, "npc"), position = "bottom")) +
        theme_minimal()
}

# Display and save the plot of availability over 365 days against availability over 90 days.
display_and_save_plot(
    plot_airbnb.availability_365_availability_90,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 1 5 availability 03082025.svg",
    width = 6,
    height = 8
)





# ======== QUESTION 2.1 ========

# Load the parameters for all students.
parameters_by_student <- read.csv(file = parameters_by_student_path)

# Filter the parameters for a particular student and store them.
parameters_self <- filter(parameters_by_student, zID == "z5692836")
param.p <- parameters_self$p
param.b <- parameters_self$b
param.a <- parameters_self$a

# Define the PDF of the BAZIG distribution.
# Takes the parameters:
# - x: The amount of damage.
# - q0: The probability of zero loss.
# - p: A parameter for BAZIG.
# - b: The maximum possible damage.
dbazig <- function(x, q0, p = param.p, b = param.b) {
    ifelse(x < 0 | x > b, 0, 
    ifelse(x == 0, q0, (1 - q0) * (p) * (1 - p)^(x - 1) / (1 - (1 - p)^b))
    )
}

# Store all possible values of damages that can be taken in a vector x.
x <- 0:param.b

# Calculate and display the kurtosis of BAZIG for q0 = 0.2 and 0.8.
for (param.q in c(0.2, 0.8)) {
    expectation <- sum(x * dbazig(x, param.q))
    variance <- sum(x^2 * dbazig(x, param.q)) - expectation^2
    kurtosis <- sum((x - expectation)^4 * dbazig(x, param.q)) / variance^2
    cat(sprintf("Kurtosis for q0 = %f: %f\n", param.q, kurtosis))
}


# ======== QUESTION 2.2 ========

# Calculate the loading factor c under the PZU for q0 = 0.2 and 0.8.
for (param.q in c(0.2, 0.8)) {
    premium <- 1 / param.a * log(sum(exp(param.a * x) * dbazig(x, param.q)))
    expectation <- sum(x * dbazig(x, param.q))
    loading <- premium / expectation
    cat(sprintf("For q0 = %f, \t Premium: %.8f, \t Loading factor c: %.8f\n", param.q, premium, loading))
}


# ======== QUESTION 2.3 ========

# Define a range of possible values for q0.
q_input <- seq(0.01, 0.99, by = 0.01)

# Define functions to calculate the kurtosis and loading factor for a given input q0.
calculate_kurtosis <- function(param.q) {
    expectation <- sum(x * dbazig(x, param.q))
    variance <- sum(x^2 * dbazig(x, param.q)) - expectation^2
    kurtosis <- sum((x - expectation)^4 * dbazig(x, param.q)) / variance^2
}
calculate_loading <- function(param.q) {
    premium <- 1 / param.a * log(sum(exp(param.a * x) * dbazig(x, param.q)))
    expectation <- sum(x * dbazig(x, param.q))
    loading <- premium / expectation
}

# Create a data frame with q_inputs and the resulting kurtoses and loading factors.
q_kurtosis_loading <- data.frame(q_input = q_input) %>%
    mutate(kurtosis = sapply(q_input, calculate_kurtosis), 
           loading = sapply(q_input, calculate_loading))

plot_kurtosis <- function() {
    ggplot(q_kurtosis_loading, aes(x = q_input, y = kurtosis)) +
        geom_line(colour = "darkcyan", linewidth = 1) +
        scale_x_continuous(breaks = seq(0.00, 1.00, by = 0.20)) +
        labs(title = expression("Kurtosis of BAZIG for different q"[0]), x = expression("q"[0]), y = expression("Kurtosis" ~ kappa(X))) +
        theme_minimal()
}
plot_loading <- function() {
    ggplot(q_kurtosis_loading, aes(x = q_input, y = loading)) +
        geom_line(colour = "darkorchid3", linewidth = 1) +
        scale_x_continuous(breaks = seq(0.00, 1.00, by = 0.20)) +
        labs(title = expression("Loading Factor of Premium for different q"[0]), x = expression("q"[0]), y = "Loading Factor") +
        theme_minimal()
}

# Display and save the plots for the kurtoses and loading factors for different q0.
display_and_save_plot(
    plot_kurtosis,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 2 3 kurtosis 26072025.svg",
    width = 5,
    height = 3.5
)
display_and_save_plot(
    plot_loading,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 2 3 loading 26072025.svg",
    width = 5,
    height = 3.5
)


# ======== QUESTION 2.4 ========

# Reinitialise the values that BAZIG can take, 
# and the BAZIG function itself for clarity / self-containment.
x <- 0:param.b
dbazig <- function(x, q0, p = param.p, b = param.b) {
    ifelse(x < 0 | x > b, 0, 
    ifelse(x == 0, q0, (1 - q0) * (p) * (1 - p)^(x - 1) / (1 - (1 - p)^b))
    )
}

# Create several lambda functions to aid in computing the maximum loading factor, 
# with input parameters matching that of their mathematical definitions, i.e.:
# - w0: The initial wealth.
# - q0: The probability of zero loss.
# - d: The deductible.
# - P: The premium.
# - z: An input to the utility function (which must be named differently to the already-
# defined x).
compute_expectation_y <- function(q0, d) { sum(pmax(0, x - d) * dbazig(x, q0)) }
utility_function <- function(z) { log(z) }
utility_without_insurance <- function(w0, q0, d) { sum(utility_function(w0 - x) * dbazig(x, q0)) }
utility_with_insurance <- function(w0, q0, d, P) { sum(utility_function(w0 - P - pmin(x, d)) * dbazig(x, q0)) }
utility_difference <- function(P, w0, q0, d) { utility_with_insurance(w0, q0, d, P) - utility_without_insurance(w0, q0, d) }
 
# Define the values of q0 and d to be investigated.
q_inputs <- c(0.2, 0.4, 0.6, 0.8)
d_inputs <- c(0, 1, 2, 3, 5, 10)

# Create a dataframe to store the results for debugging/inspection,
# and a matrix for presentation.
loadings_dataframe <- data.frame()
loadings_matrix <- matrix(nrow = length(q_inputs), ncol = length(d_inputs))
rownames(loadings_matrix) <- q_inputs
colnames(loadings_matrix) <- d_inputs

# Use a nested loop to iterate through all possible combinations of q0 and d,
# and then calculate the maximum acceptable loading.
# Add this to both the dataframe and the matrix.
for (param.q in q_inputs) {
    for (d in d_inputs) {
        expectation_y <- compute_expectation_y(param.q, d)
        premium <- uniroot(utility_difference, interval = c(0, param.b), w0 = 10 + param.b, q0 = param.q, d = d, tol = 1e-12)
        maximum_loading <- premium$root / expectation_y

        loadings_dataframe <- rbind(loadings_dataframe, data.frame(q0 = param.q, d = d, premium = premium$root, loading = maximum_loading, expectation_y = expectation_y, precision = premium$estim.prec))
        loadings_matrix[as.character(param.q), as.character(d)] <- maximum_loading
    }
}
# Display the dataframe and matrix of loadings.
loadings_dataframe
loadings_matrix


# Define a function to plot the maximum loading factor against the deductible, grouped by q0.
# Generates a plot as a side effect.
plot_loading_deductible <- function() {
    ggplot(loadings_dataframe, aes(x = d, y = loading, group = factor(q0), colour = factor(q0))) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = seq(0, 10, by = 1)) +
        labs(title = expression("Maximum loading factor " ~ theta^"*" ~ "for different " ~ q[0] ~ " and deductible " ~ d),
             x = expression("Deductible" ~ d ~ "(million $)"), 
             y = expression("Maximum loading factor " ~ theta^"*"), 
             colour = expression(q[0])
            ) +
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank(), legend.position = "right") +
        scale_colour_brewer(palette = "Greens", direction = -1)
}

# Display and save the plot of the maximum loading factor against the deductible grouped by q0.
display_and_save_plot(
    plot_loading_deductible,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 2 4 01082025.svg",
    width = 8,
    height = 5
)


# Plot a graph representing of the utility change for small and catastrophic losses.
plot_utility_change <- function() {
    w0 = 10 + param.b
    input_points <- c(10, 30, w0)
    graph_colours <- c("cornflowerblue", "chartreuse3", "firebrick3", "slategray", "gray")
    plot.new()
    legend("bottomright", c("u(x)", "small loss", "catastrophic loss"), col = c("dodgerblue", "lawngreen", "red"), lwd = 3)
    curve(utility_function, from = 1, to = 40, col = graph_colours[1], lwd = 3, main = "Utility function of Elom, with secants", xlab = "Wealth (million $)", ylab = "Utility")
    grid(lty = "solid", col = graph_colours[5])
    lines(c(input_points[3], input_points[2]), utility_function(c(input_points[3], input_points[2])), col = graph_colours[2], lwd = 3)
    lines(c(input_points[3], input_points[1]), utility_function(c(input_points[3], input_points[1])), col = graph_colours[3], lwd = 3)
    points(input_points, utility_function(input_points), col = graph_colours[4], pch = 19, cex = 1)
    legend("bottomright", c("u(x)", "small loss", "catastrophic loss"), col = graph_colours[1:3], lwd = 3)
}

# Display and save the plot of utility changes with secants for small and catastrophic losses.
display_and_save_plot(
    plot_utility_change,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 2 4 utility change 01082025.svg",
    width = 8,
    height = 5
)





















# ======== OLD PLOTS FOR QUESTION 1.5 ========

# if (!requireNamespace("ellipse")) {
#     install.packages("ellipse")
# }
library(ellipse)
plot_airbnb.data_correlogram_old <- function() {
    colour_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
    plotcorr(airbnb.data_correlation_matrix, col = colour_palette[(airbnb.data_correlation_matrix + 1) * 100 / 2], mar = c(0, 0, 0, 0))
}


# boxplot of minimum nights by host response time
ggplot(airbnb.data_numeric, aes(x = factor(host_response_time), y = minimum_nights)) +
    geom_boxplot() +
    scale_y_continuous(limits = c(0, 20))

# scatterplot of host response rate vs host acceptance rate
ggplot(airbnb.data_numeric, aes(x = host_response_rate, y = host_acceptance_rate)) +
    geom_point()

# boxplot of bedrooms vs price
ggplot(airbnb.data, aes(x = factor(bedrooms), y = price)) +
    geom_boxplot()

# boxplot of host acceptance rate by minimum nights
ggplot(airbnb.data_numeric %>% filter(minimum_nights >= 1 & minimum_nights <= 20), aes(x = factor(minimum_nights), y = host_acceptance_rate)) +
    geom_boxplot()

# stacked barplot of host response time for different minimum nights
ggplot(airbnb.data_numeric %>% filter(minimum_nights >= 1 & minimum_nights <= 20), 
    aes(x = factor(minimum_nights), fill = factor(host_response_time))) +
    geom_bar(position = "fill") +
    labs(fill = "Host Response Time")


# scatterplot of market time vs number of reviews
ggplot(airbnb.data_numeric, aes(x = market_time, y = number_of_reviews)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_x_continuous(limits = c(0, 250)) +
    scale_y_continuous(limits = c(0, 50))

# scatterplot of number of reviews vs market time
ggplot(airbnb.data_numeric, aes(x = number_of_reviews, y = market_time)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_x_continuous(limits = c(0, 25)) +
    scale_y_continuous(limits = c(0, 200))

# scatterplot of review scores accuracy vs review scores rating
ggplot(airbnb.data, aes(x = review_scores_accuracy, y = review_scores_rating)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE)

# scatterplot of review scores accuracy vs review scores rating
ggplot(airbnb.data, aes(x = review_scores_communication, y = review_scores_rating)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE)


# scatterplot of number of reviews vs market time
ggplot(airbnb.data_numeric, aes(x = number_of_reviews, y = market_time)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Scatterplot of Time on the Market vs Number of Reviews", x = "Number of Reviews", y = "Time on market (days)") +
    scale_x_continuous(limits = c(0, 25)) +
    scale_y_continuous(limits = c(0, 200)) +
    theme_minimal()

# histogram of market time with a curve of best fit
ggplot(airbnb.data_numeric, aes(y = market_time)) +
    geom_histogram(binwidth = 3, fill = "skyblue", colour = "black") +
    geom_density(aes(x = after_stat(count) * 3), colour = "red", size = 1) +
    labs(title = "Histogram of Market Time for Airbnb Listings", x = "Number of Properties", y = "Time on Market (days)") +
    scale_y_continuous(limits = c(0, 200)) +
    theme_minimal()



# ======== OLD PLOTS FOR QUESTION 2.3 ========

# Create a data frame with q_inputs and the resulting kurtoses and loading factors.
# The other columns were added retrospectively.
q_kurtosis_loading <- data.frame(q_input = q_input) %>%
    mutate(kurtosis = sapply(q_input, calculate_kurtosis), 
           loading = sapply(q_input, calculate_loading),
           expectation = sapply(q_input, function(param.q) { sum(x * dbazig(x, param.q)) }),
           variance = sapply(q_input, function(param.q) { sum(x^2 * dbazig(x, param.q)) - sum(x * dbazig(x, param.q))^2 }),
           expectation_expectation = sapply(q_input, function(param.q) { sum((x - sum(x * dbazig(x, param.q)))^4 * dbazig(x, param.q)) })
          )

plot_variance_expectation <- function() {

    ggplot(q_kurtosis_loading, aes(x = q_input)) +
        geom_line(aes(y = expectation), colour = "cadetblue", linewidth = 1) +
        geom_line(aes(y = variance), colour = "goldenrod", linewidth = 1) +
        labs(title = expression("Expectation and Variance for different q"[0]), x = expression("q"[0] ~ "input"), y = "Value") +
        scale_x_continuous(breaks = seq(0.00, 1.00, by = 0.20)) +
        theme_minimal()

    ggplot(q_kurtosis_loading, aes(x = q_input)) +
        geom_line(aes(y = expectation), colour = "cadetblue", linewidth = 1) +
        geom_line(aes(y = variance), colour = "goldenrod", linewidth = 1) +
        geom_line(aes(y = expectation_expectation), colour = "slateblue", linewidth = 1) +
        labs(title = expression("Expectation, Variance and Kurtosis for different q"[0]), x = expression("q"[0] ~ "input"), y = "Value") +
        scale_x_continuous(breaks = seq(0.00, 1.00, by = 0.20)) +
        theme_minimal()
}

display_and_save_plot(
    plot_variance_expectation,
    save = export_graphs_flag,
    filename = "./output/actl1101 assignment question 2 3 expectation variance kurtosis 06082025.svg",
    width = 8,
    height = 5
)




# ======== PMF THINKING FOR QUESTION 2.3 ========

# Function to plot the PMF of the BAZIG distribution
plot_dbazig_pmf <- function(q0_val, expectation_mode = FALSE) {
    plot_data <- data.frame(
        x = x,
        prob = dbazig(x, q0 = q0_val, p = param.p, b = param.b)
    )
    # plot_data$prob <- ifelse(expectation_mode, plot_data$prob * x, plot_data$prob)
    ggplot(plot_data, aes(x = factor(x), y = prob)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "PMF of BAZIG", x = "Damage (million $)", y = "Probability") +
        theme_minimal()
}

plot_dbazig_pmf(0.2)
plot_dbazig_pmf(0.4)
plot_dbazig_pmf(0.8)
plot_dbazig_pmf(0.95)

plot_dbazig_pmf(0.2, expectation_mode = TRUE)
plot_dbazig_pmf(0.4, expectation_mode = TRUE)
plot_dbazig_pmf(0.8, expectation_mode = TRUE)
plot_dbazig_pmf(0.95, expectation_mode = TRUE)

for (q0 in c(0.2, 0.4, 0.8, 0.95)) {
    display_and_save_plot(
        plot_function = plot_dbazig_pmf,
        save = export_graphs_flag,
        filename = sprintf("./output/actl1101 assignment question 2 4 pmf %.2f 01082025.svg", q0),
        width = 8,
        height = 5,
        q0_val = q0
    )
}



# ======== OTHER COMPUTATIONS FOR QUESTION 2.4 ========


loading_ratios_matrix <- loadings_matrix
for (i in 1:nrow(loadings_matrix)) {
    if (i == 1) {
        loading_ratios_matrix[i, ] <- NA
        next
    }
    loading_ratios_matrix[i, ] <- loadings_matrix[i, ] / loadings_matrix[i - 1, ]
}
loading_ratios_matrix

