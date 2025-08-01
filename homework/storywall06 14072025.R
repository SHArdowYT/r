

airbnb.data <- read.csv("./data/AirbnbSydney.csv")
# airbnb.data$host_response_rate <- as.numeric(sub("%", "", airbnb.data$host_response_rate)) / 100
# airbnb.data$host_acceptance_rate <- as.numeric(gsub("%", "", airbnb.data$host_acceptance_rate)) / 100

price.per.guest <- airbnb.data$price / airbnb.data$accommodates

quantile(price.per.guest, probs = c(0.05, 0.95))


# library(ggplot2)

price.per.guest_mean = mean(price.per.guest)
price.per.guest_sd = sd(price.per.guest)


plot_price_per_guest <- function() {
    
    # hist(price.per.guest, breaks = 50, probability = TRUE, col ="lightblue")
    hist(price.per.guest, breaks = 50, probability = TRUE, col ="lightblue", main = "Histogram of Price per Guest", xlab = "Price per Guest Night ($ day^-1)")
    curve(dnorm(x, price.per.guest_mean, price.per.guest_sd), n = 1000, add = TRUE, lwd = 3, col = "skyblue3")
    
    legend("topright", legend = c("Prices", "Normal Curve"), col = c("lightblue", "skyblue3"), fill = c("lightblue", NA), border = c("black", NA), lwd = c(NA, 3), lty = c(1, 1))
    grid()

}

plot_price_per_guest()

png(filename = "storywall06 14072025.png", width = 3840, height = 2160, pointsize = 72)
plot_price_per_guest()
dev.off()

svg(filename = "storywall06 14072025.svg", width = 8, height = 6)
plot_price_per_guest()
dev.off()


outlier.lower <- quantile(price.per.guest, 0.25, names = FALSE) - 1.5 * IQR(price.per.guest)
outlier.upper <- quantile(price.per.guest, 0.75, names = FALSE) + 1.5 * IQR(price.per.guest)


# cor(price.per.guest, airbnb.data)
library(dplyr)

airbnb.data_numeric <- airbnb.data %>%
    select(where(is.numeric))

price.per.guest_correlation_matrix <- cor(price.per.guest, airbnb.data_numeric)
# price.per.guest_correlation_matrix <- cor(price.per.guest, airbnb.data_numeric, use = "everything", method = c("pearson", "kendall", "spearman"))
# price.per.guest_correlation_matrix <- cor(price.per.guest, airbnb.data_numeric, method = "spearman")
# class(price.per.guest_correlation_matrix)
price.per.guest_correlation_matrix

# sort(price.per.guest_correlation_matrix, decreasing = TRUE)



price.per.guest_correlation_frame <- data.frame(correlation = price.per.guest_correlation_matrix[1, ])

price.per.guest_correlation_frame_sorted <- price.per.guest_correlation_frame %>%
    arrange(desc(correlation))

price.per.guest_correlation_frame_sorted


library(ggplot2)



plot_longitude_price.per.guest <- function(max_rows = 1000) {

    length = min(max_rows, length(price.per.guest))
    
    ggplot(airbnb.data[1:length, ], aes(x = longitude, y = price.per.guest[1:length])) + 
        geom_point(colour = "aquamarine3", shape = 4, size = 2) + 
        # geom_point(colour = "aquamarine3", alpha = 0.9, shape = 4, size = 2) + 
        geom_smooth(method = "lm", se = TRUE, colour = "cadetblue") +
        scale_x_continuous(breaks = seq(150.6, 151.4, by = 0.1))+ 
        labs(x = "longitude", y = "price per guest") +
        theme_minimal() + 
        theme(panel.grid.minor = NULL)
    
}

plot_longitude_price.per.guest(1000000)

svg(filename = "storywall06 15072025.svg", width = 8, height = 5)
plot_longitude_price.per.guest(1000000)
dev.off()



airbnb.data$host_response_time <- factor(airbnb.data$host_response_time, levels = c("within an hour", "within a few hours", "within a day", "a few days or more", "N/A"), ordered = TRUE)
unique(airbnb.data$host_response_time)



sorted_neighbourhoods <- airbnb.data %>%
    group_by(neighbourhood) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

airbnb.data_filtered_neighbourhoods <- airbnb.data %>%
    filter(neighbourhood %in% sorted_neighbourhoods$neighbourhood[1:4])

plot_neighbourhood_host_response_time <- function() {
    
    ggplot(airbnb.data_filtered_neighbourhoods, aes(x = neighbourhood, fill = host_response_time)) +
        geom_bar(position = "fill") +
        labs(x = "neighbourhood", y = "percentage") +
        theme_minimal() +
        coord_flip() +
        xlim(limits = rev(sorted_neighbourhoods$neighbourhood[1:4]))
    
}

plot_neighbourhood_host_response_time()

svg(filename = "storywall06 16072025.svg", width = 8, height = 4)
plot_neighbourhood_host_response_time()
dev.off()





