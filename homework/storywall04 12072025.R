
airbnb.data <- read.csv(file = "./data/AirbnbSydney.csv")
# class(airbnb.data)

cor(airbnb.data$price, airbnb.data$accommodates)
cor(airbnb.data$price, airbnb.data$review_scores_rating)
cor(airbnb.data$price, airbnb.data$number_of_reviews_l30d)

library(dplyr)

typeof(airbnb.data$room_type)
unique(airbnb.data$room_type)


room_type_factor <- factor(airbnb.data$room_type, levels = c("Shared room", "Private room", "Hotel room", "Entire home/apt"), ordered = TRUE)
levels(room_type_factor)
boxplot(price ~ room_type_factor, data = airbnb.data, col= "cornflowerblue")


airbnb.data$room_type_factor <- room_type_factor
airbnb.data %>%
    group_by(room_type_factor) %>%
    summarise(room_type_factor.count = n(),
              room_type_factor.mean = mean(price),
              room_type_factor.median = median(price),
              room_type_factor.stdev = sd(price))


as.Date(airbnb.data$first_review, format="%d/%m/%Y")
as.Date(airbnb.data$last_review, format="%d/%m/%Y")


delta_review_dates = as.Date(airbnb.data$last_review, format="%d/%m/%Y") - as.Date(airbnb.data$first_review, format="%d/%m/%Y")

mean(delta_review_dates)
sd(delta_review_dates)
# mean(as.numeric(delta_review_dates))
# sd(as.numeric(delta_review_dates))


price_per_guest = airbnb.data$price / airbnb.data$accommodates

neighbourhood.price_per_guest <- airbnb.data %>%
    group_by(neighbourhood) %>%
    summarise(neighbourhood.price_per_guest = mean(price / accommodates)) %>%
    arrange(desc(neighbourhood.price_per_guest))

head(neighbourhood.price_per_guest, 5)
tail(neighbourhood.price_per_guest, 5)


