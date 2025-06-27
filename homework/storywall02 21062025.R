# Yes, because the first tutorial motivated me to do some of the exercises, and was also well taught.


# Yes, because I need it to be able to do this task.


airbnbSydneyData <- read.csv(file = "./data/AirbnbSydney.csv")
str(airbnbSydneyData)
dim(airbnbSydneyData) # c(11407, 38)
# The dataframe has 11407 rows and 38 columns.
# The rows represent individual Airbnb listings and the columns attributes of said listings.


airbnbSydneyDataPrices <- airbnbSydneyData$price
head(sort(airbnbSydneyDataPrices, decreasing = TRUE), 100)
# 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
# 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 999
# 999  999  999  999  999  999  995  995  995  995  993  993  993  993  991  990  990  990  990  989  986  986  985  985  980 
# 980  980  980  980  978  971  971  970  970  964  960  959  959  957  953  950  950  950  950  950  950  950  950  950  950

# This dataset probably includes all of the possible listings in Sydney, because it is unlikely that 49 properties would have the same nightly price of $1000
# (though it might just be that $1000 looks like a good number to many people, given that there are a significant number of $999 per night listings as well).
# Assuming this is the case, the maximum being $1000 and with such a frequency suggests that there is simply a cap to the price that can be set in Airbnb,
# or that the data has just been restricted to a particular range ($0--$1000).


hist(airbnbSydneyDataPrices, breaks = 100, col ="firebrick")
hist(log10(airbnbSydneyDataPrices), breaks = 100, col ="dodgerblue") # why not use log10 so i can better get an idea of the order of magnitude
# The fact that we get a symmetrical bell-curvey distribution in the log plot (albeit cutoff past 10^3) suggests that the prices follow a roughly log-normal distribution.
# in the linear plot, we also see mysterious spikes just before numbers roll over to the next ten/hundred (e.g., 290, 440, 590), which is consistent with how we see some prices to be set.


airbnbSydneyIncomeMean = 365 * sum(airbnbSydneyDataPrices) # 1159364830
# We find the total yearly income to be $1.159 billion, with the assumption of full occupancy every day of the year.
# Of course, this is unrealistic -- at least some days need to be set aside for cleaning, so we might better estimate the yearly income with:
airbnbSydneyIncomeAvailability = sum((365 - airbnbSydneyData$availability_365) * airbnbSydneyData$price) # 599185375
# With this method we find the yearly income to be half of what was calculated than with the assumption of full occupancy.


mean(airbnbSydneyData$review_scores_rating[airbnbSydneyData$neighbourhood == "Randwick"]) # 4.740491


airbnbSydneyDataRandwick <- subset(airbnbSydneyData, neighbourhood == "Randwick")
airbnbRandwickMatrix <- cbind(airbnbSydneyDataRandwick$review_scores_rating, airbnbSydneyDataRandwick$review_scores_accuracy, airbnbSydneyDataRandwick$review_scores_cleanliness, airbnbSydneyDataRandwick$review_scores_checkin,
                              airbnbSydneyDataRandwick$review_scores_communication, airbnbSydneyDataRandwick$review_scores_location, airbnbSydneyDataRandwick$review_scores_value)
ratingWeights = c(0.40, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10)
airbnbRandwickWeightedRatings <- airbnbRandwickMatrix %*% ratingWeights
median(airbnbRandwickWeightedRatings) # 4.85


sum(airbnbSydneyData$room_type == "Hotel room") / nrow(airbnbSydneyData) # 0.006224248
# No surprises here because it makes sense for hotels to increase their exposure by listing their rooms on Airbnb,
# and sum(airbnbSydneyData$room_type == "Hotel room") == 71, or 0.6224248% is not very many hotels/listings.


# No, because learning from the ground up (i.e., doing something manually, and then simplifying that process in some way) seems like the best way to learn to do something.


# ...
