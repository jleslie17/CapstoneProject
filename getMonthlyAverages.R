require(dplyr)
require(zoo)
require(ggplot2)


##Here we build a dataframe with the monthly averages
##for stars from the BID
BIDRevs <- RestRevsHiCount[RestRevsHiCount$business_id == BID,]

##Make a new data frame with only stars and date data
BIDStarsDates <- select(BIDRevs, business_id, 
                      date, stars, review_id, text)
##Add a variable 'YearMonth' that is only the month, ignoring days
BIDStarsDates$YearMonth <- strftime(BIDStarsDates$date, "%Y-%m")
BIDStarsDates$YearMonth <- as.Date(as.yearmon(BIDStarsDates$YearMonth))

##Group by business and YearMonth
BIDStarsDatesGrouped <- group_by(BIDStarsDates, business_id, YearMonth)
##Calculate the average stars per month and discard RevID, Date, Text
BIDStarsDatesGrouped <- summarise(BIDStarsDatesGrouped, 
                                 AvStarsPerMonth = mean(stars))
##Convert back into a data frame
BIDStarsDatesGrouped <- data.frame(BIDStarsDatesGrouped)


fit <- lm(AvStarsPerMonth~YearMonth, data = BIDStarsDatesGrouped)


##Plot the averages over time
Data <- BIDStarsDatesGrouped
g <- ggplot(Data, 
             aes(x = YearMonth, y = AvStarsPerMonth))
p <- g + geom_point() + 
        stat_smooth(method = lm, colour = "blue") +
        ggtitle("Plot 2 \nChanges in star ratings over time") +
        ylim(0,5) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(p)

print("Change in star rating (stars per year):")
x <- print(round(365*(fit$coef[[2]]),2))

