
##As a proof of principle for the workflow, 
##I will show the plot of the 
##the average star rating over time for one restaurant.

##Pull out the restaurant with the highest number of reviews
##and get all of its reviews
TestHiCount <- RestsHiCount[RestsHiCount$review_count == 
                                    max(RestsHiCount$review_count),]
TestHiRev <- RestRevs[RestRevs$business_id == TestHiCount$business_id,]

##Grab just the stars and date data for business with the most reviews
TestDF <- data.frame(Stars = TestHiRev$stars, Date = TestHiRev$date)
TestDF$YearMonth <- strftime(TestDF$Date, "%Y-%m")
TestDF$YearMonth <- as.Date(as.yearmon(TestDF$YearMonth))

##Group all of the review by month
TestDFgrouped <- group_by(TestDF, YearMonth)
TestDTSum <- summarise(TestDFgrouped, AvStarsPerMonth = mean(Stars))
TestDFSum <- data.frame(TestDTSum)

##Plot the star rating over time, averaged monthly
g1 <- ggplot(TestDFSum, aes(x=YearMonth, y=AvStarsPerMonth)) + 
        geom_line()
p1 <- g1 + stat_smooth(method = lm, colour = "blue") +
        ggtitle("Plot 1 \nChanges in star ratings over time for one restaurant") +
        ylim(0,5) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14)) +
        ggplot2::annotate("text",
                          x=as.Date("2011-01-01"), y=3.2,
                          label="slope = -0.023 stars/year (±95% CI)", 
                          colour = "blue")
print(p1)
