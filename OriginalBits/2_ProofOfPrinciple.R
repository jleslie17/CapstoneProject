
##As a proof of principle for the workflow, 
##I will show the plot of the 
##the average star rating over time for one restaurant.

##Pull out the restaurant with the highest number of reviews
##and get all of its reviews
PoPHiCount <- RestsHiCount[RestsHiCount$review_count == 
                                    max(RestsHiCount$review_count),]
PoPHiRev <- RestRevs[RestRevs$business_id == PoPHiCount$business_id,]

##Grab just the stars and date data for business with the most reviews
PoPDF <- data.frame(Stars = PoPHiRev$stars, Date = PoPHiRev$date)
PoPDF$YearMonth <- strftime(PoPDF$Date, "%Y-%m")
PoPDF$YearMonth <- as.Date(as.yearmon(PoPDF$YearMonth))

##Group all of the review by month
PoPDFgrouped <- group_by(PoPDF, YearMonth)
PoPDTSum <- summarise(PoPDFgrouped, AvStarsPerMonth = mean(Stars))
PoPDFSum <- data.frame(PoPDTSum)

##Plot the star rating over time, averaged monthly
#png(filename = "Plot1.png")
g1 <- ggplot(PoPDFSum, aes(x=YearMonth, y=AvStarsPerMonth)) + 
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
#dev.off()