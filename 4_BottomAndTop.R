source("3_MakeLMDataframe.R")


##Subsetting for the businesses in the bottom 5% trend
bottom <- quantile(SlopeDF$Slope, probs = 0.05)
##These are the regression lines for the worst 5% (132 businesses):
WorstTrends <- SlopeDF[SlopeDF$Slope < bottom,]



StarsBottom <- StarsDatesAveraged[StarsDatesAveraged$Business %in%
                                          WorstTrends$Business,]
##Plotting the bottom 5% only lines
gbottom <- ggplot(PlotData, 
                  aes(x = YearMonth, y = AvStarsPerMonth)) +
        geom_line(alpha = 0)
pbottom <- gbottom + geom_abline(aes(intercept = WorstTrends$Intercept, 
                                     slope = WorstTrends$Slope),
                                 data=WorstTrends,
                                 alpha = 0.5) +
        ggtitle("Changes in star ratings over time for \nRestaurants in the worst 5% Quantile") +
        ylim(0,10) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(pbottom)

WorstRevs <- RestRevsHiCount[RestRevsHiCount$business_id %in%
                                       WorstTrends$Business,]




##These are the 20 worst ones
SortedSlopeDF <- SlopeDF[with(SlopeDF, order(Slope)),]
Worst20 <- SortedSlopeDF[1:20,]


## Plot the bottom 5% with stars
gBottomStars <- ggplot(StarsBottom, aes(x=YearMonth, 
                                        y=AvStarsPerMonth,
                                        colour=Business))
pBottomStars <- gBottomStars + geom_point(size=1) + 
        geom_smooth(method=lm, se=F)+
        ylim(0,5)+
        xlim(PlotData$YearMonth[1], PlotData$YearMonth[103])
print(pBottomStars)        


gWorst20Lines <- ggplot(PlotData, 
                        aes(x = YearMonth, y = AvStarsPerMonth)) +
        geom_line(alpha = 0)
pWorst20Lines <- gWorst20Lines + 
        geom_abline(aes(intercept = Intercept, 
                        slope = Slope),
                    data=Worst20,
                    alpha = 0.5) +
        ggtitle("Worst 20 (lines)") +
        ylim(0,5) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(pWorst20Lines)

RevsWorst20 <- RestRevsHiCount[RestRevsHiCount$business_id %in%
                                       Worst20$Business,]
StarsWorst20 <- StarsDatesAveraged[StarsDatesAveraged$Business %in%
                                           Worst20$Business,]


gWorst20 <- ggplot(StarsWorst20, aes(x=YearMonth, 
                                     y=AvStarsPerMonth,
                                     colour=Business))
pWorst20 <- gWorst20 + geom_point(size=1) + 
        geom_smooth(method=lm, se=F)+
        ylim(0,5)+
        xlim(PlotData$YearMonth[1], PlotData$YearMonth[103])
print(pWorst20)

