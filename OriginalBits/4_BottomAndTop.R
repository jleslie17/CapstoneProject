library(jsonlite)
library(tm)
library(dplyr)
library(zoo)
library(ggplot2)
source("3_MakeLMDataframe.R")


##Subsetting for the businesses in the top and bottom 5% trend
bottom <- quantile(SlopeDF$Slope, probs = 0.05)
top <- quantile(SlopeDF$Slope, probs = 0.95)

##These are the regression lines for the worst 5% (132 businesses):
WorstTrends <- SlopeDF[SlopeDF$Slope < bottom,]
BestTrends <- SlopeDF[SlopeDF$Slope > top,]

##This gathers the review texts for the worst 5%
WorstRevs2 <- StarsDates2[StarsDates2$business_id %in%
                                  WorstTrends$Business,]
BestRevs <- StarsDates2[StarsDates2$business_id %in%
                                BestTrends$Business,]

##Group by business and YearMonth
WorstRevsGrouped <- group_by(WorstRevs2, business_id, YearMonth)
BestRevsGrouped <- group_by(BestRevs, business_id, YearMonth)

##Gathers the review texts for each month
WorstRevsGathered <- summarise(WorstRevsGrouped, 
                               Texts = paste(text, collapse = ''))
WorstRevsGathered <- data.frame(WorstRevsGathered)
BestRevsGathered <- summarise(BestRevsGrouped, 
                               Texts = paste(text, collapse = ''))
BestRevsGathered <- data.frame(BestRevsGathered)

rm(ReviewData)
rm(BusData)


##Plotting the bottom 5% only lines
StarsBottom <- StarsDatesAveraged2[StarsDatesAveraged2$business_id %in%
                                           WorstTrends$Business,]
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

##Plotting the top 5% only lines
StarsTop <- StarsDatesAveraged2[StarsDatesAveraged2$business_id %in%
                                           BestTrends$Business,]
gTop <- ggplot(PlotData, 
                  aes(x = YearMonth, y = AvStarsPerMonth)) +
        geom_line(alpha = 0)
pTop <- gbottom + geom_abline(aes(intercept = BestTrends$Intercept, 
                                     slope = BestTrends$Slope),
                                 data=BestTrends,
                                 alpha = 0.5) +
        ggtitle("Changes in star ratings over time for \nRestaurants in the best 5% Quantile") +
        ylim(0,10) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(pTop)

