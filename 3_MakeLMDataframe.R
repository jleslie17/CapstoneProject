
##Now apply the scheme to the rest of the restaurants
##Make a new data frame with only stars and date data
StarsDates <- data.frame(Business = RestRevsHiCount$business_id,
                         Date = RestRevsHiCount$date,
                         Stars = RestRevsHiCount$stars,
                         RevID = RestRevsHiCount$review_id,
                         RevText = RestRevsHiCount$text)
##Add a variable 'YearMonth' that is only the month, ignoring days
StarsDates$YearMonth <- strftime(StarsDates$Date, "%Y-%m")
StarsDates$YearMonth <- as.Date(as.yearmon(StarsDates$YearMonth))

##Group by business and YearMonth
StarsDatesGrouped <- group_by(StarsDates, Business, YearMonth)
##Calculate the average stars per month and discard RevID, Date, Text
StarsDatesAveraged <- summarise(StarsDatesGrouped, 
                                AvStarsPerMonth = mean(Stars))
##Convert back into a data frame
StarsDatesAveraged <- data.frame(StarsDatesAveraged)

##Here I create a function that will calculate a linear regression
##model for each business, plotting average star rating over time.
##It returns the intercept and slope of the regression line.
FindCoefs <- function(DataFrame) {
        SlopeTemp <- numeric()
        fit <- lm(DataFrame$AvStarsPerMonth ~ DataFrame$YearMonth)
        TempCoefs <- fit$coefficients
        return(TempCoefs)
}
##Initiate an empty dataframe 
SlopeDF <- data.frame(Business = character(nrow(RestsHiCount)), 
                      Intercept = numeric(nrow(RestsHiCount)),
                      Slope = numeric(nrow(RestsHiCount)),
                      stringsAsFactors = F)
count <- 0 #Set counter to 0
##This loop iterates, by business ID, through the data frame of 
##grouped, averaged star ratings. It calls the FindCoefs function
##for each business in the short list.
##It is assembled into the dataframe SlopeDF
for(i in RestsHiCount$business_id) {
        count <- count + 1
        TempBus <- filter(StarsDatesAveraged, Business == i)
        ReturnedCoefs <- FindCoefs(TempBus)
        SlopeDF[count,1] <- i
        SlopeDF[count,2] <- ReturnedCoefs[[1]]
        SlopeDF[count,3] <- ReturnedCoefs[[2]]
}

##These two lines give ggplot a date range (PlotData) to use to make the
##graph, without going through the entire dataset.
FirstBus <- StarsDatesAveraged[StarsDatesAveraged$YearMonth == 
                                       min(StarsDatesAveraged$YearMonth),]
PlotData <- StarsDatesAveraged[StarsDatesAveraged$Business == FirstBus[1,1],]

#png(filename = "Plot2.png")
g2 <- ggplot(PlotData, 
             aes(x = YearMonth, y = AvStarsPerMonth)) +
        geom_line(alpha = 0)
p2 <- g2 + geom_abline(aes(intercept = SlopeDF$Intercept, 
                           slope = SlopeDF$Slope),
                       data=SlopeDF,
                       alpha = 0.05) +
        ggtitle("Plot 2 \nChanges in star ratings over time for Restaurants") +
        ylim(0,10) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(p2)
#dev.off()
