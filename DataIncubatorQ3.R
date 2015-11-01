library(jsonlite)
library(tm)
library(wordcloud)
library(dplyr)
library(zoo)
library(ggplot2)

##Load the data. 
##I will use only the business and review files for exploratory analysis
##Files are contained in a folder "yelp_dataset_challenge_academic_dataset"
##Unzipped files are json format
PATH <- "yelp_dataset_challenge_academic_dataset"
BusData <- stream_in(file(paste(PATH, "/",
                                "yelp_academic_dataset_business.json",sep = "")))
reviewData <- stream_in(file(paste(PATH, "/", 
                                   "yelp_academic_dataset_review.json", sep = "")))

##Save the data in rds format for future use
saveRDS(reviewData, "reviewData.rds")
saveRDS(BusData, "businessData.rds")

##Can start from here once data saved as rds files in working directory

##Reload the data
BusData <- readRDS("businessData.rds")
ReviewData <- readRDS("reviewData.rds")

##Grab all businesses that have "Restaurants" in their category
Restaurants <- BusData[grepl("Restaurants", BusData$categories),]
##Grabbing all reviews of the restaurant businesses
RestRevs <- ReviewData[ReviewData$business_id %in% 
                               Restaurants$business_id,]
##Selecting a shortlist of restaurants that have 50 or more reviews
Threshold <- 50
RestsHiCount <- Restaurants[Restaurants$review_count >= Threshold,]
##Gather all the reviews for those restaurants
RestRevsHiCount <- RestRevs[RestRevs$business_id %in%
                                    RestsHiCount$business_id,]

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
p1 <- g1 + stat_smooth(method = lm, se=F, colour = "blue") +
        ggtitle("Changes in star ratings over time for one restaurant") +
        ylim(0,5) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(p1)

##Now apply this scheme to the rest of the restaurants
##Make a new data frame with only stars and date data
StarsDates <- data.frame(Business = RestRevsHiCount$business_id,
                         Date = RestRevsHiCount$date,
                         Stars = RestRevsHiCount$stars)
##Add a variable 'YearMonth' that is only the month, ignoring days
StarsDates$YearMonth <- strftime(StarsDates$Date, "%Y-%m")
StarsDates$YearMonth <- as.Date(as.yearmon(StarsDates$YearMonth))

##Group by business and YearMonth
StarsDatesGrouped <- group_by(StarsDates, Business, YearMonth)
##Calculate the average stars per month
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
##This look iterates, by business ID, through the data frame of 
##grouped, averaged star ratings. It calls the FindCoefs function
##at for each business in the short list.
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
g2 <- ggplot(PlotData, 
            aes(x = YearMonth, y = AvStarsPerMonth)) +
        geom_line(alpha = 0)
p2 <- g2 + geom_abline(aes(intercept = SlopeDF$Intercept, 
                         slope = SlopeDF$Slope),
                     data=SlopeDF,
                     alpha = 0.05) +
        ggtitle("Changes in star ratings over time for Restaurants") +
        ylim(0,5) +
        xlab("Year") +
        ylab("Average Star rating \n(calculated monthly") +
        theme(axis.title.x=element_text(size = 14)) +
        theme(axis.title.y=element_text(size = 14))
print(p2)
