# Load the packages
library(dplyr)
library(ggplot2)
library(car)
library(data.table)


# Read the data
TempData  <-  read.csv("~/Desktop/R Project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv", stringsAsFactors = FALSE)
Locations <- read.csv("~/Desktop/R Project/GlobalLandTemperatures/Locations.csv", header = TRUE)
TempData <- tbl_df(TempData)
TempData$dt <- as.Date(TempData$dt)
TempData <- na.omit(TempData)

# Format a year field column
TempData$Year <- format(TempData$dt, '%Y')

# Use Data.table to keep only those rows with complete 12 month observations
TempData <- data.table(TempData)
setkey(TempData,City,Year)
Test1 <- TempData[.(TempData[,.N,by=.(City,Year)][N==12,!"N",with=F])]


# Calculate the mean temeperature, by city, every year
CYMeanTemp <- as.data.frame(Test1) %>% group_by(City, Year) %>% summarise(mean(AverageTemperature))
names(CYMeanTemp) <- c("City", "Year", "MeanTemp")


# Create a list containing the mean temperatures for the second and first half of the data for each city
CYMeanTemp <- as.data.frame(CYMeanTemp)
CityNames <- unique(CYMeanTemp$City)
CitySubsets <- list()
Mean1H <- c()
Mean2H <- c()
for (i in 1:length(CityNames)) {
  CitySubsets[[i]] <- subset(CYMeanTemp, City == CityNames[i])
  Mean1H[i] <-  mean(as.data.frame(CitySubsets[i])[1:round(1/2*length(CitySubsets[[i]][,1])),c("MeanTemp")])
  Mean2H[i] <- mean(as.data.frame(CitySubsets[i])[(1 + round(1/2*length(CitySubsets[[i]][,1]))):length(CitySubsets[[i]][,1]), c("MeanTemp")])
}
names(CitySubsets) <- CityNames

# Create a dataframe to hold the most important high level info from the dataset
MasterDF <- cbind(CityNames, Locations, Mean1H, Mean2H)

# Take the difference between Mean2H and Mean1H
MasterDF <- MasterDF %>% mutate(Warming = Mean2H - Mean1H)
colnames(MasterDF)[1] <- "City"
colnames(MasterDF)[6] <- "Change (°C) (2H-1H)"

# Create a dataframe, DF, that will create a visual table in markdown with summary information. 
roundit <- function(x) round(x,5)
DF <- as.data.frame(sapply(MasterDF[,4:6], roundit))
DF <- cbind(MasterDF[,1:3], DF)
TableFinal <- DF[,c("City", "Mean1H", "Mean2H", "Change (°C) (2H-1H)")]









