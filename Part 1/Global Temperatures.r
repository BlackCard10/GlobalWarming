# Load the packages
library(dplyr)
library(ggplot2)
library(car)
library(grid)
library(gridExtra)


# Read the data
TempData  <-  read.csv("~/Desktop/R Project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv")


# Subset the data for Ho Chi Minh City (in honorarium)
HCMCTemps <- subset(TempData, City == "Ho Chi Minh City")


# Missing data is bothersome. See if there are any spans of 100 years + w/o missing data
StripYear <- function(x) substring(x, 1,4)
HCMCTemps$Year <- sapply(HCMCTemps$dt, StripYear)
HCMCTemps$Year <- as.numeric(HCMCTemps$Year)
HCMCTBlanks <- subset(HCMCTemps, is.na(AverageTemperature) & is.na(AverageTemperatureUncertainty))
YearBlanks <- unique(HCMCTBlanks$Year) # The span from 1863 and 2012 does not exhibit any missing data


# Subset the HCMCTemps data to contain only 1863 to 2012
WorkingHCMCTemps <- subset(HCMCTemps, Year <= 2012 & Year > 1862)
WorkingHCMCTemps$dt <- as.Date(WorkingHCMCTemps$dt)


# Create an aggregated temp X year dataset
TempYear <- WorkingHCMCTemps %>% 
            select(Year, AverageTemperature, AverageTemperatureUncertainty) %>% 
            group_by(Year) %>% 
            summarise(mean(AverageTemperature), mean(AverageTemperatureUncertainty))

names(TempYear) <- c("Year","MeanTemp", "AverageTemperatureUncertainty")


# Plot the Dataset & a trendline
G1 <- ggplot(TempYear, aes(x = Year, y = MeanTemp)) + 
        geom_point() + 
        geom_smooth(method = "lm", se = FALSE) +
        theme(plot.title = element_text(size = 11)) +
        ggtitle("Average Temperature(Celcius)")
TempYearLM <- lm(MeanTemp ~ Year, data = TempYear)
summary(TempYearLM)

G1 

G2 <- ggplot(TempYear, aes(x = Year, y = AverageTemperatureUncertainty)) +
      geom_point() + 
      geom_smooth() +
      theme(plot.title = element_text(size = 11)) +
      ggtitle("Average Uncertainty in Measurement(Celcius)")
G2   

grid.arrange(G1, G2)


# Add a 1/2 column to mark the first/second half of the data respectively
WorkingHCMCTemps[1:900,"Half"] <- 1
WorkingHCMCTemps$Half[901:1800] <- 2
Testset1 <- data.frame(WorkingHCMCTemps$AverageTemperature[1:900])
Testset2 <- data.frame(WorkingHCMCTemps$AverageTemperature[901:1800])
names(Testset1) <- ("Temp")
names(Testset2) <- ("Temp")
WorkingHCMCTemps$Half <- as.factor(WorkingHCMCTemps$Half)
 
# Plot a box and whiskery chart with the mean Monthly temps by first and second half 
B1 <- ggplot(WorkingHCMCTemps, aes(Half, AverageTemperature, group = Half, fill = Half)) + 
      scale_fill_discrete(name = "Time Period", 
      labels = c("1863 - 1938", "1939 - 2012")) +
      ggtitle(" Ho Chi Minh City Average Monthly Temperature (Celcius)") + 
      theme(plot.title = element_text(size = 11), 
      axis.text.x = element_blank(),
      axis.title.x = element_blank()) +
      geom_boxplot()
B1 

TestSummary <- data.frame(round(mean(Testset1),3), round(mean(Testset2),3), 
                          round(median(Testset1),3), round(median(Testset2),3))
TestMatrix <- matrix(TestSummary, nrow = 2, byrow = TRUE)
dimnames(TestMatrix) <- list(c("Mean", "Median"), c("1863 - 1938", "1939 - 2012"))


# QQplot across set 1 and 2 respectively
QQ1 <- qqnorm(WorkingHCMCTemps$AverageTemperature[1:900])
qqline(WorkingHCMCTemps$AverageTemperature[1:900], col = "red")
QQ2 <- qqnorm(WorkingHCMCTemps$AverageTemperature[901:1800])
qqline(WorkingHCMCTemps$AverageTemperature[901:1800], col = 'red')


H1 <- ggplot(Testset1, aes(x = Temp)) + 
      geom_histogram(col = "red", fill= "lightblue",  bins = 30) +
      ggtitle("1H Temperature Distribution")

H2 <- ggplot(Testset2, aes(x = Temp)) + 
      geom_histogram(col = "red", fill= "lightblue",  bins = 30) +
      ggtitle("2H Temperature Distribution")

grid.arrange(H1, H2)

# Test for the difference in means between the two datasets.  Use Welches T.  
Tresult <- t.test(Testset1, Testset2)
Tvector <- c(Tresult$method, Tresult$statistic, Tresult$p.value, Tresult$conf.int,Tresult$estimate)
TDetail <- matrix(Tvector, nrow = 1, dimnames = list(c("Results"), 
                        c("Method", "T Statistic", "P.Value", "Conf.Int(LB)", "Conf.Int(UB)", "Mean of H1", "Mean of H2")))

























