#Revenue is set between 0.1 and 30
###Read file
print("Loading data from the given file...")
startTime <- proc.time()
library(readxl)
filedata <- read_excel("D:/Projects/FYP/Code/raw.xlsx")
dataset <- filedata

###Loading required libraries
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(caret)

###Adding a new parameter 
dataset <- subset(dataset, dataset$Quantity>0)
dataset <- subset(dataset, dataset$UnitPrice>0)
dataset <- dataset %>% mutate(Revenue = Quantity * UnitPrice)
dataset <- subset(dataset, dataset$Revenue >0 & dataset$Revenue<= 30)

###Removing NAs
sapply(dataset, function(x) sum(is.na(x)))
dataset <- na.omit(dataset)

###Reformatting date variable
dataset$InvoiceDate <- as.Date(dataset$InvoiceDate, format = "%Y-%m-%d %H:%M:%S")
endTime <- proc.time() - startTime
loadTime <- paste("Finished loading data and preprocessing in", endTime[1], "seconds.")
print(loadTime)
print(paste("Dataset has", nrow(dataset), "observations."))
