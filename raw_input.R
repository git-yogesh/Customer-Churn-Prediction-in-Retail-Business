###Read file
library(readxl)
rawdata <- read_excel("D:/Projects/FYP/Code/raw.xlsx")
dataset <- rawdata
setwd("D:/Projects/FYP/Code")

###Loading required libraries
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(caret)

###Factor the variables
factor(dataset$Country)
factor(dataset$InvoiceNo)
factor(dataset$StockCode)
factor(dataset$CustomerID)

###Adding a new parameter 
dataset <- subset(dataset, dataset$Quantity>0)
dataset <- subset(dataset, dataset$UnitPrice>0)
dataset <- dataset %>% mutate(Revenue = Quantity * UnitPrice)

###Reformatting date variable
dataset$InvoiceDate <- as.Date(dataset$InvoiceDate, format = "%Y-%m-%d %H:%M:%S")

###Invoice Aggregations

#Total amount for each transaction
totalamount <-aggregate(x = dataset$Revenue, by = list(dataset$InvoiceNo), FUN = sum)
colnames (totalamount) <- c("InvoiceNo", "TotalAmount")
summary(totalamount)

#The number of unique products ordered per transaction
unique <-aggregate(x = dataset$StockCode, by = list(dataset$InvoiceNo), FUN = length)
colnames (unique) <- c("InvoiceNo", "UniqueProducts")
summary(unique)

#Dates for each transaction
transDate <-aggregate(x = dataset$InvoiceDate, by = list(dataset$InvoiceNo), FUN = mean)
colnames (transDate) <- c("InvoiceNo", "TransactionDate")
summary(transDate)

#Aggregation by max for customer ID
customerID <-aggregate(x = dataset$CustomerID, by = list(dataset$InvoiceNo), FUN = max)
colnames (customerID) <- c("InvoiceNo", "CustomerID")
summary(customerID)

#Combining...
invaggdata <- Reduce(function(x,y) merge(x,y,all=TRUE, by="InvoiceNo"), list(totalamount, unique, transDate, customerID))
str(invaggdata)
#invaggdata <- na.omit(invaggdata)


####Frequency calc function- called from different script, churn definition- 6 months


###Customer Aggregations

#(This shows we have complete cases for all transactions for only 3100 customers)
custfreq <-aggregate(x = invaggdata$InvoiceNo, by = list(invaggdata$CustomerID), FUN = length)
colnames (custfreq) <- c("CustomerID", "Frequency")
str(custfreq)

#Last date data for each customer
lastdate <-aggregate(x = invaggdata$TransactionDate, by = list(invaggdata$CustomerID), FUN = max)
colnames (lastdate) <- c("CustomerID", "LastTransactionDate")
str(lastdate)

#Average amount for each customer's transactions
avgamount <-aggregate(x = invaggdata$TotalAmount, by = list(invaggdata$CustomerID), FUN = mean)
colnames (avgamount) <- c("CustomerID", "AvgAmount")
str(avgamount)

#Total amount for each customer's transactions
totalamtpercust <-aggregate(x = invaggdata$TotalAmount, by = list(invaggdata$CustomerID), FUN = sum)
colnames (totalamtpercust) <- c("CustomerID", "TotalAmount")
str(totalamtpercust)

#Combining...
custaggdata <- Reduce(function(x,y) merge(x,y,all=TRUE, by="CustomerID"), list(custfreq, lastdate, avgamount, totalamtpercust))
str(custaggdata)
summary(custaggdata)


###Let's define churn as not buying a product in the last 5 months (July 2011 to Dec 2011)
###Note that our data runs from 2010-12-01 to 2011-12-09

custaggdata$churn <- ifelse(custaggdata$LastTransactionDate > '2011-10-19',"0", "1")
str(custaggdata)
summary(custaggdata)

#Factorize
custaggdata$churn <- as.factor(as.character(custaggdata$churn))
custaggdata$CustomerID <- as.factor(as.character(custaggdata$CustomerID))
