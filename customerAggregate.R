###Customer Aggregations
print("Performing customer aggregations...")
startTime <- proc.time()

#(This shows we have complete cases for all transactions for only 3100 customers)
custfreq <-aggregate(x = invaggdata$InvoiceNo, by = list(invaggdata$CustomerID), FUN = length)
colnames (custfreq) <- c("CustomerID", "Frequency")

#Last date data for each customer
lastdate <-aggregate(x = invaggdata$TransactionDate, by = list(invaggdata$CustomerID), FUN = max)
colnames (lastdate) <- c("CustomerID", "LastTransactionDate")

#Average amount for each customer's transactions
avgamount <-aggregate(x = invaggdata$TotalAmount, by = list(invaggdata$CustomerID), FUN = mean)
colnames (avgamount) <- c("CustomerID", "AvgAmount")

#Total amount for each customer's transactions
totalamtpercust <-aggregate(x = invaggdata$TotalAmount, by = list(invaggdata$CustomerID), FUN = sum)
colnames (totalamtpercust) <- c("CustomerID", "TotalAmount")

#Combining...
custaggdata <- Reduce(function(x,y) merge(x,y,all=TRUE, by="CustomerID"), list(custfreq, lastdate, avgamount, totalamtpercust))

###Let's define churn as not buying a product in the last 5 months (July 2011 to Dec 2011)
###Note that our data runs from 2010-12-01 to 2011-12-09

custaggdata$churn <- ifelse(custaggdata$LastTransactionDate > '2011-09-1',"0", "1")
str(custaggdata)

#Factorize
custaggdata$churn <- as.factor(as.character(custaggdata$churn))
custaggdata$CustomerID <- as.factor(as.character(custaggdata$CustomerID))

endTime <- proc.time() - startTime
custaggTime <- paste("Finished aggregating customer data in", endTime[1], "seconds.")
print(custaggTime)