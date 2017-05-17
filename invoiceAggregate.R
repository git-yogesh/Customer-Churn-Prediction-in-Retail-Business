###Invoice Aggregations
print("Performing invoice aggregations...")
startTime <- proc.time()

#Total amount for each transaction
totalamount <-aggregate(x = dataset$Revenue, by = list(dataset$InvoiceNo), FUN = sum)
colnames (totalamount) <- c("InvoiceNo", "TotalAmount")

#The number of unique products ordered per transaction
unique <-aggregate(x = dataset$StockCode, by = list(dataset$InvoiceNo), FUN = length)
colnames (unique) <- c("InvoiceNo", "UniqueProducts")

#Dates for each transaction
transDate <-aggregate(x = dataset$InvoiceDate, by = list(dataset$InvoiceNo), FUN = mean)
colnames (transDate) <- c("InvoiceNo", "TransactionDate")

#Aggregation by max for customer ID
customerID <-aggregate(x = dataset$CustomerID, by = list(dataset$InvoiceNo), FUN = max)
colnames (customerID) <- c("InvoiceNo", "CustomerID")

#Combining...
invaggdata <- Reduce(function(x,y) merge(x,y,all=TRUE, by="InvoiceNo"), list(totalamount, unique, transDate, customerID))
str(invaggdata)
endTime <- proc.time() - startTime
invaggTime <- paste("Finished aggregating invoice data in", endTime[1], "seconds.")
print(invaggTime)
