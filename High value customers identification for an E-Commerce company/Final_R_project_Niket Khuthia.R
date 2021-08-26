

#########################
## My Final R Project ##
########################


##Installing necessary packages

install.packages("plyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("NbClust")
install.packages('cluster')

library(dplyr)
library(ggplot2)
library(scales)
library(NbClust)
library(cluster)

###  Importing dataset

ecom <- read.csv(file.choose()) # Read the CSV file from the data base
head(ecom) # view head of the file(to get to know the data headings)
str(ecom) #viewing the structure of the data

###  Data cleaning & Manuplation

# Removing unnecessary column "X"

ecom <- subset(ecom, select = -X) # column X removed from the data set

# Checking customer ID data

length(levels(as.factor(ecom$CustomerID))) # to know count of unique customers
length(unique(ecom$CustomerID)) # to know count of unique values in the data set
#Some invoices have missing CustomerID numbers because there is one unique observations extra.
#We will be removing any observations with missing ID numbers.

sum(is.na(ecom$CustomerID)) # to check no of missing values
ecom <- subset(ecom, !is.na(ecom$CustomerID))#Data manuplation
length(unique(ecom$CustomerID)) # to know count of unique values in the data set
#hence now the count is equal that means our customer data is cleaned and now we can go forward 

# Checking Quantity data

length(levels(as.factor(ecom$Quantity))) # to know count of unique customers
length(unique(ecom$Quantity)) # to know count of unique values in the data set
#there are no missing quantities because the data is equal to 3950 unique customers
#now checking for negative or zero quantaties

sum(ecom$Quantity <= 0) # to check wheather there are negative or zero Quantities
# we got 7533 entries with zero or negative data
ecom <- subset(ecom, !ecom$Quantity <= 0)#Data manuplation
length(unique(ecom$Quantity)) # to know count of unique values in the data set
#hence now the count is different, that means our Quantity data is cleaned and now we can go forward 

#checking the price data

length(unique(ecom$UnitPrice)) # to know count of unique values in the data set
sum(is.na(ecom$UnitPrice)) # to check no of missing values
# there are no missing values in price
sum(ecom$UnitPrice <= 0) # to check wheather there are negative or zero Pricies
#there are 24 such entries. hence deleting them
ecom <- subset(ecom, !ecom$UnitPrice <= 0)#Data manuplation
length(unique(ecom$UnitPrice)) # to know count of unique values in the data set
#hence now the count is different, that means our Price data is cleaned and now we can go forward 

# checking stock code data 

length(levels(as.factor(ecom$StockCode)))# to know count of unique stocks
sum(is.na(ecom$StockCode)) # to check no of missing values
# hence there are no missing values, Stockcode data is okay.

# Manuplating country data

length(levels(as.factor(ecom$Country)))# to know count of unique Countries
table(ecom$Country) # there are only 38 countries so viewing them by count will be better
# as we see that UK contains maximum no of customers, hence elimnating other countries customer data wont harm our data significantly.
#Hence we will only consider UK customers for our analysis.
ecom <- subset(ecom, Country == "United Kingdom")

#checking for return items
sum(grepl("C", ecom$InvoiceNo, fixed=TRUE)) # checking for any return items
#the sum is zero there are no return items


#########################################################################
## To Create new customer database -  # to estimate valuable customers ##
#########################################################################


### code for Receancy of customers ###

# converting data into std date format
ecom$InvoiceDate <- as.Date(ecom$InvoiceDate, "%d-%b-%y") #to convert from character to date format
#creating new column for recent transaction and calculating no of days since last transaction
ecom$Recency <- ((max(ecom$InvoiceDate) + 1) - ecom$InvoiceDate) # creating new table column for recency
range(ecom$Recency)# Viewing range of difference in days
# creating new data frame for customers data
customers <- as.data.frame(unique(ecom$CustomerID))# creating unique data frame of customer ID
names(customers) <- "CustomerID" # naming the Column name
#calculating for recency
# Obtain no of days since most recent purchase
Recency <-  aggregate(Recency ~ CustomerID, data=ecom, FUN=min)
#merging data by CustomerID
customers <- merge(customers, Recency, by="CustomerID", all=TRUE, sort=TRUE)#merging operation
remove(Recency) # removing the unnecssary data
#converting data into integer format
customers$Recency <- as.numeric(customers$Recency)
str(customers)

### Code for Frequency of customers ###

custinvoice <- subset(ecom, select = c("CustomerID","InvoiceNo")) #creating new data frame
custinvoice <- custinvoice[!duplicated(custinvoice), ]# removing dublicates
custinvoice <- arrange(custinvoice, CustomerID)# arranging by customerID
row.names(custinvoice) <- NULL #removing rownames
custinvoice$Frequency <- 1 # adding extra column of value 1 to find sum 
#making new dataframe of unique customers with frequency table
invoices <- aggregate(Frequency ~ CustomerID, data=custinvoice, FUN=sum)
# Add & merge no of invoices to customers data
customers <- merge(customers, invoices, by="CustomerID", all=TRUE, sort=TRUE)
remove(invoices, custinvoice) #removing unnecessary data
table(customers$Frequency)
# Removing customers who have not made any purchases in the past year or feilds with NA
customers <- subset(customers, Frequency > 0)
# now our customer data has frequency, as well as recency

### code for Monetary Value of Customers ###

# total Spending per transaction
ecom$Amount <- (ecom$Quantity*ecom$UnitPrice) # To creat amount table to know about the total spending in a transaction
# creating new dataset total sales to customer
totalsales <- aggregate(Amount ~ CustomerID, data=ecom, FUN=sum)
names(totalsales)[names(totalsales)=="Amount"] <- "Monetary"
# Add & Merge Monetary value to customers dataset
customers <- merge(customers, totalsales, by="CustomerID", all.x=TRUE, sort=TRUE) #merging operation
remove(totalsales) # removing unnecssary data
str(customers)
View(customers)


##########################################
### Clustering to find no of divisions ###
##########################################


# standardise data 

sapply(customers[,-1], mean) # Viewing Mean of customers data
sapply(customers[,-1], sd) # Viewing SD of customers data
scale_cust <- scale(customers[,-1]) # creating standerized data by scaling
round(apply(scale_cust, 2, mean)) # checking for standarized mean
apply(scale_cust, 2, sd) # checking for standarized SD

# Hiearchical Clustering

dis_mat <- dist(scale_cust, method = 'euclidean')
hclus <- hclust(dis_mat, method = 'ward.D') 

# dendogram

plot(hclus, labels = as.character(customers$CustomerID),
     main = 'Hierarchical Clustering')

nb<- NbClust(scale_cust, distance = 'euclidean',  # NBCLUST
             method = 'ward.D',
             min.nc = 2, max.nc = 5)

#considering maximum value = 5 clusters

# concludes based on 3920 indices

plot(hclus, labels = as.character(customers$CustomerID),
     main = 'Hierarchical Clustering')
rect.hclust(hclus, k = 2, border = 'red')
rect.hclust(hclus, k = 3, border = 'blue')
rect.hclust(hclus, k = 4, border = 'green')
rect.hclust(hclus, k = 5, border = 'magenta')


#hence going with majority and considering 2 clusters 
# Premium customers # Silver Customers 

# cluster profile 

customers['cust_hc'] <- cutree(hclus, k = 2) # get cluster labels 

hclus_prof<- customers%>%
  dplyr::select(-CustomerID)%>%
  group_by(cust_hc)%>%
  summarise_all(mean)%>%
  mutate(Freq = as.vector(table(customers$cust_hc)))%>%
  dplyr::select(cust_hc,Freq, Recency, Frequency, Monetary)%>%
  data.frame()
View(hclus_prof)

# Also comparing the above dataset by kmeans clustring

nb1 <- NbClust(scale_cust, distance = 'euclidean',
             method = 'kmeans', min.nc = 2, max.nc = 5)

# again considering maximum value = 5 clusters

#result is 3 clusters from the majority the three classifications may be below
#Platinum # Gold # Silver
# perform kmeans
kmm <- kmeans(scale_cust, centers = 3)
kmm

# cluster profile 

customers['cust_kmm'] <- kmm$cluster

kmm_prof<- customers %>%
  dplyr::select(-CustomerID)%>%
  group_by(cust_kmm)%>%
  summarise_all(mean)%>%
  mutate(Freq = as.vector(table(customers$cust_kmm)))%>%
  dplyr::select(cust_kmm,Freq, Recency, Frequency, Monetary)%>%
  data.frame()

View(kmm_prof)
View(customers)

# Visualize the clusters

cluster::clusplot(scale_cust, kmm$cluster, main = 'K Means Clustering')
cluster::clusplot(scale_cust, customers$cust_hc, main = 'H Clustering')

## to know propotion of the data from H clustering

round(prop.table(table(customers$cust_hc)), 2) 
sum(customers$cust_hc == 1)

# there are 2840 top Customers according to H clustering
# this indicates that 
#premium customers are approx 72 % of the total customers
#silver customers are approx 28 % of the total customers

## to know propotion of the data from K Means Clustering

round(prop.table(table(customers$cust_kmm)), 2) 
sum(customers$cust_kmm == 2)

# there are 23 top Customers according to Kmeans clustering
# this indicates that 
#pletinum customers are approx 1 % of the total customers
#gold customers are approx 74 % of the total customers
#Silver customers are approx 25 % of the total customers

remove(dis_mat) # removing the unwanted data


###############################
## Visualization of the data ##
###############################


# Original scale

customers$cust_hc <- as.factor(customers$cust_hc) #converting cluster into factor so that it can be readebale by ggplot
customers$cust_kmm <- as.factor(customers$cust_kmm) #converting cluster into factor so that it can be readebale by ggplot

## sactter plot for original values using ggplot tool

# Scatter plot for Kmeans clustering

og_scatter_k <- ggplot(customers, aes(x = Frequency, y = Monetary))
og_scatter_k <- og_scatter_k + geom_point(aes(colour = Recency, shape = cust_kmm))
og_scatter_k <- og_scatter_k + scale_shape_discrete(name = "K-means")
og_scatter_k <- og_scatter_k + scale_colour_gradient(name="Recency\n(No of Days since \nLast Purchase)")
og_scatter_k <- og_scatter_k + xlab("Frequency\n(Number of Purchases)")
og_scatter_k <- og_scatter_k + ylab("Monetary\n(total Sales per customer)")
og_scatter_k <- og_scatter_k + ggtitle("Original scatter plot for K-means")
og_scatter_k

# Scatter plot for Hierarchical clustering

og_scatter_h <- ggplot(customers, aes(x = Frequency, y = Monetary))
og_scatter_h <- og_scatter_h + geom_point(aes(colour = Recency, shape = cust_hc))
og_scatter_h <- og_scatter_h + scale_shape_discrete(name = "Hierarchical")
og_scatter_h <- og_scatter_h + scale_colour_gradient(name="Recency\n(No of Days since \nLast Purchase)")
og_scatter_h <- og_scatter_h + xlab("Frequency\n(Number of Purchases)")
og_scatter_h <- og_scatter_h + ylab("Monetary\n(total Sales per customer)")
og_scatter_h <- og_scatter_h + ggtitle("Original scatter plot for Hierarchical Clustering")
og_scatter_h

# we cant understand the dat abecause it is concenterated on one side and dispersed on the other
#This first graph uses the variable is original metrics and is almost completely uninterpretable.
#There is a clump of data points in the lower left-hand corner of the plot, and then a few outliers.
# hence we are applying for log transformation

## Preprocessing the data ##

# Log-transformation 
customers$Recency.log <- log(customers$Recency)
customers$Frequency.log <- log(customers$Frequency)
customers$Monetary.log <- log(customers$Monetary)

## sactter plot for transformed values using ggplot tool

# Scatter plot for Kmeans clustering

log_scatter_k <- ggplot(customers, aes(x = Frequency.log, y = Monetary.log))
log_scatter_k <- log_scatter_k + geom_point(aes(colour = Recency.log, shape = cust_kmm))
log_scatter_k <- log_scatter_k + scale_shape_discrete(name = "K-means")
log_scatter_k <- log_scatter_k + scale_colour_gradient(name="Recency\n(No of Days since \nLast Purchase)")
log_scatter_k <- log_scatter_k + xlab("Frequency\n(Number of Purchases)")
log_scatter_k <- log_scatter_k + ylab("Monetary\n(total Sales per customer)")
log_scatter_k <- log_scatter_k + ggtitle("Transformed scatter plot for K-means")
log_scatter_k

# Scatter plot for Hierarchical clustering

log_scatter_h <- ggplot(customers, aes(x = Frequency.log, y = Monetary.log))
log_scatter_h <- log_scatter_h + geom_point(aes(colour = Recency.log, shape = cust_hc))
log_scatter_h <- log_scatter_h + scale_shape_discrete(name = "Hierarchical")
log_scatter_h <- log_scatter_h + scale_colour_gradient(name="Recency\n(No of Days since \nLast Purchase)")
log_scatter_h <- log_scatter_h + xlab("Frequency\n(Number of Purchases)")
log_scatter_h <- log_scatter_h + ylab("Monetary\n(total Sales per customer)")
log_scatter_h <- log_scatter_h + ggtitle("Transformed scatter plot for Hierarchical Clustering")
log_scatter_h


#################
### Thank you ###
#################


















