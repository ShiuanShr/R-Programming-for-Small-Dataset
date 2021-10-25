#environment setting( Setting WD and  importing package )
getwd()
dir()
path <- 'C:\\Users\\LeoShr\\R_file'
setwd(path)
getwd()
data <- read.csv('C:\\Users\\LeoShr\\R_file\\online_retail.csv')
#install.packages('dplyr')
#install.packages('comprehenr')
library('comprehenr')
library('dplyr')

#Replace the column name 
res_colname <- names(data)[2:ncol(data)]
names(data) <- c('invoice_No',res_colname)
head(data)


colnames(data)
#(a)
summary(data)
str(data)
#turn the data$InvoiceDat to the type of date or time

#data_1 <- as.Date(data$InvoiceDate, format = "%m/%d/%y %H:%M")

data_1 <- strptime(x = data$InvoiceDate, format = "%m/%d/%y %H:%M")
head(data_1)
class(data_1) #[1] "POSIXlt" "POSIXt" 

data$InvoiceDate <- data_1
head(data)
str(data)
# r1 <- as.Date(data$InvoiceDate) <= '2011/08/31'
# tail(r1)
# r2 <- as.Date(data$InvoiceDate)>='2011/07/01'
# tail(r2)
# period_need <- r1 & r2
# sum(as.numeric(period_need)) #having 74802 transaction record
# data <- cbind(data,period_need)
# head(data)

data_required <- filter(data,InvoiceDate< '2011/09/01' &InvoiceDate >='2011/07/01')
head(data_required)
#data_required$period_need <- NULL
nrow(data_required) #[1] 74802


names(data_required)
data_required <- distinct(data_required, invoice_No,.keep_all = TRUE)
nrow(data_required) #3664 unique transaction 

#?unique ??
?duplicated()

#####################(C)#########
#####################(1)compute the mean of Quantity and UnitPrice

attach(data_required)

getmean <- function(x){
    temp <- 0
    for (i in c(1:length(x)) )  temp<- temp+ x[i]
    mean_ <- temp/length(x)
    return(mean_)
}

Quantity_mean <- getmean(Quantity)
UniPrice_mean <- getmean(UnitPrice)

sprintf('Quantity_mean:%f ; UniPrice_mean:%f',Quantity_mean,UniPrice_mean)
#answer pf (c)-1

# #check it correct or not (if it's error free, it means pass)
# stopifnot(Quantity_mean ==mean(data_required$Quantity))
# #ok
# 
# #check it correct or not (if it's error free, it means pass)
# 
# stopifnot(UniPrice_mean ==mean(data_required$UnitPrice))
# 
# mean(data_required$UnitPrice)#[1] 20.01594
# 
# mean_UniPrice #[1] 20.01594
# 
# #FALSE why??? Difference: [1] 3.765876e-13
# mean(data_required$UnitPrice) == mean_UniPrice #FALSE why???
# #asking teacher 
# 
# 
# #using apply to get the mean
# new <- data.frame(Quantity,UnitPrice)
# apply(new,2,mean)

#####################(2) determine the types of each column


str(data_required) 
length(names(data_required)) #8 
# ans :
# $ invoice_No : chr
# $ StockCode  : chr 
# $ Description: chr 
# $ Quantity   : int
# $ InvoiceDate:POSIXlt
# $ UnitPrice  : num
# $ CustomerID : int
# $ Country    : chr

#####################(3) compute the number of unique values in each column.#########
#compute the number of  unique value in each column

# (test-1) single column transformation edition (skipped)
# unique_v<- function(n){
#     unique_no<-  for (item_num in c(1:length(n)))length(unique(unlist(data_required[n[item_num]])))
#     print (unique_no)}

x <- names(data_required);x
for (val in x) {
    unique_no <- length(unique(unlist(data_required[val])))
    # print(typeof(unique_no))
    # print(typeof(val))
    t <- sprintf('%s: %d',val,unique_no)
    print(t)
}

# (d) Subset the data for which the transactions took place in the U.K., Netherlands, and Australia.

#testing
#if (any(data_required$Country == 'Australia')) print('T')
d_subset <- subset(data_required,Country=="United Kingdom"|Country=="United Kingdom"|Country=="Australia"|Country=="Netherlands") 

# unique(d_subset[['Country']])

d_subset


#  Using the subset of data,
#(4) report the average and standard deviation (round them up to 3decimal points) of the UnitPrice as well as 
mean_0f_UnitPrice_d_subset <- round(mean(d_subset$UnitPrice),3)
mean_0f_UnitPrice_d_subset #[1]ans: mean:  20.593 
sd_0f_UnitPrice_d_subset <- round(sd(d_subset$UnitPrice),3)
sd_0f_UnitPrice_d_subset #standard deviation: [1] 435.919

#(5) the number of 'unique' transactions made in these countries. 
country_list <- unique(d_subset[['Country']]);country_list

unique_trans <- function(){
    kk <- c()
    for (countr in country_list){
        temp_set<-subset(d_subset,Country ==countr)
        unique_no <- length(unique(unlist(temp_set$invoice_No)))
        #print(unique_no)
        kk <- c(kk,unique_no)
        
    }
    return(kk)
}

uni_transation_seperately <- unique_trans()
names(uni_transation_seperately)<- country_list
uni_transation_seperately
#result:
# United Kingdom      Australia    Netherlands 
# 3310             11             11 

uni_transation_totally <- sum(uni_transation_seperately)
uni_transation_totally#[1] 3332


#(6) How many customers residing in these countries made transactions in July and August of 2011?
#make sure the time restrict is matched

July_Aug_d_subset <- filter(d_subset,InvoiceDate< '2011/09/01' &InvoiceDate >='2011/07/01') 
nrow(July_Aug_d_subset) #we have 3332 unique transaction record(by invoice_No)

#we should remove the rows which CustomerID is NA first\
July_Aug_d_subset_without_NA_CID <- d_subset[!(is.na(d_subset$CustomerID)),]

July_Aug_d_subset_without_NA_CID

#and then we count the unique customer by its own CustomerID
answer.of.q6 <- distinct(July_Aug_d_subset_without_NA_CID, CustomerID,.keep_all = TRUE)
nrow(answer.of.q6) #1379

#######################(e)
# Do we see any customers who made a refund? 
ans_e <- any(startsWith(as.character(data_required[['invoice_No']]),"C"))
ans_e #ans
####ans_e: TRUE, we do

#(7) If we do, how many customers made a refund (make sure to exclude the observations without the CustomerID (CID))?
####1. remove the customer without coustomerID
data_required_with_CID <- data_required[!(is.na(data_required$CustomerID)),] #dropout the customer without CID
data_required_with_CID
ans_7 <- sum(startsWith(as.character(data_required_with_CID[['invoice_No']]),"C"))  
ans_7 #[1] 525

# Assign the IDs of the customers who made at least one refund during the period into a vector called cust_refund.
#### 1. using the subset with CID only
#### 2.  build up a list with bool value( TRUE <- Refund record, FALSE <- Normal record)
problem_client <- startsWith(as.character(data_required_with_CID[['invoice_No']]),"C")
length(problem_client) == nrow(data_required_with_CID) #TRUE
temp <- c()
cust_refund <- c()
for(i in c(1:nrow(data_required_with_CID))){
    if (problem_client[i] == TRUE) {
        temp <- c(temp,data_required_with_CID$invoice_No[i])
    }
}
cust_refund <- unique(temp) #remove the duplicated frequent refund client's ID
cust_refund<- as.vector(cust_refund) #depends on the requirement of the question, 'assign them into a vector'
cust_refund #ans
#length(cust_refund) #525

# (f) Some customers made purchases without logging into the e-commerce site. This would create
# records of transactions for which the CustomerID is missing (i.e., NA). These transactions
# cannot be traced since we do not know who ordered the products. Create a variable called
# Sales by multiplying the Quantity and the UnitPrice. 8) Then, calculate the total sales amount
# for those that are missing the CustomerID. 

data_required['Sales'] <- data_required$Quantity*data_required$UnitPrice
tail(data_required)

#drop out the clients with customer ID
witoutht_logging <- data_required[(is.na(data_required$CustomerID)),]

# calculate the total amount $ them spent
sum(witoutht_logging$Sales) 
#[1] -36051.97, answer,(pretty surprizing that people who don't log in are more likely to refund I guessed)

#9) How many transactions were made without the customers logging into the e-commerce site?

#asking teacher: are the unique invoice counted as a single transaction or every purchase is considerd as a transaction
#[1] 527 for every purchase considerd as a transaction
per.purchase.as.trans <- nrow(witoutht_logging)
per.purchase.as.trans
#[1] 527 for every unique invoice_No   as a transaction
per.invoiceNo.as.trans <- nrow(distinct(witoutht_logging, invoice_No,.keep_all = TRUE))
per.invoiceNo.as.trans

# ########(g) 
#(EC1) Create a variable containing the monthly aggregate spending for each customer. 

length(unique(StockCode))#[1] 3137 -> [1] 1171
#using the client data with customerID
data_required_with_CID['Sales'] <- data_required_with_CID$Quantity*data_required_with_CID$UnitPrice
t1 <- data_required_with_CID[,c(7,5,9)]
head(t1)

#find the key to design new table
attach(t1)
length(unique(CustomerID)) #[1] 3137 -> [1] 1540 (unique)
length(unique(invoice_No)) # [1] 3137 ->[1] 3137 ->  all unique take it as a key
length(unique(InvoiceDate))  #[1] 3137 -> [1]52 
#get the max date and min date in my dataset(t1)
max(as.numeric(t1$InvoiceDate)) #[1] 15217 Max "2011-07-20"
min(as.numeric(t1$InvoiceDate)) #[1] 15156 min "2011-07-01"#



# t1 <- data_required_with_CID[,c(7,5,9)]
# 
# r_num <- 3137
# temp_list_for_df <- c() #empty list
# for(x in c(1:r_num)){
#     character <- t1[x,1]
#     S <- data.frame()
#     for(i in c(1:r_num)){
#         if(character == t1[i,1] ){
#             new <- t1[i,]
#             print(new)
#             S <-rbind(S,new)
#         }
#     }
#     attach(S)
#     bymonth <-aggregate(Sales~month(InvoiceDate), data=S,FUN=sum)
#     detach(S)
#     temp_list_for_df <- c(temp_list_for_df,bymonth)
# }

head(temp_list_for_df)

#if it works, temp_list_for_df should have 3664 df, 
 #which has 2 columns showing the July/ Aug spending separately
    
mon <- as.integer(as.Date("2011-01-01"))
feb <- as.integer(as.Date("2011-02-01"))


#
distinct(data_required_with_CID, CustomerID ,.keep_all = TRUE)

##
uni_customer_aggregated_shopping <- distinct(data_required_with_CID, CustomerID ,.keep_all = TRUE)
data_required['Sales'] <- data_required$Quantity*data_required$UnitPrice



head(data_required_with_CID)
### 

# bymonth <-aggregate(Sales~month(InvoiceDate), data=t1,FUN=sum)
# bymonth


#(EC2)
# Then, report the IDs and the monthly purchase amount of the five customers who have spent the most money in July 2011.


#rm(list=ls())
#q()
