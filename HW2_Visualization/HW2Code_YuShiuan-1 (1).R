## HW2
install.packages('lubridate')
install.packages('ggplot2')
install.packages('data.table')

require("dplyr")
require(data.table)
require('lubridate')
library(stringr)
require('dplyr')
require('ggplot2')
getwd()

#[a] import the data and complete the classes change

data_1999 <- fread('https://bit.ly/3c4AHbL',colClasses=list(character=1:5, numeric=6:12))
data_2012 <- fread('https://bit.ly/3nZicL2',colClasses=list(character=1:5, numeric=6:12))
#to know each classes of the column 
lapply(data_1999, class)
lapply(data_2012, class)

#[b] Take a look at the 1999 data by 
#(1) printing out the dimensions and 

dim(data_1999)#[1] 117421     12
#(2) the first 3 rows.
head(data_1999,3)

#then divide the numbers by the total number of observations in the data to calculate the proportions.
# [3-ii] (4) What is the percentage of the PM2.5 observations that are missing (round up to 3 decimal places)?

#[3]Using the 1999 data, print the summary statistics of the variable with summary().
summary(data_1999$Sample.Value)
?table()
#[3-i] Compute the number of NAs using table() and is.na(), 
temp <- table(is.na(data_1999$Sample.Value));temp['TRUE'] #13217 
theNAData <-temp[2]/sum(temp)#0.1125608 
theNAData

# [3-ii] 
temp <- as.character(round(theNAData*100,3))
sprintf("NA portion :%s percent",temp)

# [d] Bind the 1999 data and 2012 data and assign the aggregated data to an object called ‘pm’. 
#Then, subset the years from the Date variable and convert it into a factor variable called ‘year’.

pm <- rbind(data_1999,data_2012)
pm <- mutate(pm,year = as.factor(year(ymd(pm$Date))))#取代原有的year值，用factor塞進去
class(pm$year)#確認是factor
str(pm)
#[e] Next, rename the Sample.Value variable to PM which better expresses the values stored in the variable.

pm <- pm %>% rename(PM = Sample.Value)
colnames(pm)


##################################################################
####part 2 Data Exploration with Visualization using ggplot2

# [a] First, for better visibility and reproducibility, 
#(5) set the seed at 2021 and draw 1,000 randomly
# selected samples from the data (i.e., pm) using the sampling function in dplyr package.

set.seed(2021)
sub_sample <- sample_n(pm,1000) #Using sample_n function  in dplyr package

# [b] Then, create boxplots of all monitor values in 1999 and 2012 using the randomly sampled data
# as shown below. [6] Make sure to take the log of the PM values (with base 2; i.e., binary algorithm) to adjust for the skewness in the data, 
#[7] label the title, x-axis & y-axis, and (8) use
# the base white theme to replicate the graphics.
#(8) use the base white theme to replicate the graphics. As above.
ggplot(sub_sample, aes(x = year, y = log2(PM), color = year))+
    geom_boxplot()+
    labs(title = "Boxplot of PM values in 1999 and 2012", x = 'Year',y= 'Log2 PM2.5')+
    theme_bw()


# [c] (9) Describe what you observe in terms of 
# the means and variances of the observations in 1999 and 2012? 


#install.packages('hablar')
# library(hablar)
#mean and variance
mean(data_1999$Sample.Value, na.rm = TRUE)#[1] 13.7381

mean(data_2012$Sample.Value, na.rm = TRUE)#[1] 9.139924

var(data_1999$Sample.Value, na.rm = TRUE)#[1] 88.54687

var(data_2012$Sample.Value, na.rm = TRUE)#[1] 73.21078

# log version
mean(log2(data_1999$Sample.Value), na.rm = TRUE)#[1] -Inf

mean(log2(data_2012$Sample.Value), na.rm = TRUE)#[1] -Inf


var(log2(data_1999$Sample.Value), na.rm = TRUE)#[1] NaN

var(log2(data_2012$Sample.Value), na.rm = TRUE)#[1] NaN

#若要進行LOG，必須先去除負值，使用filter設定條件<0，題目沒問不多贅述

no_na_1999 <- na.omit(exe1999$PM);no_na_1999
no_na_1999_after_log <- log2(no_na_1999);no_na_1999_after_log
var(no_na_1999_after_log)#[1] var of 1999 PM2.5 : 0.9882329
mean(no_na_1999_after_log)#the mean of 1999 PM 2.5: 3.494759

#Ans  
#please referr to the word file


# [d] 
#(10) Subset the data to include only the observations from New York (i.e., State.Code == 36) 
#and only include the County.Code and the Site.ID (i.e. monitor number)
#variables using filter(), select(), and unique().

ny_data <- filter(pm, pm$State.Code == 36); ny_data
subset_ny_data  <- select(ny_data, County.Code:Site.ID);subset_ny_data

colnames(subset_ny_data)
count(unique(subset_ny_data))

#Changes in PM levels at an individual monitor: 
#[e](11) Create a new variable called Site.Code by using paste() with “.” 

colnames(ny_data) #沒有Site.Code欄位
ny_data <- mutate(ny_data,Site.Code = paste(ny_data$County.Code,ny_data$Site.ID, sep = '.') )
head(ny_data)

a = 031
b = 003 
paste(a,b,sep = '.')

# [f] (12) Find the intersection of the sites  between 1999 and 2012

#這邊使用的是單純只有紐約的探測器
head(ny_data)#使用ny_data，看看我們資料樣子

ny_1999.siteId <- filter(ny_data, ny_data$year == 1999)
ny_2012.siteId <- filter(ny_data, ny_data$year == 2012)
monitors <- unique(intersect(ny_1999.siteId$Site.ID, ny_2012.siteId$Site.ID))
monitors

# > intersect_of_ny_data_question12
#  [1] "0005" "0012" "0080" "0011" "0002" "1007" "0003" "2008" "1015" "0055"

# (13) Write a block of code to identify the monitor 
# in New York State that had the most data

#使用ny city原本資料來filter
ny_monitors_data <-  filter(ny_data, ny_data$Site.ID %in% monitors)
ny_monitors_data %>%
    group_by(Site.Code) %>%
    summarise(data_count = length(PM)) %>%
    arrange(desc(data_count))

# (h) (14) Subset the data (i.e., pm) that contains observations from the monitor 
# we just identified (State.Code = 36 & County.Code = 101 & Site.ID = 0003) 
#and assign the subset data to an obj. called ‘pmsub’.
pmsub <- subset(pm,State.Code== '36' & County.Code == '101' & Site.ID == '0003' )
pmsub

str(pmsub)
str(pm_with_Site.Code)

# (i) Next, using the lubridate package, 
# (15) convert the Date variable into a date obj. and then create a variable 
#called ‘yday’ containing info. on day of the year using yday(). 
pmsub$yday <- yday(ymd(pmsub$Date))
head(pmsub)
str(pmsub)

# Draw a scatter plot by mapping the year-day variable on the x-axis, PM2.5 level on the y-axis separately for 1999 and 2012. 
# (16) Make sure to label the x-axis, 
# (17) separate the plots using the facet function and
#(18) use the base white theme to replicate the graphics shown below. 
head(pmsub)

ggplot(pmsub, aes(yday, PM)) +
    geom_point() +
    facet_wrap(. ~ year) +
    labs(x = "Day of the Year",  y = 'PM') +
    theme_bw()
