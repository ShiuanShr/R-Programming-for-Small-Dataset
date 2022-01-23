rm(list = ls())
require(ggplot2)
library(data.table)
library(dplyr)
require(tidyverse)
install.packages("stargazer")
require(stargazer)
Data <- read.csv("C:\\Users\\LeoShr\\p_space\\NTHU\\PBA\\HW3\\banksalary.csv")
View(Data)
summary(Data) 
str(Data)

#---------------------處理salary欄位-----------------------------------
#Remove "$"
Data_2 <- mutate(Data, Salary =substring(Data$Salary, 2, )) ; head(Data_2)

#Remove','
nocomma<-function(x){
    as.integer(gsub('[,]', '' ,x))
}
temp <- apply(Data_2[9],2,nocomma)
Data_2$Salary <-temp
head(Data_2$Salary)

#------transform EducLev into  factor [1,5] -----------------------------------------------
Data_2$EducLev <- factor(Data_2$EducLev,levels = c("1", "2", "3",'4',"5"))
#-----transform JobGrade into factor[1,6]-----------------
Data_2$JobGrade <- factor(Data_2$JobGrade,levels = c("1", "2", "3",'4',"5","6"))
str(Data_2)
#-----transform PCJob into factor---------------------------------
Data_2$PCJob <- factor(Data_2$PCJob)
#-----transform Gender into factor---------------------------------
Data_2$Gender <- factor(Data_2$Gender)
#---------------------------------------------------------------------
##Q1: (1)Import the csv file (2)present the descriptive statistics for both categorical data and numerical data

# the descriptive statistics for both categorical data and numerical data

#Notice: The categorical variable shall be factor if it's represent by the ranking.
#And the order of the rank shall be set. 
#Therefore, the 'summary' function will  show the counting number of the categorical variable in  summary correctly.
summary(Data_2) #Q1 answer

#-----------------------------------------------------------
# Q2) A plaintiff’s lawyer claims that there is a significant difference in 
# average salary between female employees and male employees. 
# As an analyst for the plaintiff, how would you support this claim?
# Use a t-test and explain the results as well as your interpretation.


##subset our data into 2 group: male & female
Male_Data <- subset(Data_2,Gender=="Male")
Female_Data <- subset(Data_2,Gender=="Female") 
#--------------male(To obtain the basic understanding of male salary)
mean(Male_Data$Salary)
#[1] 45505.44
sd(Male_Data$Salary, na.rm = T)
#[1] 15843.22
#-------------female (To obtain the basic understanding of Female salary)
mean(Female_Data$Salary)
#[1] 37209.93
sd(Female_Data$Salary, na.rm = T)
#[1] 6710.867

#T- test: 
t.test(Male_Data$Salary,Female_Data$Salary)
#'var.equal = FALSE' is  default setting

#t = 4.141, df = 78.898, p-value = 8.604e-05 
# Conclusion: p value <0.05 hence, mean of male salary (45505.44 )>mean of female salary (37209.93) having significant variance (p<0.001)

#Notes: Default pattern  of t.test (Welch) as below: 
# t.test(x, y = NULL,
#        alternative = c("two.sided", "less", "greater"),
#        mu = 0, paired = FALSE, var.equal = FALSE,
#        conf.level = 0.95, ...)
# Obviously, var.equal = False, therefore, the level of freedom will be re-calculated
#註記: Welch 是重算degree of freedom 不等方差 不用比較var 是否一樣

#---------------補充Shapiro-Wilk 檢定-------------------------------------------------       
# t 檢定的前提是資料必須是常態分佈的，所以要先使用 Shapiro-Wilk 檢定，檢查常態性假設是否成立，除非sample 數量夠大：

#shapiro.test-----------Male_Data$Salary
shapiro.test(Male_Data$Salary)
# result of Male_Data$Salary: 
# W = 0.83295, p-value = 2.744e-07<< 0.05 ，reject H0，it represents it doesn't follow Normal Distribution.
#shapiro.test---------Female shapiro.test
shapiro.test(Female_Data$Salary)
 
# result:Female_Data$Salary
# W = 0.7792,  4.814e-07 < 0.05，reject H0，it represents it doesn't follow Normal Distribution.

#And going further, we shall test the variance of this two sample by Ansari-Bradley Test.

ansari.test(Salary ~ Gender, data = Data_2)

#And the parametric test can not be used under this condition, 
#We shall use Median and IQR to replace means and variance.

#--------------KS or Wilcox  testing------------------------------------------
# non- parametric Maann- Whitney U Test
wilcox.test(Male_Data$Salary, Female_Data$Salary)

# #Kindly refer to markdown(word file) 
# #--------------male
# median(Male_Data$Salary)
# #[1] 42500
# IQR(Male_Data$Salary, na.rm = T)
# #[1] 14250
# #--------------female
# median(Female_Data$Salary)
# #[1] 35450
# IQR(Female_Data$Salary, na.rm = T)
# #[1] 8975


#-----------------------------------------------------
#visualization
install.packages("GGally")
require(GGally)
str(Data_2)
GGally::ggpairs(Data_2[, c(9, 2:8)]) 
#------------------------------------------------------------------
#Q3: dummy variable change EducLev, JobGrade, Gender, and PCJob1
#We have 2 condition
#Condition1: Boolean mapping : 'Gender ' , 'PCJob'
#Use  boolean values as dummy variable to replace original variable
head(Data_2)
#Let Gender let male ==1,else 0
Data_3 <- Data_2 %>% mutate(Gender = ifelse(Gender == "Male",1,0))
Data_3$Gender <- factor(Data_3$Gender,levels = c(0,1)) #set the order

head(Data_3)
#Let PCJob let yes ==1,else 0
Data_3 <- Data_3 %>% mutate(PCJob = ifelse(PCJob == "Yes", 1, 0))
Data_3$PCJob <- factor(Data_3$PCJob,levels = c(0,1))
str(Data_3)
#Condition2. multiple different objects 
# EducLev, range[1,4] , setting baseline  = EducLev1 
#Dummy variable: 'EducLev2' 'EducLev3' 'EducLev4'
Data4 <- Data_3
str(Data4)


Data4<-Data4 %>% 
    mutate('EducLev2' = ifelse(EducLev  == 2,1,0))%>% 
    mutate('EducLev3' = ifelse(EducLev  == 3,1,0))%>%
    mutate('EducLev4' = ifelse(EducLev  == 4,1,0))%>%
    mutate('EducLev5' = ifelse(EducLev  == 5,1,0))
#turn them into factor
Data4$EducLev2 = factor(Data4$EducLev2,levels = c(0,1))
Data4$EducLev3 = factor(Data4$EducLev3,levels = c(0,1))
Data4$EducLev4 = factor(Data4$EducLev4,levels = c(0,1))
Data4$EducLev5 = factor(Data4$EducLev5,levels = c(0,1))


Data4 <- Data4[,-2] #eliminate the column EducLev
head(Data4)

#JobGrade (range[1,6], represents it by 5 column)
#Baseline: JobGrade1
#eg, 'JobGrade2', 'JobGrade3', 'JobGrade4', 'JobGrade5', 'JobGrade6'
Data4<-Data4 %>% 
    mutate('JobGrade2' = ifelse(JobGrade   == 2,1,0))%>% 
    mutate('JobGrade3' = ifelse(JobGrade   == 3,1,0))%>%
    mutate('JobGrade4' = ifelse(JobGrade   == 4,1,0))%>%
    mutate('JobGrade5' = ifelse(JobGrade   == 5,1,0))%>%
    mutate('JobGrade6' = ifelse(JobGrade   == 6,1,0))
#turn them into factor
Data4$JobGrade2 = factor(Data4$JobGrade2)
Data4$JobGrade3 = factor(Data4$JobGrade3)
Data4$JobGrade4 = factor(Data4$JobGrade4)
Data4$JobGrade5 = factor(Data4$JobGrade5)
Data4$JobGrade6 = factor(Data4$JobGrade6)

#eliminate the orginal  column JobGrade 
Data4 <- Data4[,-2] 
head(Data4)




#Q4.(1) Estimate a multiple regression model to strengthen/bolster the plaintiff’s justification, then write a report explaining your results.
# (2) Also discuss about: what R-squared is and what it means, what the meaning of the t- values and the coefficients are (or estimates).
#We adopt Data4(with dummy variables)
sum(is.na(Data4)) #0 there is no NA

#linear multiple variable regression analysis 線性多變數迴歸分析 : 
#we choose to use Data_3, which is the original df having categorical variable as factor. 
#And besides PCJob & Gender, other category variable has represented by numeric with certained  order.
# With the concept of ranking, we can use them directly
#Avoid unfolding the feature into dummy variables can retain the density of the matrix. 
# head(Data_3)
# regression_data <-Data_3
# 
# model <- lm(formula= Salary ~ EducLev+ JobGrade+ YrsExper+ Age+ Gender+ YrsPrior+PCJob,
#             data=regression_data)
# summary(model)
# str(model)


head(Data4)
regression_data <-Data4
head(Data4)
model <- lm(formula= Salary ~ Gender + Age +YrsExper +YrsPrior +
                PCJob + 
                EducLev2+
                EducLev3 +
                EducLev4 +
                EducLev5+
                JobGrade2 +
                JobGrade3 +
                JobGrade4 +
                JobGrade5 +
                JobGrade6,
            data=regression_data)
summary(model)

# Q5. Do these data provide evidence that there is discrimination against female employees in terms
# of salary?

#yes, kindly refer to the markdown. 
#The p- value of the gender is smaller than 0.05. Therefore, we reject Ho, accept H1.



# Q_extra
# a. You may get more interesting results to talk about by including 交互作用項（interaction term）in your
# regression model. Explain what an interaction term is, how we can estimate a regression
# model with interaction terms and how we could interpret the results.

#Ans: Kindly refer to word/pdf file (Markdown)
#Using Data4 with dummy variable


# b. How would you determine whether the interaction terms contribute in a meaningful way to
# the explanatory power of your estimation model?
#Ans: 

#I would like to focus on the impact of JobGrade rank of PCjob employee for both sex.
#Hence, I only remain the JobGrade(1-5) and  Gender, which supposed to increase the explanatory power. 
#Furthermore, I want to add the interaction term into the model to observe the  male worker(1)  in different JobGrade.
#Normally, it should be the male  with higher rank having the higher salary.
#Using Data4 with dummy variable

str(Data4)
extra_model_2 <- lm(formula= Salary ~ JobGrade2 +JobGrade3 +JobGrade4 +JobGrade5 + JobGrade6 +Gender + 
                        Gender*JobGrade2+
                        Gender*JobGrade3+
                        Gender*JobGrade4+
                        Gender*JobGrade5+
                        Gender*JobGrade6, data=Data4)

summary(extra_model_2)


# #---------------------------------testing (Male engineer with different Job grading)
# head(Data4)
# extra_model_2 <- lm(formula= Salary ~ PCJob +JobGrade2 +JobGrade3 +JobGrade4 +JobGrade5 + JobGrade6 +Gender + 
#                         Gender*JobGrade2*PCJob+
#                         Gender*JobGrade3*PCJob+
#                         Gender*JobGrade4*PCJob+
#                         Gender*JobGrade5*PCJob+
#                         Gender*JobGrade6*PCJob, data=Data4)
# 
# summary(extra_model_2)
str(Data4)
extra_model_2 <- lm(formula= Salary ~ Gender , data=Data4)

summary(extra_model_2)


str(Data4)
extra_model_2 <- lm(formula= Salary ~ JobGrade2 +JobGrade3 +JobGrade4 +JobGrade5 + JobGrade6 +Gender 
                        , data=Data4)

summary(extra_model_2)
