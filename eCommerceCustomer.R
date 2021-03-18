#import libraries

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("hexbin")
install.packages("caret")

library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hexbin)
library(caret)

#Import data
customers <- read.csv("D:/DATA CAMP PROJECTS/Ecommerce Customers/Ecommerce Customers.csv")

#Check the head,str and summary of the customers 
head(customers)
glimpse(customers)
summary(customers)
str(customers)




#Exploratory Data Analysis
ggplot(customers,aes(x = Time.on.Website, y = Yearly.Amount.Spent)) + geom_point() + geom_smooth()

#Do the same but with the Time on App column instead
ggplot(customers,aes(x = Time.on.App, y = Yearly.Amount.Spent)) + geom_point() + geom_smooth()

#create a hex bin plot comparing Time on App and Length of Membership
ggplot(customers,aes(x = Time.on.App, y = Yearly.Amount.Spent)) + geom_hex(bins =30)

#Only keep numeric variables
NumCol <- sapply(customers,is.numeric)
Ecom_cust <- customers[,NumCol]


#From the below diagram we can see 'Yearly Amount Spent' is highly correlated with 'Lenght of Membership'

install.packages("psych")
library(psych)
pairs.panels(Ecom_cust[,1:5],
            method = "pearson",
            hist.col = "#66A61E",
            density = TRUE,
            ellipses = TRUE)

# Create a linear model plot of Yearly Amount Spent vs. Length of Membership
ggplot(customers,aes(x = Length.of.Membership, y = Yearly.Amount.Spent)) + geom_point() + geom_smooth(method = lm)

#Training and Testing Data
#let's go ahead and split the data into training and testing sets. Set a variable X equal to the numerical features of the customers and a variable y equal to the "Yearly Amount Spent" column

install.packages("caTools")
library(caTools)
set.seed(101)
sample <- sample.split(Ecom_cust$Yearly.Amount.Spent, SplitRatio = 0.7)
train <- subset(Ecom_cust,sample = TRUE)


#Training the Model
#Train model on our training data
model <- lm(Yearly.Amount.Spent~., data = train)
summary(model)






#regression diagnostics
plot(model,which=1, col=c("red"))
plot(model, which=2, col=c("blue"))

#Residuals
ggplot(Ecom_cust, aes(model$residuals)) + geom_histogram(fill = 'steelblue', color= 'black')
+ labs(title = "Histogram of Residuals", x = "residuals", y = "count")


#Conclusion

#We still want to figure out the answer to the original question,
#do we have to focus our effort on mobile app or website development? 
#Or maybe that doesn't even really matter, and Membership Time is what is really important.
#Let's see if we can interpret the coefficients at all to get an idea.

#Print out the coefficients of the model
confint(model)
summary(model)$coef


#How can you interpret these coefficients?

#Holding all other features fixed, a 1 unit increase in Avg. Session Length is associated with an increase of 25.73 total dollars spent.
#Holding all other features fixed, a 1 unit increase in Time on App is associated with an increase of 38.71 total dollars spent.
#Holding all other features fixed, a 1 unit increase in Time on Website is associated with an increase of 0.44 total dollars spent.
#Holding all other features fixed, a 1 unit increase in Length of Membership is associated with an increase of 61.58 total dollars spent.

