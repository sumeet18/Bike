#remove all data
rm(list=ls(all=T))

#set working directory
setwd("G:/edwisor")

#Current working directory
getwd()

##Load data in R
#reading CSV
data = read.csv("day.csv", header = T)

#view data
View(data)

#check datatype
class(data)

#summary of data
summary(data)

#column names
colnames(data)

#number of variables
length(unique(data))

#change class from numerical to factor
data$season = as.factor(data$season)
data$yr = as.factor(data$yr)
data$mnth = as.factor(data$mnth)
data$holiday = as.factor(data$holiday)
data$weekday = as.factor(data$weekday)
data$workingday = as.factor(data$workingday)
data$weathersit = as.factor(data$weathersit)

#load libraries
library("ggplot2")
library("scales")
library("psych")
library("gplots")

#Bar plot(categorical data)
#If you want count then stat="bin"
ggplot(data, aes_string(x = data$season)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Season") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("frequency count of Season") +  theme(text=element_text(size=15))


ggplot(data, aes_string(x = data$yr)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Year") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("frequency count of Year") +  theme(text=element_text(size=15))

ggplot(data, aes_string(x = data$holiday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Holiday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("frequency count of Holiday") +  theme(text=element_text(size=15))

ggplot(data, aes_string(x = data$weekday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Weekday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("frequency count of Weekday") +  theme(text=element_text(size=15))

ggplot(data, aes_string(x = data$workingday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Workingday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("frequency count of Workingday") +  theme(text=element_text(size=15))

ggplot(data, aes_string(x = data$weathersit)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Weathersit") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("frequency count of Weathersit") +  theme(text=element_text(size=15))


#Histogram 
ggplot(data, aes_string(x = data$temp)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("temperature") + ylab("Frequency") + ggtitle("data: temperature") +
  theme(text=element_text(size=20))


ggplot(data, aes_string(x = data$atemp)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("temperature felt") + ylab("Frequency") + ggtitle("data: temperature felt") +
  theme(text=element_text(size=20))

ggplot(data, aes_string(x = data$hum)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("humidity") + ylab("Frequency") + ggtitle("data: humidity") +
  theme(text=element_text(size=20))

ggplot(data, aes_string(x = data$windspeed)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("windspeed") + ylab("Frequency") + ggtitle("data: windspeed") +
  theme(text=element_text(size=20))


#Box plot
ggplot(data, aes_string(x = data$cnt, y = data$temp, 
                                  fill = data$temp)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("count") + ylab("temperature") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

ggplot(data, aes_string(x = data$cnt, y = data$atemp, 
                        fill = data$temp)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("count") + ylab("temperature felt") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

ggplot(data, aes_string(x = data$cnt, y = data$hum, 
                        fill = data$temp)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("count") + ylab("humidity") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

ggplot(data, aes_string(x = data$cnt, y = data$windspeed, 
                        fill = data$temp)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("count") + ylab("windspeed") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

quantile(data$hum, c(.01))
data[which(data$hum<0.3121251),("hum")] = 0.3121251

quantile(data$windspeed, c(.98))
data[which(data$windspeed>0.3761186),("windspeed")] = 0.3761186

copy=data


# dummify the data
library(caret)

sea <- dummyVars(" ~ season", data = copy)
season <- data.frame(predict(sea, newdata = copy))

year <- dummyVars(" ~ yr", data = copy)
Year <- data.frame(predict(year, newdata = copy))

mnth <- dummyVars(" ~ mnth", data = copy)
month <- data.frame(predict(mnth, newdata = copy))

week <- dummyVars(" ~ weekday", data = copy)
weekday <- data.frame(predict(week, newdata = copy))

weather <- dummyVars(" ~ weathersit", data = copy)
Weather <- data.frame(predict(weather, newdata = copy))

z=cbind(season,Year,weekday,Weather,month,copy)

new = subset(z, 
              select = -c(season, yr, mnth, instant, dteday, weekday, weathersit))

#correlation
#Load Libraries
library(corrgram)
numeric_index = sapply(new,is.numeric)

numeric_data = new[,numeric_index]

corrgram(new[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


new = subset(new, 
             select = -c(yr.1,weathersit.2,atemp,casual,registered))

#Decision treemodel
#Load Libraries
library(rpart)

#Divide the data into train and test
#set.seed(123)
train_index = sample(1:nrow(new), 0.8 * nrow(new))
train = new[train_index,]
test = new[-train_index,]

# ##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-32])

#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,32], predictions_DT)


#calculate MSE
MSE = function(m, o){
  (mean((m - o)^2))
}
MSE(test[,32], predictions_DT)

#calculate RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(test[,32], predictions_DT)


#Linear Regression
#check multicollearity
library(usdm)
vif(new[,-32])

vifcor(new[,-32], th = 0.9)

#run regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,1:31])

#Calculate MAPE,MSE,RMSE
MAPE(test[,32], predictions_LR)
MSE(test[,32], predictions_LR)
RMSE(test[,32], predictions_LR)

#Randomforest
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 100)

#transform rf object to an inTrees' format
treeList = RF2List(RF_model)

#Extract rules
exec = extractRules(treeList, train[,-33])  # R-executable conditions

exec[1:2,]

# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]

ruleMetric = getRuleMetric(exec, train[,-33], train$cnt)  # get rule metrics
# 
# #evaulate few rules
ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-32])


#Calculate MAPE,MSE,RMSE
MAPE(test[,32], RF_Predictions)
MSE(test[,32], RF_Predictions)
RMSE(test[,32], RF_Predictions)


