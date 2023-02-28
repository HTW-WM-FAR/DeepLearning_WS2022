library (ggplot2)
library(tseries) 
library(zoo)
library(hydroGOF) 
library(forecast) 
library(Metrics)
library(tidyr)

##########################
#######Allianz SE#########
##########################

##########################
#### Data Preparation ####
#Read in data:
alvdata <- read.csv('/Users/annalenafink/Deeplearning/yahoo_alv.csv', header=T, stringsAsFactors=F)
alvdata$Date = as.Date(alvdata$Date) #set date in according format

#add timesteps t(t1=1, t2=2,...,tn=n) for regression model:
alvdata <- cbind(alvdata, "observation"=1:nrow(alvdata)) 

#set limit for training and test data (ratio: 80/20):
training_size <- ceiling(0.8*nrow(alvdata)) 

######################################
#### Create training and test set #### 

#create new data frame for training data:
training_data <- alvdata[1:training_size,] 

#declare closing price as time series:
ts_data <- ts(training_data[,5]) 

#add time series to training data:
training_data$timeseries <- ts(ts_data) 

##create new data frame for test set and fill with test data:
testing_data <- alvdata[(training_size+1):nrow(alvdata),] 

#declare closing price as time series:
ts_data <- ts(testing_data[,5]) 

#add time series to testing data:
testing_data$timeseries <- ts(ts_data) 

###########################
#### Linear Regression ####

#Develop Regression Model:
regression_model <- lm(timeseries~observation, data=training_data) #compute regression from training data
print(regression_model) #show intercept and slope of regression line

training_data$Regression=as.ts(predict(regression_model,training_data)) #estimate y for each time step

#Prediction for testing model:
prediction <- predict(regression_model,testing_data) #predict testing data
testing_data$Prediction=predict(regression_model,testing_data)

#Plot testing and training and prediction:
ggplot ()+
  labs(title="Linear Regression: Allianz SE", x="time in t", y="Closing Price in EUR", col="") + 
  theme(plot.title=element_text(hjust=0.5),legend.position="none") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(data=training_data, aes(x=Date, y = Close, col = "Closing Price (Training data)")) +
  geom_line(data=training_data, aes(x=Date, y = Regression , col = "Estimated Closing Price (Training data)"))+
  geom_line(data=testing_data, aes(x=Date, y = Adj.Close, col = "Closing Price (Test data)")) +
  geom_line(data=testing_data, aes(x=Date, y = Prediction , col = "Predicted Closing Price (Test data)")) +
  scale_x_date(date_labels = "/%b /%y", date_breaks = "1 year")+
  scale_color_manual(values = c("#8B0000","#FA8072","#6495ED","#00008B"))

#Calculate RMSE for training and testing data:
RMSE_testing <- rmse(testing_data$timeseries ,testing_data$Prediction)
RMSE_testing


#####################
#### ARIMA Model ####

#Stationary test:
adf.test(diff(log(alvdata[,4])), alternative="stationary", k=0)

#Build ARIMA model:
#develop arima model according to timeseries:
arima_model <- auto.arima(training_data$timeseries, trace=T, approximation=F) 

#check residuals:
tsdisplay(residuals(arima_model),lag.max=15) 

#save estimated training data:
training_data$fit=arima_model$fitted

#Predict testing data:
predicted <- forecast(arima_model,h=nrow(testing_data)) #predict testing data
predicted_tmp=as.numeric(predicted$mean)
testing_data$Prediction=predicted_tmp

#Plot testing and training and prediction:
ggplot ()+
  labs(title=(expression(atop("ARIMA (1,1,1): Allianz SE"))), x="time in t", y="Closing Price in EUR", col="") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line(data=training_data, aes(x=Date, y=Close, col="Closing Price (Training data)")) +
  geom_line(data=training_data, aes(x=Date, y=fit , col = "Estimated Closing Price (Training data)"))+
  geom_line(data=testing_data, aes(x=Date, y=Adj.Close, col="Closing Price (Test data)")) +
  geom_line(data=testing_data, aes(x=Date, y=Prediction , col="Predicted Closing Price (Test data)")) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year")+
  scale_color_manual(values = c("#8B0000","#FA8072","#6495ED","#00008B"))

#Calculate RMSE for training and testing data                                
RMSE_test <- rmse(testing_data$timeseries, testing_data$Prediction)
RMSE_test

