
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 11 15:27:05 2023

@author: annalenafink
"""

import math
import yfinance as yf
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler 
import matplotlib.pyplot as plt
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import seaborn as sns

#Import Yahoo Finance Data:
    
alv_data = yf.download('ALV.DE', start='2017-01-01', end='2021-12-30')
alv_data.head()

#####################################
###### Descriptive Statistics #######
#####################################

#.describe() generates descriptive statistics inclduing those that summarize the central tendency, 
#dispersion and shape of a dataset's distribution, excluding NaN values
alv_data.describe()

#.info() method prints info about a DataFrame including index dtype and columns, non-null values and memory usage
alv_data.info()

#Closing Price: last price at which the stock is traded during the regular trading day.
#A stock's closing price is the standard benchmark used by investors to track its performance over time.
#Historical view of the closing price:
plt.figure(figsize=(15, 6)) #Set the plot figure size
plt.title('Stock Prices History: Allianz SE') ##Set the plot title
plt.plot(alv_data['Close'], color=(0.1,0.7,0.2)) #Use the Matplotlib plot method to create a line chart for historical close prices of Allianz Stock
plt.xlabel('Date') #Set the x-axis labels
plt.ylabel('Prices in EUR') #Set the y-axis labels
plt.savefig('alv_price_history.pdf')   #Save as pdf
    
#Volume of Sales: volume refers to the number of shares of security traded between its daily open and close.
#Plot of Total volume of stock being traded each day:
plt.figure(figsize=(15, 6)) #Set the plot figure size
plt.title('Sales Volume History: Allianz SE') ##Set the plot title
plt.plot(alv_data['Volume'], color=(0.1,0.4,0.3)) #Use the Matplotlib plot method to create a line chart for historical close prices of Allianz Stock
plt.xlabel('Date') #Set the x-axis labels
plt.ylabel('Volume') #Set the y-axis labels
plt.savefig('alv_sales_history.pdf')   #Save as pdf


###################################
############# Model ###############
###################################

#########################
#### Data Processing ####
#Extract the closing prices from the stock data and convert it to a timeseries:
close_prices = alv_data['Close']
values = close_prices.values

#Calculate the data size for 80% of the dataset. The math.ceil method is to ensure the data size is rounded up to an integer.
training_data_len = math.ceil(len(values)* 0.8)

#Use the Scikit-Learn MinMaxScaler to normalize all stock data ranging from 0 to 1 and reshape the normalized data into a two-dimensional array.
scaler = MinMaxScaler(feature_range=(0,1))
scaled_data = scaler.fit_transform(values.reshape(-1,1))

#Set apart the first 80% of the stock data as the training set:
training_data = scaled_data[0: training_data_len, :]

#Create an empty list for a sequence of feature data (x_training)
x_training = []

#Create an empty list for a sequence of label data (y_training).
y_training = []

#Create a 304-days window of historical prices as our feature data (x_train) and the following 304-days window as label data (y_train):
for i in range(304, len(training_data)):
    x_training.append(training_data[i-304:i, 0])
    y_training.append(training_data[i, 0])

#Convert the feature data (x_training) and label data (y_training) into Numpy array as it is the data format accepted by the Tensorflow when training a neural network model. Reshape again the x_training and y_training into a three-dimensional array as part of the requirement to train a LSTM model:   
x_training, y_training = np.array(x_training), np.array(y_training)
x_training = np.reshape(x_training, (x_training.shape[0], x_training.shape[1], 1))


#################################
#### Preparation of Data Set ####
#Extract the closing prices from normalized dataset (the last 20% of the dataset):
testing_data = scaled_data[training_data_len-304: , : ]

#Create feature data (x_testing) and label data (y_testing)from test set:
x_testing = []
y_testing = values[training_data_len:]

for i in range(304, len(testing_data)):
  x_testing.append(testing_data[i-304:i, 0])

#Convert the feature data (x_testing) and label data (y_testing) into Numpy array. Reshape again the x_testing and y_testing into a three-dimensional arra:
x_testing = np.array(x_testing)
x_testing = np.reshape(x_testing, (x_testing.shape[0], x_testing.shape[1], 1))
    

##############################################
#### Setting Up LSTM Network Architecture ####
#Define a Sequential model which consists of a linear stack of layers
model = keras.Sequential()

#Add a LSTM layer by giving it 100 network units. Set the return_sequence to true so that the output of the layer will be another sequence of the same length.
model.add(layers.LSTM(100, return_sequences=True, input_shape=(x_training.shape[1], 1)))

#Add another LSTM layer with also 100 network units. But setting the return_sequence to false for this time to only return the last output in the output sequence.
model.add(layers.LSTM(100, return_sequences=False))

#Add a densely connected neural network layer with 25 network units
model.add(layers.Dense(25))

#AAdd a densely connected layer that specifies the output of 1 network unit
model.add(layers.Dense(1))

#Show the summary of our LSTM network architecture.
model.summary()


#############################
#### Training LSTM Model ####
#Adopt “adam” optimizer and set the mean square error as loss function:
model.compile(optimizer='adam', loss='mean_squared_error')

#Train the model by fitting it with the training set with a batch_size of 1 and running the training for 3 epochs.
model.fit(x_training, y_training, batch_size= 1, epochs=3)

##########################
#### Model Evaluation ####
#Apply the model to predict the stock prices based on the test set:
predictions = model.predict(x_testing)

#Use the inverse_transform method to denormalize the predicted stock prices.
predictions = scaler.inverse_transform(predictions)

#Apply the RMSE formula to calculate the degree of discrepancy between the predicted prices and real prices (y_testing) and display the result.
rmse = np.sqrt(np.mean(predictions - y_testing)**2)
rmse

#### Visualizing the Predicted Prices
data = alv_data.filter(['Close'])
train = data[:training_data_len]
validation = data[training_data_len:]
validation['Predictions'] = predictions
plt.figure(figsize=(16,8))
plt.title('LSTM: Allianz SE')
plt.xlabel('Date')
plt.ylabel('Close Price in EUR')
plt.plot(train)
plt.plot(validation[['Close', 'Predictions']])
plt.legend(['Train', 'Val', 'Predictions'], loc='lower right')
plt.savefig('alv_lstm1.pdf',format="pdf",)   #Save as pdf
plt.show()





















