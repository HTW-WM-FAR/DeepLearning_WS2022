#############################################
# AutoRegressive Integrated Moving Average. #
# Arima                                     #
#############################################
import math
import numpy as np
import yfinance as yf
import pandas as pd
import matplotlib.pyplot as plt
import warnings
# Ignore warning messages
warnings.filterwarnings('ignore')
# Download data from Yahoo Finance
data = yf.download('GOOGL', '2018-01-01', '2022-12-20')

# Extract adjusted closing price and store in a pandas series
df = data.iloc[:, [4]]['Adj Close']

# Split data into training and test sets
len_df = round(len(df) * 0.7)
train = df.iloc[:len_df-1].tolist()
test = df.iloc[len_df-1:].tolist()

#Build the ARIMA model
from statsmodels.tsa.arima.model import ARIMA
prediction = []
ARIMA_model = ARIMA(train, order=(1, 1, 1))
model_fit = ARIMA_model.fit()

for i in range(len(test)):
    yhat = model_fit.forecast()[0]
    prediction.append(yhat)
    actual_test_value = test[i]
    train.append(actual_test_value)
    ARIMA_model = ARIMA(train, order=(1, 1, 1))
    model_fit = ARIMA_model.fit()
print(model_fit.summary())

# Plot the results
plt.figure(1,figsize=(16,8),facecolor='w')
range = data[len_df:].index
plt.plot(range, prediction[:-1],marker = '.', linestyle='dashed')
plt.plot(range, test[:-1])
plt.xlabel('Date')
plt.ylabel('Adjusted Close Price')
plt.show()

#Preparatory work
#Check if data is stationary
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
#ADF-Test
p_value= adfuller(df)
print('p-value:', p_value[1]) #df is not stationary

#Graphical test for stationarity
figure, axis = plt.subplots(2, 1)
axis[0].plot(df,linewidth=1.0)
plot_acf(df, ax=axis[1],linewidth=1.0)
axis[1].set_title('')


# Make the data stationary by taking the first difference
df_diff = data.iloc[:,[4]].diff().dropna()
#Check again for stationarity with ADF-Test
p_value= adfuller(df_diff)
print('p-value:', p_value[1])
#Plot the first difference
plt.figure(2,figsize=(16,8))
plt.plot(df_diff['Adj Close'], linewidth=1.0)
plt.title('')#First Difference')
plt.show()

#Compare original data with first difference
figure, axis = plt.subplots(2, 1)
#Plot original data
axis[0].plot(df,linewidth=1.0)
axis[0].set_title('Original Time Series Data')
#Plot first difference
axis[1].plot(df_diff,linewidth=1.0)
axis[1].set_title('First Difference of Time Series Data')#First Difference

##########################################################################################
#Now that the first order is stationary we will check parameter values for AR and MA     #
#using autocorrelation and partial autocorrelation plots                                 #
##########################################################################################
# Plot autocorrelation
fig, ax = plt.subplots(figsize=(12, 5))
fig.set_facecolor('white')
plot_acf(df_diff, lags=10, ax=ax)
ax.set_title('')
ax.set_xlabel('Lag')
ax.set_ylabel('ACF')
plt.show()

# Plot partial autocorrelation
fig, ax = plt.subplots(figsize=(12, 5))
plot_pacf(df_diff, lags=10, ax=ax)
ax.set_xlabel('Lag')
ax.set_ylabel('PACF')
ax.set_title('')
plt.show()

#Determine the optimal ARIMA parameters for the model
import pmdarima as pm
from pmdarima.arima import auto_arima
import warnings
warnings.filterwarnings('ignore')
model_autoARIMA = auto_arima(y = train, start_p=0,
                             start_q=0,
                             test='adf',
                             max_p=3,
                             max_q=3,
                             m=1,
                             d=None,
                             start_P=0,
                             D=0,
                             trace=True,
                             stepwise=True)
print(model_autoARIMA.aic())
print(model_autoARIMA.bic())
print(model_autoARIMA.summary())



#plot whole graph
plt.figure(3,figsize=(16,8))
plt.plot(data.index, data['Adj Close'], label='Historical Price',linewidth=2)
# Define the range of the test data index
plt.plot(range, prediction, linestyle='dashed', label='Predicted Price')
plt.xlabel('Date')
plt.ylabel('Adjusted Close Price')
plt.legend()
plt.show()

#Check the Performance
from sklearn.metrics import mean_squared_error, r2_score
#Calculate mean squared error
mse = mean_squared_error(test, prediction)
#Calculate root mean squared error
rmse = np.sqrt(mse)
#Calculate R^2
r2 = r2_score(test, prediction)
print('RMSE:', rmse)
print('MSE:', mse)
print('R^2:', r2)