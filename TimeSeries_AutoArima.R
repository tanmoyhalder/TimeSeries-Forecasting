#Title: Forecast 12 months for EXO workload

#Loading Libraries####
library(forecast)
library(tseries)
require(graphics)

#EXO Forecast####
rawdata <- read.csv(~"\\inputdata.csv")

head(rawdata)
rawdata$Month <- c(1:nrow(rawdata))
rawdata$Sales <- rawdata$target
total_timeser <- ts(rawdata$Sales)

plot(total_timeser)

# TrainData
indata <- rawdata[1:33,] 
timeser <- ts(indata$Sales)
plot(timeser)

#Test Data####
outdata <- rawdata[34:36,]

#ARIMA Univate model with Differencing-1####
autoarima <- auto.arima(timeser,trace = T,d=1)
autoarima

#ARIMA Univate model with Differencing-2####
autoarima <- auto.arima(timeser,trace = T,d=2)
autoarima

#ARIMA Univate model with Differencing-3####
autoarima <- auto.arima(timeser,trace = T,d=3)
autoarima

#Based on AIC values, model is finalized with second level of differencing
#ARIMA Univate model with Differencing-2####
autoarima <- auto.arima(timeser,trace = T,d=3)
autoarima

# tsdiag(autoarima)

#plot the original train values
plot(autoarima$x, col="black")
# Plot the fitted data from the model to the existing graph
lines(fitted(autoarima), col="red")

#Stationarity check####
#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

acf(resi_auto_arima)
acf(resi_auto_arima,type = "partial")
# x=auto.arima(resi_auto_arima,d=1)
# lines(fitted(autoarima)+fitted(x),col="blue")

#Conclusion from both the tests & ACF/PACF plots: we have left out
#only with white noise.

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 15)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
MAPE_x <- c()
for ( i in 1:nrow(outdata))
{
  MAPE_x[i] <- accuracy(fcast_auto_arima$pred[i],outdata[,2][i])[5]
  
}
MAPE_x
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
options(scipen = 999)
plot(total_timeser, col = "black",
     xlim=c(1,nrow(rawdata)+12),
     ylim=c(min(auto_arima_pred),min(auto_arima_pred)+max(auto_arima_pred)),type = "l",xlab=NULL,ylab=NULL,
     frame.plot=TRUE,xaxt = "n"
)
lines(auto_arima_pred, col = "red")

# draw an axis on the bottom 
axis(1, at=seq(1,48,2),labels=seq(as.Date("2015/9/1"), by = "2 month", length.out = 24), col.axis="black", las=1)
title("Time Series Forecasting for EXO", xlab="Month",
      ylab="Actual and Forecasted values")


