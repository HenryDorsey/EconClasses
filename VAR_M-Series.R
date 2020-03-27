# An example VAR model using the Box-Jenkins M-Series.
carpet<- read.table("E://Data/SeriesM.txt",header=T)
head(carpet)
install.packages("fpp2")
library(fpp2)
install.packages("urca")
library(urca)

# The first part of this program is just trying to 
# determine the stochastic order of the sales and
# hstarts time series.  They are both found to be I(1).
sales <- ts(carpet$y,start=1990,frequency=12)
hstarts <- ts(carpet$x,start=1990,frequency=12)
plot(sales)
plot(hstarts)
acf(sales)
acf(hstarts)
# Unit root tests for sales and hstarts
summary(ur.kpss(sales,type="tau"))
summary(ur.df(sales,type="trend",lags=2))
summary(ur.kpss(hstarts,type="tau"))
summary(ur.df(hstarts,type="trend",selectlags="BIC"))
dsales<-diff(sales)
dhstarts<-diff(hstarts)
# Unit root tests for dsales and dhstarts
summary(ur.kpss(dsales,type="mu"))
summary(ur.df(dsales,type="drift",selectlags="BIC"))
summary(ur.kpss(dhstarts,type="mu"))
summary(ur.df(dhstarts,type="drift",selectlags="BIC"))
plot(dsales)
plot(dhstarts)
acf(dsales)
acf(dhstarts)
pacf(dhstarts)

#####################################################
# Looking to model the M-series using a VAR
# of the hstarts and sales series.
#####################################################

install.packages("vars")
library(vars)

# Putting the two time series together in the file both.

both<-cbind(sales,hstarts)

# Establishing the equal lag length for the VAR
# to be used in the Granger Causal testing.
# Notic the use of the type="trend" option since
# both hstarts and sales are I(1).
VARselect(both, lag.max=10,
          type="trend")[["selection"]]
# The lag length of 9 is favored by 3 of the four
# system-wide information measures produced
# by the VARselect function.  Therefore, we use
# a lag-length of 9 for the VAR to do the Granger
# Causal testing and forecasting using the VAR.

var1 <- VAR(both,p=9, type="trend")
# Using the summary function to get the VAR coefficients
summary(var1)
#
# The serial.test below indicates that the residuals
# of the VAR are white noise.
serial.test(var1, lags.pt=24, type="PT.asymptotic")

# The first causality test examines whether hstarts
# Granger Causes sales.  It found in the affirmative.
causality(var1,cause="hstarts")
causality(var1,cause="sales")
# The second causality test examines whether sales
# Granger Causes hstarts.  This test found that
# sales does not Granger cause hstarts.  Therefore
# the hstarts variable is exogenous.
# Both tests found that there is no contemporaneous
# relationship between the two series. 
# These causality test results hint that
# an ARDL model (to be studied next) might be better
# for modeling the M-series and forecasting the M-series
# than a VAR model that assumes two-way causality.
# Nevertheless we will continue and forecast the two
# series (sales and hstarts) using a VAR of lag length 8
# with trend.

# Now generating plot of forecasts using var1 model and below pipe command.

forecast(var1) %>%
  autoplot() + xlab("Year")

# Now we begin an out-of-sample forecasting experiment
# to see what extent the hstarts series can help us
# in forecasting sales of our firm.

# Here we move to partitioning the data into a training set
# and a test set so that we can compare the forecasting 
# accuracy of an Arima model for sales and the VAR for
# sales.  The training set spans 1990.1 to 2001.6.  The
# test data set spans 2001.7 - 2002.6, a total of 12 observations.
sales.train <- window(sales,start=1990,end=c(2001,6))
sales.test <- window(sales,start=c(2001,7))
# Here is the auto.arima fit of the sales data and its forecasts.
arima.fit <-auto.arima(sales.train,d=1)
# The best arima model for sales over the training data is
# arima(4,1,0).
summary(arima.fit)
# Here we create the test data set forecasts for sales and
# plot them.
arima.fcst <- forecast(arima.fit, h=12)
autoplot(arima.fcst)
# Now we put together the training data set for the VAR.
hstarts.train <- window(hstarts,start=1990,end=c(2001,6))
hstarts.test <- window(hstarts,start=c(2001,7))
both.train<-cbind(sales.train,hstarts.train)
both.test<-cbind(sales.test,hstarts.test)
# One more time we check out the optimal lag length for the
# VAR, this time over the training data set.  As it turns out
# the majority choice is 8 so we go with 8 instead of 9.
#   
VARselect(both.train, lag.max=8,
          type="trend")[["selection"]]
# 
var1.train<-VAR(both.train,p=8, type="both")
summary(var1.train)
# Generating the forecasts for the VAR(8) and plotting them.
fcast.var<-forecast(var1.train,h=12)
autoplot(fcast.var)
#
#
# Now we go into the comparison of the accuracy of an arima
# model for sales versus a VAR model that incorporates a
# "leading" indicator that might possibly enhance the accuracy
# in forecasting sales.  This is what we call an 
# out-of-sample test of the efficacy of a leading indicator,
# hstarts, for enhancing the accuracy of forecasting sales.
# If the leading indicator enhances the accuracy vis-a-vis
# the VAR, we conclude that the leading indicaor (hstarts)
# is useful and should be used in the future.  If the leading
# indicator does not enhance forecasting accuracy, we go in
# search for some other variable to serve as a leading indicator. 
#
# Here we calculate the test data set forecasting accuracy
# of the univariate arima model. 
accuracy(arima.fcst,sales.test)
# Here we calculate the test data set forecasting accuracy
# for sales using the VAR(8). 
accuracy(fcast.var,both.test[,1:2],h=12,d=1,D=0)
# The results of the out-of-sample forecasting accuracy tests
# for sales are as follows:
# The arima test RMSE = 3.599 and test MAE = 2.92.
# The VAR test measures for sales are
# test RMSE = 2.17 and test MAE = 1.667.
# Obviously, the hstarts varable via the VAR offers improved
# forecasting accuracy over the arima model for sales.
# Therefore, we conclude that hstarts is a useful leading
# indicator in this instance.
