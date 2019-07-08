## Loading the required libraries

library(dplyr)
library(fpp2)
library(readxl)
library(urca)
library(gridExtra)
library(seasonal)
## Getting the required library
 getwd()
##


########################################################################
## Benchmark Methods
########################################################################
## Q1:Plot the data

## Reading the required data
retail <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>% pull("29389429") %>%
  ts(start = c(1982,4), frequency = 12)

retail<-window(retail,start=c(1988,4))


## Plotting the data
autoplot(retail)+
  xlab("Year")+
  ylab("Turnover $ in millions")+
  ggtitle("Retail data: Footwear and other personal accessory in Northern Territory")

## Sub-series plot
ggsubseriesplot(retail)+
  xlab("Month")+
  ylab("Turnover $ in millions")+
  ggtitle("Retail data- Subseries plot")

## Seasonal plot
ggseasonplot(retail)+
  xlab("Month")+
  ylab("Turnover $ in millions")+
  ggtitle("Retail data- Season plot")

########################################################################
## Q2:Transformation

## Cube root transformation
autoplot(retail^(1/3))+
  xlab("Year")+
  ylab("Cube root Transformation: Turnover")+
  ggtitle("Cube root transformaton")

## Square root transformation
autoplot(retail^(1/2))+
  xlab("Year")+
  ylab("Square root Transformation: Turnover")+
  ggtitle("Square root transformaton")

## Log transformation
autoplot(log(retail))+
  xlab("Year")+
  ylab("Log Transformation: Turnover")+
  ggtitle("Log transformaton")

## Box Cox transformation
lambda=BoxCox.lambda(retail)
autoplot(BoxCox(retail,lambda))+
  xlab("Year")+
  ylab("Box Cox Transformation: Turnover")+
  ggtitle("Box Cox transformaton")
## We choose the Box-Cox transformation

########################################################################
## Q3:Train test split

## Splitting the data into testing and training set
#Splitting the data into test and training
retail_train <- window(retail, end=c(2014,12))
retail_test <- window(retail, start=c(2015,1))

#plotting the training and testing data
autoplot(retail) +
  xlab("Year")+
  ylab("Turnover ($ millions)")+
  ggtitle("Training and testing data")+
  autolayer(retail_train, series="Training") +
  autolayer(retail_test, series="Test")+xlab("year")+
  ylab("Turnover ($ millions)")+
  ggtitle("Train and test data")

########################################################################
## Q4:Applying the benchmark method

#Applying two benchmark methods
autoplot(retail_test,series="Testing")+
  autolayer(retail_train, series="Training") +
  autolayer(snaive(retail_train,h=24,lambda = lambda),PI=F,series="Seasonal Naive")+
  autolayer(rwf(retail_train,drift=TRUE,h=24,lambda=lambda),PI=F,series="Drift")+
  xlab("year")+
  ylab("Turnover ($ millions)")+
  ggtitle("Seasonal Naive and random walk forecast")

## Choosing two benchmark method
f1 <- snaive(retail_train, h=length(retail_test))
f2 <- rwf(retail_train, drift=TRUE, h=length(retail_test))

## Tabulating the acccuracy
tab <- matrix(NA,ncol=4,nrow=2)
tab[1,] <- accuracy(f1, retail_test)[2,c(2,3,5,6)]
tab[2,] <- accuracy(f2, retail_test)[2,c(2,3,5,6)]
colnames(tab) <- c("RMSE","MAE","MAPE","MASE")
rownames(tab) <- c("Seasonal naive method", "random walk method")

## Accuracy table
tab

########################################################################
## Q5:Checking the residuals

## Checking the residuals
checkresiduals(f1)

## ########################################################################
## Q6:Plotting the forecast

autoplot(retail)+
  autolayer(snaive(retail,h=24,lambda = BoxCox.lambda(retail)),PI=F,series="Naive")+
  ggtitle("Forecast 2017-18")+xlab("Year")+ylab("Turnover ($ millions)")

########################################################################
## Q7: New forecast method
## The forecasting method of seasonal naive ignores the increasing trend of the graph.
## Thus, we would like to consider a forecasting method that is a combination of 
## the random walk with drift and the seasonal naive method.

########################################################################



########################################################################
## ETS and Decomposition
########################################################################

## Q1: Plot the data
## Plotting the data
autoplot(retail)+
  xlab("Year")+
  ylab("Turnover $ in millions")+
  ggtitle("Retail data: Footwear and other personal accessory in Northern Territory")

## Arbitrary decomposition of the time series in order to check the remainders
autoplot(stl(retail,s.window=5))

autoplot(decompose(retail,type="multiplicative"))+
  xlab("Year")+
  ggtitle("Retail data: Footwear and other personal accessory in Northern Territory")
## We observe that the remainders are multiplicative

########################################################################

## Q2: Plotting the estimated model
## MAdM model for the retail data
retail %>% ets(model="MAM",damped=T) %>% autoplot

model<-retail %>% ets(model="MAM",damped=T)

## Check the summary of the model
summary(model)

########################################################################
## Q3: Checking the residuals
## Checking the residuals
checkresiduals(model)
## The residuals are normally distributed and the test indicates we reject the null
## hypothesis that the residuals are white noise

########################################################################
## Q4: Choosing the automated model by R

## Choosing the model using R
retail %>% ets() %>% autoplot

model.R<-retail %>% ets()

## Summary of the model
summary(model.R)
## R proposes a holt winter's method model

## Checking the residuals
checkresiduals(model.R)
########################################################################
## Q5: Choose the model

## We choose the model given by R that is MAM model

########################################################################
## Q6: Forecasting with the models

## Forecasting using the R model
mf<-forecast(model.R,h=24)
autoplot(mf,xlim=c(2010,2020))+xlab("Year")+
  ylab("Turnover $ in millions")+ggtitle("Chosen model")

## Forecasting using our model
m<-forecast(model,h=24)

########################################################################
## Q7: Forecasting with the models

## Plotting the two using grid.arrange function to make the comparison
library(gridExtra)
p1<-autoplot(mf,xlim=c(2010,2020))+xlab("Year")+
  ylab("Turnover $ in millions")+ggtitle("Forecast using estimated model by R")

p2<-autoplot(m,xlim=c(2010,2020))+xlab("Year")+
  ylab("Turnover $ in millions")+ggtitle("Our estimated model")

## Plotting the two graphs side by side
grid.arrange(p1,p2)

#################################################
#### ARIMA

## Q1: Transformation and differencing
## Transformation
lambda=BoxCox.lambda(retail)

## plotting the transformation
BoxCox(retail,lambda) %>% autoplot()+xlab("YEAR")+ylab("Turnover BoxCox scale")+
  ggtitle("BoxCox Time series")

## The data shows a clear trend hence differencing is required
## We assume that first order differencing may be required

cbind("Turnover $ Millions" = retail,
      "BoxCox Transformation" = BoxCox(retail,lambda),
      "Seasonally\n differenced logs" =
        diff(BoxCox(retail,lambda),12),
      "Doubly\n differenced logs" =
        diff(diff(BoxCox(retail,lambda),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Time series with differencing")

BoxCox(retail,lambda) %>% diff(lag=12) %>% autoplot()

## KPSS unti root test
BoxCox(retail,lambda) %>% diff(lag=12) %>% diff() %>% ur.kpss() %>% summary()
## This confirms that the obtained series is stationary

#################################################
## Q2: Choosing an appropriate ARIMA model

## Thus, we require only first order difference for non-seasonal component
BoxCox(retail,lambda) %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

## Fitting the ARIMA model
fit1=Arima(retail,order=c(0,1,1),seasonal=c(0,1,1),lambda=lambda)


#################################################
## Q3: Checking the residuals

## Checking the residuals and ACF and PACF
checkresiduals(fit1)
## We observe significant spike at lag 3 and 12 

summary(fit1)
## AICc=-668.29

#################################################
## Q4:Considering upto 3 models

### MODEL 2:
## Fitting the ARIMA model
fit2=Arima(retail,order=c(1,1,1),seasonal=c(0,1,1),lambda=lambda)
## Checking the residuals and ACF and PACF
checkresiduals(fit2)
summary(fit2)

## AICc value: -674.99

### MODEL 3:
## Fitting the ARIMA model
fit3=Arima(retail,order=c(1,1,1),seasonal=c(1,1,1),lambda=lambda)
## Checking the residuals and ACF and PACF
checkresiduals(fit3)
summary(fit3)

## AICc value: -673.89

### MODEL 4:
## Fitting the ARIMA model
fit4=Arima(retail,order=c(1,1,3),seasonal=c(1,1,1),lambda=lambda)
## Checking the residuals and ACF and PACF
checkresiduals(fit4)
summary(fit4)

## AICc value: -672.02

### Lowest value is for MODEL 2: ARIMA(1,1,1)(0,1,1)

#################################################

## Q5: Model chosen by auto.arima
## Fitting the model
auto_fit1<-retail %>% auto.arima(lambda=lambda) 
## ARIMA(0,1,2)(2,1,2)[12] 

## Checking the residuals and the summary of the model
checkresiduals(auto_fit1)
summary(auto_fit1)
## AIC=-669.67   AICc=-669.32   BIC=-643.03

#################################################

## Q6: Model chosen by auto.arima with approximation and stepwise FALSE
auto_fit2<-retail %>% auto.arima(lambda=lambda,approximation = FALSE,stepwise=FALSE) 
## ARIMA(1,1,1)(0,1,1)[12] 

## Checking the residuals and the summary
checkresiduals(auto_fit2)
summary(auto_fit2)
## AIC=-675.12   AICc=-674.99   BIC=-659.9

###################################################

## Q7: Choosing the model after RMSE
## Function to calculate the RMSE
getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}

### Calculating the RMSE values
ARIMAtab <- matrix(NA,ncol=1,nrow=5)
ARIMAtab[1,] <- getrmse(retail,h=24,order=c(0,1,1),seasonal=c(0,1,1),lambda=lambda)
ARIMAtab[2,] <- getrmse(retail,h=24,order=c(1,1,1),seasonal=c(0,1,1),lambda=lambda)
ARIMAtab[3,] <- getrmse(retail,h=24,order=c(1,1,1),seasonal=c(1,1,1),lambda=lambda)
ARIMAtab[4,] <- getrmse(retail,h=24,order=c(1,1,3),seasonal=c(1,1,1),lambda=lambda)
ARIMAtab[5,] <- getrmse(retail,h=24,order=c(0,1,2),seasonal=c(2,1,2),lambda=lambda)

colnames(ARIMAtab) <- c("RMSE")
rownames(ARIMAtab) <- c("ARIMA(0,1,1)(0,1,1)[12] |", "ARIMA(1,1,1)(0,1,1)[12] |",
                   "ARIMA(1,1,1)(1,1,1)[12] |", "ARIMA(1,1,3)(1,1,1)[12] |",
                   "ARIMA(0,1,1)(2,1,2)[12] |")

## The ARIMA table for the RMSE calculated values
ARIMAtab
## We choose the ARIMA model (1,1,1)(0,1,1)[12]

#######################################################
## Q8: Generating the forecasts

ARIMA_fit<-Arima(retail,lambda=lambda,order=c(1,1,1),seasonal=c(0,1,1))
## Checking the residuals of the fit
checkresiduals(ARIMA_fit)

## Forecast of the ARIMA fit
forecast(ARIMA_fit) %>% autoplot(xlim=c(2015,2019))+
  xlab("YEAR")+ylab("Turnover $ million")+ggtitle("ARIMA forecast")
## Forecasting increases with the forecast period

#######################################################
## Q9: Observed values

## Reading the given data
myts <- read_xlsx("RetailDataIndividualFull.xlsx", skip=3) %>% 
  pull("29389429") %>% ts(start = c(1982,4), frequency = 12)
myts<-window(myts,start=c(1988,4))

## Plotting the observed values
autoplot(window(myts,start=c(2017,1)),series="2017-18 Observed")+
  autolayer(retail,series="Time series given")+
  xlab("YEAR")+
  ylab("Turnover $ million")+
  ggtitle(" Observed values for 2017-18")

#######################################################
## Q10: Plotting the forecast

## plotting the required graph
autoplot(window(myts,start=c(2015,1)),series="Actual data")+
  autolayer(forecast(ARIMA_fit),PI=FALSE,series="ARIMA Forecast")+
  autolayer(snaive(retail,lambda=lambda),PI=FALSE,series="Seasonal naive")+
  autolayer(forecast(model.R),PI=FALSE,series="ETS")+
  xlab("YEAR")+
  ylab("Turnover $ Millions")+
  ggtitle("Forecast comparison")

#######################################################
## Q11: Calculating the accuracy

## Finding accuracy and tabulating
FCAST_tab <- matrix(NA,ncol=4,nrow=3)
## ARIMA fit
FCAST_tab[1,] <- accuracy(forecast(ARIMA_fit),window(myts,start=c(2017,1)))[2,c(2,3,5,6)]
## Benchmark method
FCAST_tab[2,] <- accuracy( snaive(retail,lambda=lambda),
                    window(myts,start=c(2017,1)))[2,c(2,3,5,6)]
## ETS model
FCAST_tab[3,] <- accuracy(forecast(model.R),window(myts,start=c(2017,1)))[2,c(2,3,5,6)]

colnames(FCAST_tab) <- c("RMSE","MAE","MAPE","MASE")
rownames(FCAST_tab) <- c("ARIMA          |", "Seasonal Naive |", "ETS            |")

## Plotting the forecast table for accuracy
FCAST_tab

#######################################################
## Q12: Plotting the forecast accuracy

## Using the best model to forecast
lambda=BoxCox.lambda(myts)
## Fitting the best model selected
best_fit<-Arima(myts,lambda=lambda,order=c(1,1,1),seasonal=c(0,1,1))
## Forecasting the model and plotting
forecast(best_fit) %>% autoplot(xlim=c(2016,2021))+
  xlab("YEAR")+
  ylab("Turnover $ millions")+
  ggtitle("ARIMA Forecast: Best model")

forecast(best_fit) %>% autoplot()+
  xlab("YEAR")+
  ylab("Turnover $ millions")+
  ggtitle("ARIMA Forecast: Best model")

## Prediction interval keeps on increasing
######################################################
