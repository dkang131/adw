setwd('C:/Users/darre/Desktop/SEM 7/ADW/Post Midterm')

library(forecast)
library(caret)
#install.packages('fpp')
library(fpp)
library(ggplot2)

return = read.table('monthly-simple-returns.txt', header = F)
head(return)
attach(return)

win.graph()
ts.plot(V2)
par(mfrow=c(2,1))
acf(V2, lag.max = 60)
pacf(V2, lag.max = 60)

diff_1 = diff(V2, 1)
acf(diff_1, lag.max = 60)
pacf(diff_1, lag.max = 60)

diff_12 = diff(diff_1, 12)
acf(diff_12, lag.max = 60)
pacf(diff_12, lag.max = 60)

#Train Test
set.seed(123)
train = sample(seq(nrow(return)), size = floor(0.75*nrow(return)), replace = F)
train_data = return[train,]
test_data = return[-train,]

#### ARIMA(0,1,1)(0,1,1,12) ####
model1_1 <- function(x,h){
  forecast(Arima(x, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12),
                 lambda = 0),h = h)
}

e <- tsCV(train_data$V2, model1_1, h = 12)
rmse <- sqrt(mean(e^2, na.rm=TRUE));rmse

sma1_1 = Arima(train_data$V2, order = c(0,1,1),
                seasonal = list(order = c(0,1,1), period = 12), lambda = 0)
summary(sma1_1)
plot(forecast(sma1_1, h = 12))

#### ARIMA(0,1,2)(0,1,1,12) ####
sma1_2 = Arima(V2, order = c(0,1,2),
                seasonal = list(order = c(0,1,1), period = 12), lambda = 0)
summary(sma1_2)

model1_2 <- function(x,h){
  forecast(Arima(x, order = c(0,1,2), seasonal = list(order = c(0,1,1), period = 12),
                 lambda = 0),h = h)
}
e1 <- tsCV(V2, model1_2, window = 24, h = 12)
rmse1 <- sqrt(mean(e1^2, na.rm=TRUE));rmse1


#### AUTO ARIMA ####
data_2 = ts(train_data$V2, frequency = 12)
summary(auto.arima(data_2,d=1,D=1))

auto_arima <- function(x,h){
  forecast(auto.arima(x, d = 1, D=1), h = h)
}

e2 <- tsCV(data_2, auto_arima, window = 24, h = 6)
rmse2 <- sqrt(mean(e2^2, na.rm = TRUE));rmse2
