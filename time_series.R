sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)

tsales <- ts(sales, start=c(2003, 1), frequency=12)
tsales


plot(tsales)

start(tsales)
end(tsales)
frequency(tsales)

tsales.subset <- window(tsales, start=c(2003, 5), end=c(2004, 6))
tsales.subset


#########################################
# ma 移动平均
library(forecast)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw time series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

###########################################
## 分解
plot(AirPassengers)
lAirPassengers <- log(AirPassengers) ## 对数化 相乘模型
plot(lAirPassengers, ylab="log(AirPassengers)")
fit <- stl(lAirPassengers, s.window="period")  # s.windows="periodic"可使得季节效应在各年间都一样
plot(fit)
fit$time.series
exp(fit$time.series)


#########################################
## 3种拟合模型
ets(ts, model="ANN")  ## 单指数 水平项 
ses(ts)
ets(ts, model="AAN")  ## 双指数 水平项、斜率
holt(ts)
ets(ts, model="AAA")  ## 三指数 水平项、斜率、季节项
hw(ts)




