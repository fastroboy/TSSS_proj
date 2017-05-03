# This is the project of TSSS.
# We try to analyse the properties of a stock
# from the perspective of time series.
# HAVE FUN!!!

library(zoo)
library(xts)
library(TTR)
# These 3 packages are dependencies of quantmod:)
library(quantmod)

# CFLD: CHINA FORTUNE LAND DEVELOPMENT CO., LTD.
setSymbolLookup(CFLD=list(name='600340.ss', src='yahoo'))
getSymbols('CFLD')
chartSeries(CFLD)

# Openning & closing price
op <- Op(CFLD)
cl <- Cl(CFLD)
# We focus on the data in the past 365 market days.
op.365 <- tail(op, 365)
cl.365 <- tail(cl, 365)

# Smoothing
# MA smoothers with different window lengths.
ma.w <- filter(cl.365, sides=2, rep(1, 5)/5)
ma.m <- filter(cl.365, sides=2, rep(1, 31)/31)
ma.s <- filter(cl.365, sides=2, rep(1, 91)/91)
par(mfrow=c(2, 2))
plot(cl.365, main='Closing prices\nin last 365 market days')
plot(ma.w, ylab='', xlab='', main='Weekly MA')
plot(ma.m, ylab='', xlab='', main='Monthly MA')
plot(ma.s, ylab='', xlab='', main='Seasonal MA')

# Detrending
# ACF comparison
fit.time <- lm(cl.365~time(cl.365))
par(mfcol=c(3, 1))
acf(cl.365, main='Original')
acf(resid(fit.time), main='Detrended')
acf(na.omit(diff(cl.365)), main='First difference')

# Turning point:P
tp <- '2017-04-05'
tp.ind <- which(index(cl.365)==tp)
before <- head(cl.365, tp.ind)
after <- tail(cl.365, 365-tp.ind)

pacf(cl.365)
regr1 <- arima(cl.365, order=c(1, 0, 0))