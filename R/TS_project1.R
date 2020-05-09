library(chron); library(lmtest); library(tseries); library(TSA); library(forecast); library(xts)

Database_2018_3 <- read.csv2("C:/Users/lucas/Desktop/Applied Time Series/project 1/Database_2018_3.csv")
 View(Database_2018_3)
con <- Database_2018_3$RCON12Q2


##Description of Time Series
ts.plot(con,ylab="Consumption Expenditure (in billions of real dollars)",xlab="Time",main="Real Personal Consumption Expenditures")
ts.plot(log(con),ylab="Logarithm of Consumption Expenditure",xlab="Time",main="")
summary(con)
sd(con)


##Model Selection - General
acf(con,main="")
pacf(con)
adf.test(con)


##difference of time series
ts.plot(diff(con),main="difference of 'con'")
acf(diff(con),lag=50,main="")
pacf(diff(con),main="")

arima1.1.0 <- arima(con,order=c(1,1,0),method = "ML")
arima1.1.0
coeftest(arima1.1.0)
Box.test(arima1.1.0$residuals,lag=10,type="Ljung-Box",fitdf = 1)
#parameter is significant, but residuals correlated

arima3.1.2<-arima(con,order=c(3,1,2),method="ML")
coeftest(arima3.1.2)
Box.test(arima3.1.2$residuals,lag=10,type="Ljung-Box",fitdf = 5)
AIC(arima3.1.2)
BIC(arima3.1.2)
#parameters are not significant, H0 of Box.test not rejected


##second difference
con.2diff=diff(diff(con))
ts.plot(con.2diff,main="Second difference of 'con'")
adf.test(con.2diff)
acf(con.2diff,main="ACF(con.2diff)")
acf(con.2diff,lag=50)   #to check behaviour for higher number of lags
pacf(con.2diff,main="PACF(con.2diff)")

#ARIMA(2,2,1)
arima2.2.1 <- arima(con,order=c(2,2,1),method="ML")
AIC(arima2.2.1)
BIC(arima2.2.1)
coeftest(arima2.2.1)
Box.test(arima2.2.1$residuals,lag=10,type="Ljung-Box",fitdf=3)
# all parameters significant, and H0 in Box.test not rejected
# short-list this model

#ARIMA(3,2,2)
arima3.2.2 <- arima(con,order=c(3,2,2),method="ML")
AIC(arima3.2.2)
BIC(arima3.2.2)
coeftest(arima3.2.2)
Box.test(arima3.2.2$residuals,lag=10,type="Ljung-Box",fitdf=5)
# not all parameters are significant, H0 in Box.test not rejected

#The following models (until logarithm) are not specifically mentioned or presented in the report.
arima1.2.0<-arima(con,order=c(1,2,0),method="ML")
AIC(arima1.2.0)
BIC(arima1.2.0)
coeftest(arima1.2.0)
Box.test(arima1.2.0$residuals,lag=10,type="Ljung-Box",fitdf=1)
# parameter is significant, but residuals are correlated

arima1.2.1<-arima(con,order=c(1,2,1),method="ML")
AIC(arima1.2.1)
BIC(arima1.2.1)
coeftest(arima1.2.1)
Box.test(arima1.2.1$residuals,lag=10,type="Ljung-Box",fitdf=2)
# parameters are significant, but residuals are correlated

arima1.2.2<-arima(con,order=c(1,2,2),method="ML")
AIC(arima1.2.2)
BIC(arima1.2.2)
coeftest(arima1.2.2)
Box.test(arima1.2.2$residuals,lag=10,type="Ljung-Box",fitdf=3)
# produces NA for standard deviation, coefficient tests not possible

arima2.2.2<-arima(con,order=c(2,2,2),method="ML")
AIC(arima2.2.2)
BIC(arima2.2.2)
coeftest(arima2.2.2)
Box.test(arima2.2.2$residuals,lag=10,type="Ljung-Box",fitdf=4)
# not all parameters significant, H0 in Box.test not rejected

arima3.2.1<-arima(con,order=c(3,2,1),method="ML")
AIC(arima3.2.1)
BIC(arima3.2.1)
coeftest(arima3.2.1)
Box.test(arima3.2.1$residuals,lag=10,type="Ljung-Box",fitdf=4)
# not all parameters are significant, H0 in Box.test not rejected

arima3.2.3<-arima(con,order=c(3,2,3),method="ML")
AIC(arima3.2.3)
BIC(arima3.2.3)
coeftest(arima3.2.3)
Box.test(arima3.2.3$residuals,lag=10,type="Ljung-Box",fitdf=6)
# not all parameters significant, H0 in Box.test not rejected

arima4.2.2<-arima(con,order=c(4,2,2),method="ML")
AIC(arima4.2.2)
BIC(arima4.2.2)
coeftest(arima4.2.2)
Box.test(arima4.2.2$residuals,lag=10,type="Ljung-Box",fitdf=6)
# not all parameters significant, H0 in Box.test not rejected


##take logarithm of data
con.log <- log(con)
ts.plot(con.log,main="log(con)")
adf.test(con.log)
con.log.diff <- diff(con.log)
ts.plot(con.log.diff, main="diff(log(con))")
adf.test(con.log.diff)
acf(con.log.diff,main="ACF(con.log.diff)")
pacf(con.log.diff,main="PACF(con.log.diff)")
#from pacf: order p is 2 or 4 (or 1 or 3)
#from acf: order q is 2 (or 1)

#Not all of the following models are presented or specifically mentioned in the report.
log.arima2.1.1<-arima(con.log,order=c(2,1,1))
AIC(log.arima2.1.1)
BIC(log.arima2.1.1)
coeftest(log.arima2.1.1)
checkresiduals(log.arima2.1.1)
#parameters significant but residuals correlated

log.arima2.1.2<-arima(con.log,order=c(2,1,2))
AIC(log.arima2.1.2)
BIC(log.arima2.1.2)
coeftest(log.arima2.1.2)
checkresiduals(log.arima2.1.2)
#parameters not (all) significant and residuals correlated

log.arima3.1.1<-arima(con.log,order=c(3,1,1))
AIC(log.arima3.1.1)
BIC(log.arima3.1.1)
coeftest(log.arima3.1.1)
checkresiduals(log.arima3.1.1)
#significant parameters but correlated residuals

log.arima3.1.2<-arima(con.log,order=c(3,1,2))
AIC(log.arima3.1.2)
BIC(log.arima3.1.2)
coeftest(log.arima3.1.2)
checkresiduals(log.arima3.1.2)
#mostly significant parameters but correlated residuals

log.arima4.1.1<-arima(con.log,order=c(4,1,1))
AIC(log.arima4.1.1)
BIC(log.arima4.1.1)
coeftest(log.arima4.1.1)
checkresiduals(log.arima4.1.1)
#mostly significant parameters but correlated residuals

log.arima4.1.2<-arima(con.log,order=c(4,1,2))
AIC(log.arima4.1.2)
BIC(log.arima4.1.2)
coeftest(log.arima4.1.2)
checkresiduals(log.arima4.1.2)
#only some parameters significant and correlated residuals

log.arima1.1.1<-arima(con.log,order=c(1,1,1))
AIC(log.arima1.1.1)
BIC(log.arima1.1.1)
coeftest(log.arima1.1.1)
checkresiduals(log.arima1.1.1)
# parameters significant but correlated residuals

log.arima1.1.2<-arima(con.log,order=c(1,1,2))
AIC(log.arima1.1.2)
BIC(log.arima1.1.2)
coeftest(log.arima1.1.2)
checkresiduals(log.arima1.1.2)
# only some parameters significant and correlated residuals

log.arima1.1.3<-arima(con.log,order=c(1,1,3),method="ML")
AIC(log.arima1.1.3)
BIC(log.arima1.1.3)
coeftest(log.arima1.1.3)
checkresiduals(log.arima1.1.3)
#significant parameters and non-correlation of residuals not rejected
#short-list this model

log.arima1.1.4<-arima(con.log,order=c(1,1,4),method="ML")
AIC(log.arima1.1.4)
BIC(log.arima1.1.4)
coeftest(log.arima1.1.4)
checkresiduals(log.arima1.1.4)
#mostly significant parameters but correlated residuals


##forecast
#error [discussed with Prof Gao, no solution found to straigthen out error, thus forecast by hand using predict function]
forecast(log.arima1.1.3,h=10)

#forecast for log model
pre.A <- predict(log.arima1.1.3,n.ahead=20)
ts.plot(pre.A$pred)
graphA <- c(con.log,pre.A$pred)
ts.plot(graphA,col="blue")
lines(con.log)

#forecast for d=2 model
pre.B <- predict(arima2.2.1,n.ahead=20)
pre.B$pred
ts.plot(pre.B$pred)
graphB <- c(con,pre.B$pred)
ts.plot(graphB,col="red")
lines(con)

#forecast for both models in one graph
ts.plot(graphB,col="red",ylab="",main="Forecasts for ARIMA(1,1,3) of logarithmic data and
        ARIMA(2,2,1)",xlim=c(200,280),ylim=c(5000,11000))
pre.A.absolute<-c(exp(pre.A$pred[1]),exp(pre.A$pred[2]),exp(pre.A$pred[3]),exp(pre.A$pred[4]),
                  exp(pre.A$pred[5]),exp(pre.A$pred[6]),exp(pre.A$pred[7]),exp(pre.A$pred[8]),
                  exp(pre.A$pred[9]),exp(pre.A$pred[10]),exp(pre.A$pred[11]),exp(pre.A$pred[12]),
                  exp(pre.A$pred[13]),exp(pre.A$pred[14]),exp(pre.A$pred[15]),exp(pre.A$pred[16]),
                  exp(pre.A$pred[17]),exp(pre.A$pred[18]),exp(pre.A$pred[19]),exp(pre.A$pred[20]))
graphC <- c(con,pre.A.absolute)
lines(graphC,col="blue")
lines(con)

#forecast for shorter period
graphD <- c(con,pre.B$pred[1:8])
ts.plot(graphD,col="red",ylab="",main="Forecast for shorter period",xlim=c(200,260),ylim=c(5000,11000))
pre.A.absolute2<-c(exp(pre.A$pred[1]),exp(pre.A$pred[2]),exp(pre.A$pred[3]),exp(pre.A$pred[4]),
                  exp(pre.A$pred[5]),exp(pre.A$pred[6]),exp(pre.A$pred[7]),exp(pre.A$pred[8]))
graphE <- c(con,pre.A.absolute2)
lines(graphE,col="blue")
lines(con)

#general pattern for forecast
forecast(model,h=20)   #model not generated
plot(forecast(model,h=20))