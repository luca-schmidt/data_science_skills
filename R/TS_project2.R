##### Project 2: Group 21 (Luca Schmidt, Jonas Enders) #####

##### Question 1 #####

library(forecast);library(lmtest)

#simulation 1
set.seed(2018); epsilon1=rnorm(250,mean=0,sd=sqrt(0.5))
sim1=rep(0,250)
sim1[1]=epsilon1[1]
for(t in 1:249){sim1[t+1]=sim1[t]+epsilon1[t+1]}
ts.plot(sim1)
summary(sim1)

#simulation 2
set.seed(1998); epsilon2=rnorm(250,mean=0,sd=sqrt(0.5))
sim2=rep(0,250)
sim2[1]=epsilon2[1]
for(t in 1:99){sim2[t+1]=sim2[t]+epsilon2[t+1]}
for(t in 100){sim2[t+1]=10+sim2[t]+epsilon2[t+1]}
for(t in 101:249){sim2[t+1]=sim2[t]+epsilon2[t+1]}
ts.plot(sim2)
summary(sim2)

noshock2=rep(0,250)
noshock2[1]=epsilon2[1]
for(t in 1:249){noshock2[t+1]=noshock2[t]+epsilon2[t+1]}
ts.plot(sim2,col="red");lines(noshock2,col="black",lty=2)
ts.plot(sim2[90:120],col="red"); lines(noshock2[90:120],col="black",lty=2)

#simulation 3
set.seed(1997); epsilon3=rnorm(250,mean=0,sd=sqrt(0.5))
sim3=rep(0,250)
sim3[1]=epsilon3[1]
for(t in 1:249){sim3[t+1]=0.6*sim3[t]+epsilon3[t+1]}
ts.plot(sim3)
summary(sim3)

#simulation 4
set.seed(1994); epsilon4=rnorm(250,mean=0,sd=sqrt(0.5))
sim4=rep(0,250)
sim4[1]=epsilon4[1]
for(t in 1:98){sim4[t+1]=0.6*sim4[t]+epsilon4[t+1]}
for(t in 99){sim4[t+1]=10+0.6*sim4[t]+epsilon4[t+1]}
for(t in 100:249){sim4[t+1]=0.6*sim4[t]+epsilon4[t+1]}
ts.plot(sim4)
summary(sim4)

noshock4=rep(0,250)
noshock4[1]=epsilon4[1]
for(t in 1:249){noshock4[t+1]=0.6*noshock4[t]+epsilon4[t+1]}
ts.plot(sim4,col="red");lines(noshock4,col="black",lty=2)
ts.plot(sim4[90:120],col="red"); lines(noshock4[90:120],col="black",lty=2)

#forecast (sim1): 10 steps ahead forecast starting at 50 (not asked in exercise)
model1<-arima(sim1[1:50],order=c(1,0,0),include.mean = FALSE)
coeftest(model1)
fore1<-forecast(sim1[1:50],model=model1)
plot(fore1); lines(sim1[1:60],lty=2)

#forecast (sim1): recursive forecast (without recursive model adaption)
model1rec=arima(sim1[1:50],order=c(1,0,0),include.mean = F)
coeftest(model1rec)
fore1rec=rep(0,250)
for(t in 51:250){
  fore1rec[t]=forecast(sim1[1:(t-1)],h=1,model=model1rec)$mean
}
ts.plot(sim1,col="red",main="Recursive forecast of simulation 1")
lines(c(rep(NA,50),fore1rec[51:250]),col="blue")
legend(x=0,y=15,legend=c("sim1","forecast of sim1"),col=c("red","blue"),lty=c(1,1))
# !!! uses same model the whole time 

#forecast (sim1): recursive forecast, trying to adapt the estimated model each period
fore1rec=rep(0,250)
for(t in 51:250){
  model1rec=ar(sim1[1:t],n.ahead=1)
  fore1rec[t]=predict(object=model1rec,n.ahead = 1)
}
ts.plot(sim1,col="red")
lines(c(rep(NA,50),fore1rec[51:250]))
# does not work; we cannot find a different solution
# arima function cannot be used to fit data, because phi=1
# thus at some point non-stationarity detected and arima function stopped (arima=stationary)

#forecast (sim2): recursive forecast (without recursive model adaption)
model2rec=arima(sim2[1:50],order=c(1,0,0),include.mean = FALSE)
coeftest(model2rec)
fore2rec=rep(0,250)
for(t in 51:250){
  fore2rec[t]=forecast(sim2[1:(t-1)],h=1,model=model2rec)$mean
}
ts.plot(sim2,col="red",main="Recursive forecast of simulation 2")
lines(c(rep(NA,50),fore2rec[51:250]),col="blue")
legend(x=0,y=15,legend=c("sim2","forecast of sim2"),col=c("red","blue"),lty=c(1,1))

#recursive forecast of sim3
fore3rec=rep(0,250)
for(t in 51:250){
  model3rec=arima(sim3[1:t],order=c(1,0,0),include.mean = FALSE)
  fore3rec[t]=forecast(sim3[1:(t-1)],h=1,model=model3rec)$mean
}
ts.plot(sim3,col="red",main="Forecast of sim3")
lines(c(rep(NA,50),fore3rec[51:250]),col="blue")
legend(x=80,y=-1.8,legend=c("sim3","forecast of sim3"),col=c("red","blue"),lty=c(1,1))
model3rec
# parameter overestimated
arima(sim3[1:250],order=c(1,0,0),include.mean = FALSE)
arima(sim3[1:250],order=c(1,0,0),include.mean = FALSE)
# fit this model for more intervals (decrease back limit of interval)
# => estimation draws nearer to actual parameter

# recursive forecast of sim4
fore4rec=rep(0,250)
for(t in 51:250){
  model4rec=arima(sim4[1:t],order=c(1,0,0),include.mean = FALSE)
  fore4rec[t]=forecast(sim4[1:(t-1)],h=1,model=model4rec)$mean
}
ts.plot(sim4,col="red",main="Forecast of sim4")
lines(c(rep(NA,50),fore4rec[51:250]),col="blue")
legend(x=0,y=8,legend=c("sim4","forecast of sim4"),col=c("red","blue"),lty=c(1,1))
model4rec
#parameter underestimated
arima(sim4[1:250],order=c(1,0,0),include.mean = FALSE)
arima(sim4[1:200],order=c(1,0,0),include.mean = FALSE)
#fit this model for more intervals (decrease back limit of interval)
# => estimation draws nearer to actual parameter



##### Question 2 #####
remove(list=ls())

library(readr);library(tseries); library(forecast)
data2_1 <- read_table2("project_2_1.txt", col_names = FALSE)

#graphical analysis
m<-as.ts(data2_1[,1])
p<-as.ts(data2_1[,2])
y<-as.ts(data2_1[,3])
r<-as.ts(data2_1[,4])
m_p=m-p
#m: natural log of M1
#p: natural log of NNP price deflator
#y: natural log of NNP
#r: commercial paper rate in percent (annual rate)
ts.plot(m_p,ylim=c(-1,15),ylab="")
lines(y,col="blue")
lines(r,col="red")
legend(x=5,y=15,legend=c("m-p","y","r"),col=c("black","blue","red"),lty=1)

#ADF tests
r.diff=diff(r)
ts.plot(r.diff)
adf.test(r)
adf.test(r.diff)
adf.test(r.diff,k=2)

m_p.diff=diff(m_p)
ts.plot(m_p.diff)
adf.test(m_p)
adf.test(m_p.diff)
adf.test(m_p.diff,k=2)
adf.test(diff(diff(m_p)))
adf.test(diff(diff(m_p)),k=2)

#ADF-GLS test of difference series
library(urca)
r.diff.adfgls<-ur.ers(r.diff,type="DF-GLS",lag.max=10)
summary(r.diff.adfgls)

y.diff<-diff(y)
y.diff.adfgls<-ur.ers(y.diff,type="DF-GLS",lag.max=10)
summary(y.diff.adfgls)
y.2diff.adfgls<-ur.ers(diff(y.diff),type="DF-GLS",lag.max=10)
summary(y.2diff.adfgls)

m_p.diff.adfgls<-ur.ers(m_p.diff,type="DF-GLS",lag.max=10)
summary(m_p.diff.adfgls)

#Engle-Granger test
library(zoo);library(xts);library(egcm)
egtest<-egcm(X=m_p, Y=y,i1test="adf")
summary(egtest)
#not reject H0 in ADF test of residuals => residuals not stationary
# => series m-p and y are not cointegrated
cor(m_p,y)

egtest<-egcm(X=m_p, Y=r,i1test="adf")
summary(egtest)
#not reject H0 in ADF test of residuals => residuals not stationary
# => series m-p and r are not cointegrated
cor(m_p,r)



##### Question 3 #####
remove(list=ls())

library(readr);library(tseries);library(vars);library(egcm)

data_2_2 <- read_delim("C:/Users/lucas/Desktop/Applied Time Series/project 2/project_2_2.txt","\t", escape_double = FALSE, trim_ws = TRUE)
View(data2_2)

quarter<-data_2_2[,1]
consumption<-as.ts(data_2_2[,2])
output<-as.ts(data_2_2[,3])
ts.plot(consumption,ylim=c(280,920),ylab="",main="Consumption and Output")
lines(output,col="blue");
legend(x=5,y=600,legend=c("Output","Consumption"),col=c("blue","black"),lty=1)
ts.plot(consumption)
ts.plot(output,col="blue")

#graph of consumption and (output-500)
ts.plot(consumption); lines(output-500,col="yellow")
# => series start at same starting point; to compare growth more directly

summary(consumption)
summary(output)

#basic analysis of the two series
acf(consumption)
pacf(consumption)
adf.test(consumption)
acf(output)
pacf(output)
adf.test(output)

con.diff<-diff(consumption)
output.diff<-diff(output)
plot(con.diff,ylab="",main="Difference series' of consumption and output")
lines(output.diff,col="red")
legend(x=180,y=4,legend=c("con.diff","output.diff"),col=c("black","red"),lty=1)
adf.test(con.diff)
adf.test(output.diff)
summary(con.diff)
summary(output.diff)
cor(con.diff,output.diff)

#relation of the two series
ccf(data_2_2[,2],data_2_2[,3],main="CCF of consumption and output")
# indicates spurious relationship between output and consumption
#po.test with H0: two series are not cointegrated
po.test(cbind(consumption,output)) #high p-value => cannot reject H0
# however, PO test not necessarily reliable; there could be a lagged relationship
# Engle-Granger test for cointegration
egtest<-egcm(X=consumption, Y=output,i1test="adf")
summary(egtest) #as in PO test: not reject H0; cannot decide against "not cointegrated"

# some definitions
dataQ3<-data.frame(consumption,output)
ts.plot(dataQ3)
acf(dataQ3)
diff.dataQ3<-data.frame(diff(consumption),diff(output))
ts.plot(con.diff,col="black");lines(output.diff,col="blue")
acf(diff.dataQ3)

#VAR model selection
acf(diff.dataQ3)
#both drive each other
pacf(diff.dataQ3)
# look at p=1,2,3,4

var.1<-VAR(diff.dataQ3,p=1)
summary(var.1)  #only significant parameters
acf(resid(var.1))
AIC(var.1)
BIC(var.1)
serial.test(var.1) # serial correlation (of residuals) detected

var.2<-VAR(diff.dataQ3,p=2)
summary(var.2)  #some estimates insignificant => restrict
restr.var.2<-restrict(var.2)
summary(restr.var.2)
AIC(restr.var.2)
BIC(restr.var.2)
serial.test(restr.var.2) # serial correlation (of residuals) detected

var.3<-VAR(diff.dataQ3,p=3)
summary(var.3)  #some estimates insignificant => restrict
restr.var.3<-restrict(var.3)
summary(restr.var.3)
AIC(restr.var.3)
BIC(restr.var.3)
serial.test(restr.var.3) #no serial correlation (of residuals) detected

var.4<-VAR(diff.dataQ3,p=4)
summary(var.4)  #some estimates insignificant => restrict
restr.var.4<-restrict(var.4)
summary(restr.var.4)
AIC(restr.var.4)
BIC(restr.var.4)
serial.test(restr.var.4) #no serial correlation detected (except at 10% significance level)

#seasonal models
seasonal.1<-VAR(diff.dataQ3,p=1,season=4)
summary(seasonal.1)
restrict(seasonal.1)
seasonal.2<-VAR(diff.dataQ3,p=2,season=4)
summary(seasonal.2)
restrict(seasonal.2)
seasonal.3<-VAR(diff.dataQ3,p=3,season=4)
summary(seasonal.3)
restrict(seasonal.3)
seasonal.4<-VAR(diff.dataQ3,p=4,season=4)
summary(seasonal.4)
restrict(seasonal.4)
# for orders 1 to 4 no seasonal estimate is significant

# final model decision: use restr.var.3
restr.var.3

#test stability condition
roots(restr.var.3) # stability fulfilled (all roots inside complex unit circle)

#prediction
sr.pred.restr.var.3<-predict(restr.var.3,n.ahead=4)
sr.pred.restr.var.3
plot(sr.pred.restr.var.3)
plot(sr.pred.restr.var.3,xlim=c(225,235))
con.diff[230]
output.diff[230]
mean(con.diff)
mean(output.diff)

lr.pred.restr.var.3<-predict(restr.var.3,n.ahead=20)
lr.pred.restr.var.3
plot(lr.pred.restr.var.3)
plot(lr.pred.restr.var.3,xlim=c(200,250))