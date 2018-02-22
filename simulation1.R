library(quantmod)
library(sde)
library(ggplot2)
library(sde)
library(plotly)
library(dplyr)
library(dygraphs)


getSymbols('AAPL', from = '2017-01-01')
data1 = data.frame(AAPL)    

###calculate sigma
price <- data1$AAPL.Adjusted
n = length(price)
ret <- log(price[-1]/price[-n])
vol <- sd(ret) * sqrt(250) * 100
class(ret)
###calculate expected return
return1 = sum(ret)
###parameters for GBM
mu=return1; sigma=vol/100; P0=40; T = 30 ##1 month
nt=100; n=2^(8)
#############Generate nt trajectories
dt=T/n; t=seq(0,T,by=dt)
X=matrix(rep(0,length(t)*nt), nrow=nt)
for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
##Plot
ymax=max(X); ymin=min(X) #bounds for simulated prices
plot(t,X[1,],t='l',ylim=c(ymin, ymax), col='grey',
     ylab="Price P(t)",xlab="time t")
for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}

dim(X)
X[,257]
X[,1]
simple_return = (X[,257]-X[,1])/X[,1]
sorted = sort(simple_return)
var95 = sorted[0.05*length(sorted)]
var95
simple_return[95]
colMeans(X)
X1 = data.frame(rbind(t, colMeans(X)))
class(X1)
plot = ggplot(data = X1, mapping = aes(x = as.numeric(X1[1,]), y = as.numeric(X1[2,]))) + 
       geom_point()
plot

graphics::plot(x = as.numeric(X1[1,]), y = as.numeric(X1[2,]), type = 'l')

####
dygraph(AAPL) %>%
  dySeries('AAPL.Close')

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)
lungDeaths

dygraph(lungDeaths) %>% dyRangeSelector()

dygraph(AAPL) %>% dyRangeSelector()

getSymbols('AAPL', from = '2017-01-01')
AAPL1 = data.frame(AAPL)
# AAPL1$AAPL.Volume = NULL
# AAPL1$AAPL.Adjusted = NULL
AAPL1[, 5] = NULL
AAPL1[, 5] = NULL
colnames(AAPL1) = c('Open', 'High', 'Low', 'Close')
AAPL2 = as.matrix(AAPL1)
dygraph(AAPL2) %>% 
  dyRangeSelector() %>% 
  dyCandlestick()

library(xts)
data(sample_matrix)
dygraph(sample_matrix) %>%
  dyCandlestick()

##################################################################################

data_1 = data.frame(date=index(AAPL), coredata(AAPL))
daily_return = 100 * (diff(data_1$AAPL.Adjusted)/data_1$AAPL.Adjusted[-length(data_1$AAPL.Adjusted)])
length(daily_return)
c(0, daily_return)
