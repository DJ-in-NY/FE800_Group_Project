library(quantmod)
start_date = as.Date('01/01/10', "%m/%d/%y")
end_date = as.Date('01/31/18', "%m/%d/%y")
getSymbols(Symbols = 'XLE', from = start_date, to = end_date)
barChart(XLE[, c(6,5)],theme='white.mono',bar.type='hlc')
chartSeries(XLE, subset='last 3 months', theme = chartTheme('white'))
XLE_return = Delt(x1 = XLE[, 6,], k = 1, type = 'arithmetic')
XLE_return = XLE_return[2:2033]

####### part 2########
####### GARCH
library(tseries)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)
library(rugarch)
library(forecast)

n=length(XLE_return)
u<-sum(XLE_return)/n                  # 求均值
e<-sqrt(sum((XLE_return-u)^2)/(n-1))  # 求标准差
s<-sum((XLE_return-u)^3)/((n-1)*e^3)  # 求偏度
k<-sum((XLE_return-u)^4)/((n-1)*e^4)  # 求峰度
jarque.bera.test(XLE_return)          # JB正态性检验

###收益率不是正态分布，呈现左偏分布，且有高峰厚尾的现象

par(mfrow=c(2,1))   
acf(XLE_return,main='',xlab='Lag(a)',ylab='ACF',las=1)    #画自相关图 
title(main='(a) the ACF of Return',cex.main=0.95)         #为图形加标题，并设置标题大小
pacf(XLE_return,main='',xlab='Lag(b)',ylab='PACF',las=1)  #画偏自相关图   
title(main='(b) the PACF of Return',cex.main=0.95)

###两个图大部分函数值在置信区间内（图中蓝色虚线区域）上下跳跃,所以收益率序列自相关性很低,或者说具有很弱的自相关性

par(mfrow=c(2,1))  
XLE_return_square<-XLE_return^2
acf(XLE_return_square,main='',xlab='Lag(c)',ylab='ACF',las=1)               
title(main='(a) the ACF of Return Square',cex.main=0.95)
pacf(XLE_return_square,main='',xlab='Lag(d)',ylab='PACF',las=1)    
title(main='(b) the PACF of Return Square',cex.main=0.95)

#尽管股价收益率序列的 ACF 值揭示了其弱相关性,但收益率平方的 ACF 值 却表现出了一定的相关性和持续性，其大部分值都超过了置信区间
#注意到收益率平方的 ACF 值在滞后2,8,10,14,31期后都有缓慢衰退,说明了方差序列具有一定程度的序列相关性
#因此采用 GARCH 模型来描述股价波动过程中的条件方差

library(zoo)
library(FinTS)   #LM检验
ArchTest(XLE_return,lag=12)  #滞后 12 期

#检验结果为卡方统计量的值为318.18，对应的 P 值几乎为0，也就是说在 1% 的显著性水平上拒绝原假设，
#从而拒绝不存在 ARCH 效应的假设，收益率序列存在 ARCH 效应，可以进行 GARCH 模型的拟合

fit1 <- auto.arima(XLE_return, trace = TRUE, test = "kpss", ic = "bic" )

## Best model: ARIMA(0,0,0) with zero mean  
Box.test(fit1$residuals^2,lag = 12, type = "Ljung-Box")

m1<-garchFit(~1+garch(1,1),data=XLE_return,trace=F) #拟合GARCH（1,1）模型
m2<-garchFit(~1+garch(1,2),data=XLE_return,trace=F) #拟合GARCH（1,2）模型
m3<-garchFit(~1+garch(2,1),data=XLE_return,trace=F) #拟合GARCH（2,1）模型
m4<-garchFit(~1+garch(2,2),data=XLE_return,trace=F) #拟合GARCH（2,2）模型
summary(m1)
summary(m2)
summary(m3)
summary(m4)

#-5.960207 -5.949151 -5.960215 -5.956151 
#-5.959203 -5.945382 -5.959215 -5.954133
#-5.960240 -5.946419 -5.960252 -5.955169
#-5.964482 -5.947897 -5.964500 -5.958397

#由信息准则的值可以看出，拟合的 4 个 GARCH 模型的 AIC、BIC、SIC、HQIC 的值
#发现随着参数增加,模型的AIC,BIC等的值并没有显著增加。理论上选择最简洁的模型GARCH(1,1)建模最合适

res_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(0,0)))
res_garch11_fit <- ugarchfit(spec = res_garch11_spec, data = XLE_return)
res_garch11_fit


ctrl <- list(tol=1e-7, delta = 1e-9)
res_garch11_roll <- ugarchroll(res_garch11_spec, XLE_return, n.start = 120, refit.every = 1,
                               refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                               VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl,
                               fit.control = list(scale = 1))

report(res_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
plot(res_garch11_fit)

res_var <- zoo(res_garch11_roll@forecast$VaR[,1])
index(res_var) <- as.yearmon(rownames(res_garch11_roll@forecast$VaR))
res_actual <- zoo(res_garch11_roll@forecast$VaR[,2])
index(res_actual) <- as.yearmon(rownames(res_garch11_roll@forecast$VaR))
par(mfrow=c(1,1))
plot(res_actual, type = "b", main = "VaR Backtesting at 99%", xlab = "Date",ylab  = "VaR%" )

lines(res_var,col = "green")

res_garch11_fcst = ugarchforecast(res_garch11_fit, n.ahead=12)
res_garch11_fcst



