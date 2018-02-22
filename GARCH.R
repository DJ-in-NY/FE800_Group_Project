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
u<-sum(XLE_return)/n                  # ���ֵ
e<-sqrt(sum((XLE_return-u)^2)/(n-1))  # ���׼��
s<-sum((XLE_return-u)^3)/((n-1)*e^3)  # ��ƫ��
k<-sum((XLE_return-u)^4)/((n-1)*e^4)  # ����
jarque.bera.test(XLE_return)          # JB��̬�Լ���

###�����ʲ�����̬�ֲ���������ƫ�ֲ������и߷��β������

par(mfrow=c(2,1))   
acf(XLE_return,main='',xlab='Lag(a)',ylab='ACF',las=1)    #�������ͼ 
title(main='(a) the ACF of Return',cex.main=0.95)         #Ϊͼ�μӱ��⣬�����ñ����С
pacf(XLE_return,main='',xlab='Lag(b)',ylab='PACF',las=1)  #��ƫ�����ͼ   
title(main='(b) the PACF of Return',cex.main=0.95)

###����ͼ�󲿷ֺ���ֵ�����������ڣ�ͼ����ɫ��������������Ծ,��������������������Ժܵ�,����˵���к������������

par(mfrow=c(2,1))  
XLE_return_square<-XLE_return^2
acf(XLE_return_square,main='',xlab='Lag(c)',ylab='ACF',las=1)               
title(main='(a) the ACF of Return Square',cex.main=0.95)
pacf(XLE_return_square,main='',xlab='Lag(d)',ylab='PACF',las=1)    
title(main='(b) the PACF of Return Square',cex.main=0.95)

#���ܹɼ����������е� ACF ֵ��ʾ�����������,��������ƽ���� ACF ֵ ȴ���ֳ���һ��������Ժͳ����ԣ���󲿷�ֵ����������������
#ע�⵽������ƽ���� ACF ֵ���ͺ�2,8,10,14,31�ں��л���˥��,˵���˷������о���һ���̶ȵ����������
#��˲��� GARCH ģ���������ɼ۲��������е���������

library(zoo)
library(FinTS)   #LM����
ArchTest(XLE_return,lag=12)  #�ͺ� 12 ��

#������Ϊ����ͳ������ֵΪ318.18����Ӧ�� P ֵ����Ϊ0��Ҳ����˵�� 1% ��������ˮƽ�Ͼܾ�ԭ���裬
#�Ӷ��ܾ������� ARCH ЧӦ�ļ��裬���������д��� ARCH ЧӦ�����Խ��� GARCH ģ�͵����

fit1 <- auto.arima(XLE_return, trace = TRUE, test = "kpss", ic = "bic" )

## Best model: ARIMA(0,0,0) with zero mean  
Box.test(fit1$residuals^2,lag = 12, type = "Ljung-Box")

m1<-garchFit(~1+garch(1,1),data=XLE_return,trace=F) #���GARCH��1,1��ģ��
m2<-garchFit(~1+garch(1,2),data=XLE_return,trace=F) #���GARCH��1,2��ģ��
m3<-garchFit(~1+garch(2,1),data=XLE_return,trace=F) #���GARCH��2,1��ģ��
m4<-garchFit(~1+garch(2,2),data=XLE_return,trace=F) #���GARCH��2,2��ģ��
summary(m1)
summary(m2)
summary(m3)
summary(m4)

#-5.960207 -5.949151 -5.960215 -5.956151 
#-5.959203 -5.945382 -5.959215 -5.954133
#-5.960240 -5.946419 -5.960252 -5.955169
#-5.964482 -5.947897 -5.964500 -5.958397

#����Ϣ׼���ֵ���Կ�������ϵ� 4 �� GARCH ģ�͵� AIC��BIC��SIC��HQIC ��ֵ
#�������Ų�������,ģ�͵�AIC,BIC�ȵ�ֵ��û���������ӡ�������ѡ�������ģ��GARCH(1,1)��ģ�����

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


