data=read.csv("C:/Users/pc/Downloads/CAPM DATA.CSV")
data
summary(data)
str(data)
data$risk_permium=data$mkt-data$riskfree
str(data)
View(data)
dis.fit=lm(dis~riskfree+risk_permium,data=data)
ge.fit=lm(ge~riskfree+risk_permium,data=data)
gm.fit=lm(gm~riskfree+risk_permium,data=data)
ibm.fit=lm(ibm~riskfree+risk_permium,data=data)
msft.fit=lm(msft~riskfree+risk_permium,data=data)
xom.fit=lm(xom~riskfree+risk_permium,data=data)
dis.fit
ge.fit
gm.fit
ibm.fit
msft.fit
xom.fit
stocks=c("dis","ge","gm","ibm","msft","xom")
expcted.return=c(dis.fit$coefficients[3],
                 ge.fit$coefficients[3],
                 gm.fit$coefficients[3],
                 ibm.fit$coefficients[3],
                 msft.fit$coefficients[3],
                 xom.fit$coefficients[3])
dif=data.frame(stocks,expcted.return)
dif
install.packages("colorspace")
library(ggplot2)
ggplot(data=data,aes(y=msft,x=risk_permium))+geom_point(col='red')+
  xlab("risk_premium")+
  ylab("expected return")+
  ggtitle("microsoft stock return")+geom_abline(methods="lm")

dis.predict=predict.lm(dis.fit)
ge.predict=predict.lm(ge.fit)
gm.predict=predict.lm(gm.fit)
ibm.predict=predict.lm(ibm.fit)
msft.predict=predict.lm(msft.fit)
xom.predict=predict.lm(xom.fit)

data$dis.predict=dis.predict
data$ge.predict=ge.predict
data$gm.predict=gm.predict
data$ibm.predict=ibm.predict
data$msft.predict=msft.predict
data$xom.predict=xom.predict

msft.plot=ggplot(data,aes(y=msft.predict,x=risk_permium))+
  geom_smooth(method='lm',col='blue')+
  geom_line(aes(y=msft,x=risk_permium),col='red')+
  ggtitle('microsoft')+
  xlab('risk premium')+
  ylab('expected return')
msft.plot
