ad <- read.csv("~/DA lab/advertising.csv")
View(ad)
lm1=lm(Sales~TV,ad)
summary(lm1)
library(ggplot2)
ggplot(data = ad,mapping = aes(x=TV,y=Sales))+geom_point(alpha=0.2)+geom_smooth(method = "lm")
plot(x=ad$TV,y=ad$Sales)
sample<-sample(c(TRUE,FALSE),nrow(ad),replace = T,prob = c(0.6,0.4))
train<-ad[sample,]
test=ad[!sample,]
t<-lm(Sales~TV,train)
summary(t)
pre<-predict(t,test,type="response")

rmse=sqrt(mean((test$Sales-pre)^2))
r2=1-mean((test$Sales-pre)^2)/mean((test$Sales-mean(test$Sales))^2)
rmse
r2
cor(ad,use="pairwise")

cor(ad$Sales,ad$TV)

cov(ad$Sales,ad$TV)
plot(ad$TV,ad$Sales)
