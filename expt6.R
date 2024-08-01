library(ISLR)
data("Default")
Default$default<- as.numeric(Default$default=="Yes")
cor_matrix <- cor(Default[, sapply(Default, is.numeric)], use = "pairwise")
cor_matrix
logmodel<-glm(default~balance,family = "binomial",data = Default)
summary(logmodel) 
logmodel1<-glm(default~income,family = "binomial",data = Default)
summary(logmodel1)
logmodel2<-glm(default~student,family = "binomial",data = Default)
summary(logmodel2) 
logmodel3<-glm(default~balance+income+student,family = "binomial",data = Default)
summary(logmodel3)  
Default$default<- as.numeric(Default$default=="Yes")
ggplot(data=Default,mapping=aes(y=default,x=balance))+geom_point(alpha=0.2)+
geom_smooth(method="glm", se = FALSE, method.args = list(family="binomial"))
sample<-sample(c(TRUE,FALSE),nrow(Default),replace=T,prob=c(0.6,0.4))
train=Default[sample,]
TEST=Default[!sample,]
tr=glm(default~balance,family = "binomial",train)
summary(tr)
pre=predict(tr,TEST,type="response")
pr=ifelse(pre>0.5,1,0)
lm=table(predicted=pr,actual=TEST$default)
lm
