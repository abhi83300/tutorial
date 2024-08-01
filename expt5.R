
Credit$Gender<-factor(Credit$Gender,levels=c("Male","Female"),ordered =FALSE)
r<-lm(Balance~Gender,Credit)
summary(r)
