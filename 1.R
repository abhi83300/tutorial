adver <- read.csv("C:/Users/student/Downloads/advertising.csv")
mod1=lm(Sales~TV,adver)
summary(mod1)
mod2=lm(Sales~Radio,adver)
summary(mod2)
mod3=lm(Sales~Newspaper,adver)
summary(mod3)
CTR=lm(Sales~TV+Radio,adver)
summary(CTR)
CTRN=lm(Sales~TV+Radio+Newspaper,adver)
summary(CTRN)
cor(adver$TV,adver$Sales)


