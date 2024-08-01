library(tree)
library(ISLR)
attach(Carseats)
High=factor(ifelse(Sales <=8,"No","Yes"))
Carseats =data.frame(Carseats ,High)
tree.carseats =tree(High~.-Sales , Carseats)
summary(tree.carseats)



plot(tree.carseats)
text(tree.carseats,pretty = 0)
tree.carseats





set.seed(2)
train1=sample (1: nrow(Carseats ), 300)
Carseats.test=Carseats [-train1 ,]
High.test=High[-train1]
tree.carseats =tree(High~.-Sales , Carseats ,subset=train1)
tree.pred=predict(tree.carseats ,Carseats.test ,type="class")
cm1=table(tree.pred ,High.test)
cm1


accuracy1=sum(diag(cm1)) / sum(cm1)
accuracy1

uyhhh4h




set.seed(3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
prune.carseats =prune.misclass (tree.carseats ,best=21)
plot(prune.carseats)
text(prune.carseats ,pretty=0)
tree.pred1=predict(prune.carseats ,Carseats.test , type="class")
cm2=table(tree.pred1 ,High.test)
accu3=sum(diag(cm2)) / sum(cm2)
accu3
prune.carseats =prune.misclass (tree.carseats ,best=14)
plot(prune.carseats)
text(prune.carseats ,pretty=0)
tree.pred2=predict(prune.carseats ,Carseats.test , type="class")
cm3=table(tree.pred2 ,High.test)
accu4=sum(diag(cm3)) / sum(cm3)
accu4
High.test
