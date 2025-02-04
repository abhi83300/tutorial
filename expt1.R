sales<-read.csv("C:/Users/student/Downloads/yearly_sales.csv")

str(sales)
head(sales)
summary(sales)
plot(sales$cust_id,sales$sales_total,main="cust_id VS.sales total")
plot(sales$num_of_orders,sales$sales_total,main="Number of orders VS.Sales")
setwd("C:/Users/student/Desktop/PKD20IT053")
sales<-read.csv("yearly_sales.csv")
sales$per_order<-sales$sales_total/sales$num_of_orders
write.table(sales,"sales_modified.txt",sep="\t",row.names=FALSE)
read.delim("sales_modified.txt")
sales[,4]
sales$gender
sales[1:2,]
sales[,c(1,3,4)]
sales[,c("cust_id","sales_total")]
sales[sales$gender=="F",]
sales_group<-vector(mode="character",length=length(sales$sales_total))
sales_group[sales$sales_total<100]<-"small"
sales_group[sales$sales_total>=100&sales$sales_total<500]<-"medium"
sales_group[sales$sales_total>=500]<-"big"
sales_group[sales$sales_total<100]<-"small"
sales_group[sales$sales_total>=100&sales$sales_total<500]<-"medium"
spender<-factor(sales_group,levels=c("small","medium","big"),ordered=FALSE)
summary(spender)
salesgender<-vector(mode="character",length=length(sales$gender))
salesgender[sales$gender=="F"]<-"female"
salesgender[sales$gender=="M"]<-"male"
spenders<-factor(salesgender,levels=c("female","male"),ordered=FALSE)
summary(spenders)
sales<-vector(mode="character",length=length(sales$gender))
sales[sales$gender=="F"]<-"female"
sale[sales$gender=="M"]<-"male"
spenders<-factor(sales,levels=c("female","male"),ordered=FALSE)
summary(spenders)
salesgender<-vector(mode="character",length=length(sales$gender))
salesgender[sales$gender=="F"]<-"female"
salesgender[sales$gender=="M"]<-"male"
sales$gender<-factor(salesgender,levels=c("female","male"),ordered=FALSE)
summary(sales$gender)
sales$per_order<-sales$sales_total/sales$num_of_orders
sales<-cbind(sales,spender)
str(sales$spender)
summary(sales_table)
x<-sales$sales_total
y<-sales$num_of_orders
cor(x,y)
cor(sales$sales_total,sales$per_order)
cor(sales$sales_total,sales$per_order)
cor(sales$per_order,sales$sales_total)
cor(sales$per_order,sales$num_of_orders)
cov(sales$sales_total,sales$per_order)
IQR(sales$sales_total)
x<-rnorm(50)
y<-x+rnorm(50,mean=0,sd=0.5)
data<-as.data.frame(cbind(x,y))
summary(data)
plot(x,y)
x<-c(1,2,3,NA,4)
is.na(x)
mean(x)
mean(x,na.rm=TRUE)
df<-data.frame(x=c(1,2,3),y=c(10,20,NA))
df
df<-na.exclude(df)
