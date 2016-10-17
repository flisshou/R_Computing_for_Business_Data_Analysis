#1.1
install.packages("coefplot")
library(coefplot)


###1.2
1+2+3
3*7*2
6+7*3/2
(4*6)+5
4*(6+5)
17/5
#
17%%5
17%/%5

round(98.562,1)
round(98.562,2)
round(1778,-2)
round(1234,-3)

help(round)

signif(79843.597,6)
signif(79843.597,3)

sin(pi)
cos(pi)
ceiling(pi)
floor(pi)
trunc(pi)

x<-2
3->z
y=5
rm(x)
x
rm(list=ls())

##1.3
x=c(1,2,3,4)
x=1:4
x=seq(1,4,by=1)

unique(x)
length(x)

sort(x,decreasing=FALSE)
sort(x,decreasing=TRUE)

x
x<=2
x>2
any(x<2)
all(x>2)
which(x==2)
which(x!=2)

x[1]
x[1:2]
x[c(1,4)]

x[which(x>=2 & x<=3)]
x[which(x>=2 | x<=3)]

x=rep(0,4)
x
x=x+(1:4)
x

log(x)

factorial(x)
min(x)
max(x)
sum(x)

cumsum(x)
cumprod(x)
cummax(x)
cummin(x)

X=x^2

rm(X)
X=c("Hello","R","World")
length(X)
nchar(X)

##1.4
A=matrix(1:15,nrow=5)
A
rm(A)
A=matrix(1:15,nrow=5,dimnames=list(c("r1","r2","r3","r4","r5"),
c("c1","c2","c3")))
A
nrow(A)
ncol(A)
dim(A)
colnames(A)
rownames(A)

head(A,n=2)
tail(A,n=2)

A[c(3,5),c(1,3)]

B=matrix(16:30,nrow=5)

A*B       #Do A-factor and B-factor multiplication
A%*%t(B)  #transpose B; Do real matrx multiplication
t(B)

cbind(A,B)
rbind(A,B) #

row.means=apply(B,1,mean)#apply(matrix,(1=row, 2=column),function)
col.means=apply(A,2,mean)

##Optional
theArray=array(1:12,dim=c(2,3,2))
theArray

theArray[1,,]
theArray[1,,1]
theArray[,,1]

list(1,2,3)
list(c(1,2,3))

list5=list(A,B,1:10,list(c(1,2,3),3:7))
list5
names(list5)=c("Amat","Bmat","vector","list2")
list5
list5[[1]]
list5[[1]][,2]
list5[[1]][,2,drop=FALSE]

dim(list5[[1]][,2])
dim(list5[[1]][,2,drop=FALSE])

##
results=read.table("d:/results.txt",header=T)


##1.5
##
rm(list=ls())
results=read.table("c:/results.txt",header=T)
results$arch1[5]
#
attach(results)
names(results)
arch1[5]
#
data1=data.frame(x=1:10,y=21:30,z=5:14)
data2=data.frame(x=1:5,y=21:25,z=5:1)
attach(data1)
x
detach(data1)
attach(data2)
x
data2$x
#

theDF=data.frame(10:1,-4:5,letters[1:10])
#
colnames(theDF)=c("First","Second","Third")
letters
#
#

##1.6
rm(list=ls())
x=sample(x=1:100,size=100,replace=T)
x
mean(x)
y=x
y[sample(x=100,size=20,replace=F)]=NA
y
mean(y)
mean(y,na.rm=T)
is.na(y)
which(is.na(y)==1)

#
weights=sample(1:100,100,replace=F)/sum(1:100)
weights
sum(weights)
weighted.mean(x,w=weights)
#
var(x)
sd(x)
#
summary(x)


##
##1.7
groupsizes=c(18,30,32,10,10)
labels=c("A","B","C","D","E")
pie(groupsizes,labels,col=c('grey','yellow','blue',"green","red"))

x=sample(1:100,20,replace=T)
y=sample(1:100,20,replace=T)
plot(x,y)
lines(sort(x), sort(y))
#lines(sort(x),sort(y),type='l')
lines(sort(x), sort(y), col='red', lwd=2)#還有lty參數改變線的類型
#
results=read.table("d:/results.txt",header=T)
attach(results)
names(results)

boxplot(prog1,xlab="Programming Semester 1")
boxplot(arch1~gender,xlab="Architecture Semester 1",xlab="gender",
ylab="Marks(%)")

stem(prog1)
pairs(results[,2:5])

bins=c(0,40,60,80,100)
hist(arch1,breaks=bins,xlab="Marks(%)",ylab="Number of Students",
main="Architecture Semester 1")
#
#
dev.new()
x11(width=12,height=5)
par(mar=c(3,4,1,1))
par(mfrow=c(2,2))
hist(arch1,xlab="Architecture", main="Semester 1",ylim=c(0,35))
hist(arch2,xlab="Architecture", main="Semester 2",ylim=c(0,35))
hist(prog1,xlab="Programming", main="Semester 1",ylim=c(0,35))
hist(prog2,xlab="Programming", main="Semester 2",ylim=c(0,35))



