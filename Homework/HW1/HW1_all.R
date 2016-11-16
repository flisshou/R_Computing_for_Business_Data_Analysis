#Q1.(10%) Finish the following tasks using R.

#(a)
downtime=c(0,1,2,12,12,14,18,21,21,23,24,25,28,29,30,30,30,33,36,44,45,47,51)

#(b)
mean(downtime)
median(downtime)
min(downtime)
max(downtime)
range(downtime)

#(c)
sd(downtime)
quantile(downtime,probs=seq(0,1,0.05))

#(d)
table(downtime)

#(e)
which.max(table(downtime))
#################################################################
#Q2. (10%) Use rep() and seq() as needed to create the two vectors.

#(a) 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4
c(rep(0, 5), rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5))

#(b) 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
rep(1:5, 5)
#################################################################
#Q3.

#(a)
inputdata=c(61,175,111,124,13,21,24,23,4,18,14,18)
A=matrix(inputdata, ncol = 3,dimnames = list(NULL,c(" x","y","z")))

#(b)
A[1,3]
#################################################################
#Q4. (10%) Calculate Sigma_(i=1)^N(1/i), and compare with log(N)+0.6 for N = 500, 2000, 8000

sum.frac = function (n) {
  result = 0
  for (i in 1:n) {
    result = result + (i/n)
  }
  result
}
log.add06 = function(n) {
  log(n) + 0.6
}
compare = function(n){
  sum.frac(n) > log.add06(n)
}
compare(500)
compare(2000)
compare(8000)
#################################################################
#Q5. 
x=0
count=0
f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
tolerance=0.000001
while(abs(f)>tolerance){
  f.prime=7*x^6+60000*x^5+5.3*x^4+42400*x^3+0.1815*x^2+1210*x+0.0005
  x=x-f/f.prime
  f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
  count=count+1
}


#################################################################
#Q6. (10%) Based on the results.txt file (in Lecture 1), write R code to reproduce the graph below.

result = read.table("/Users/flisshou/Desktop/R_Computing_for_Business_Data_Analysis/results.txt", header = T)
attach(result)
names(result)
names(result)
par(mfrow = c(2, 2))

#arch1
boxplot(arch1~gender, xlab = "gender", main = "Architecture Semester 1")
#arch2
boxplot(arch2~gender, xlab = "gender", main = "Architecture Semester 2")
#arch3
boxplot(prog1~gender, xlab = "gender", main = "Programming Semester 1")
#arch4
boxplot(prog2~gender, xlab = "gender", main = "Programming Semester2")
#################################################################
#Q7.

#(a)
factorial(4)
factorial(50)
factorial(5000)
lfactorial(5000)

#(b)
choose(4,2)
choose(50,20)
choose(5000,2000)
lchoose(5000,2000)

#(c)
sum(log(1:5000))
sum(log(3001:5000)-log(1:2000))

#################################################################
#Q8. (10%)

#(a) Use R to create a vector that contains all integers from 1 to 100 that are NOT divisible by 2, 3, or 7.
Filter(function(i) { all(i %% c(2,3,7) != 0) }, seq(100))

#(b) Create a 10*10 identity matrix.
diag(10)

#(c) Then use two different ways to make all the non-zero elements 5
#Solution 1:
diag(10)*5
#Solution 2:
M1 = diag(10)
for(i in 1:10){
  for(j in 1:10){
    if(i == j){
      M1[i, j] = 5
    }
    
  }
}
M1
#################################################################
#Q9. (10%) Use while( ) to write an R function that prints out all prime numbers <= n(where n is an integer). 
#After writing the function, set n=100 and show me the results.

primes=function(n) {
  result=NULL
  i=2
  while( i <= n){
    if(!any(i%%result==0)){
      result=c(result,i)
    }
    i=i+1
  }
  result
}
primes(100)

#################################################################
#Q10. Consider the function y=f(x) defined by 
#y=f(x)= <1> -x^3, for all x<=0; <2> x^2, for all x belongs to (0,1]; <3> x^(1/2), for all x>1.
#Write an R function to calculate y using if-statements.
#Generate the following plot for x=swq(-2, 2, 0.1).

f = function(x) {
  y = 0
  
  if (x > 1) {
    y = sqrt(x)
  } else if (x <= 0) {
    y = (-1) * x^3
  } else{
    y = x^2
  }
  
  y
}

datas = seq(-2, 2, 0.1)
results = rep(0, length(datas))
for (i in 1:length(datas)){
  results[i] = f(datas[i])
}

plot(datas, results, type = 'l', xlab = 'x', ylab = 'f(x)')
#################################################################