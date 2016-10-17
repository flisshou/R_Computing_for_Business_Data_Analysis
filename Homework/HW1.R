#################################################################
#Q2. (10%) Use rep() and seq() as needed to create the two vectors.

#(a) 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4
c(rep(0, 5), rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5))

#(b) 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
rep(1:5, 5)
#################################################################
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
#################################################################
#Q8. (10%)

#(a) Use R to create a vector that contains all integers from 1 to 100 that are NOT divisible by 2, 3, or 7.
Filter(function(i) { all(i %% c(2,3,7) != 0) }, seq(100))

#(b) Create a 10*10 identity matrix.
diag(10)

#(c) Then use two different ways to make all the non-zero elements 5
#Solution 1:
m1 = diag(10)
for(i in 1:10){
  for(j in 1:10){
    if (i != j){
      m1[i, j] = 5
    }
  }
}
m1

#Solution 2:
A = matrix(rep(5, 10), nrow = 10, ncol = 10 )
B = diag(10) * 4
C = A - B
C
#################################################################
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