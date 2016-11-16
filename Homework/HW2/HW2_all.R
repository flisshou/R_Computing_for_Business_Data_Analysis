#Q1. (10%) The Fibonacci sequence is famous in mathematics.
# (a) Write a while( ) loop to find the first Fibonacci number k > greater than 100.
fib=function(n){
  if(n>2){
    return(fib(n-1)+fib(n-2))
  }else{
    return(1)
  }
}

n=1
while(fib(n)<=100){
 n=n+1
}
print(fib(n))

#(b) For the number Fn=k in (a), what is the index n?
print(n)

# (c)Use for( ) to write a function print.Fib 
#that takes an integer k as its input and prints ALL Fibonacci numbers <= k. 
#Test k=100 and show me the results.

print.Fib=function(k){
  for(i in 1:n){
    if(fib(i)<k){
      print(fib(i))
    }
  }
}
print(print.Fib(100))

###########################################################################
#Q2. Write a function sencond.smallest() that takes a vector x as its input.
#The function will return the number that is the second smallest inside x.
#Show the results of second.smallest(x=c(2, 8, 8, 2, 5, 2, 5, 2)).

x = c(2, 8, 8, 2, 5, 2, 5, 2)

second.small = function (input) {
  output <- unique(sort(input, decreasing = FALSE))
  return(output[2])
}

second.small(x)

##########################################################################
#Q3. (10%) Use while( ) and/or if...else to write a function f.exist 
#that takes an integer z and a vector x as its inputs. 
#The function f.exist will return TRUE only if z is inside x.
#Test f.exist(z=10, x=c(1:10)) and f.exist(z=10, x=c(9, 3, 1)). Show the answers.

f.exist=function(z,x){
  if(any(z==x)){
    return('true')
  }
  else{
    return('false')
  }
}
print(f.exist(z=10, x=c(1:10)))
print(f.exist(z=10, x=c(9, 3, 1)))
#############################################################################
#Q4. Use while() and/or if...else to write a function f.divide that takes an
#integer z as its input. The function f.divide will return how many divisors
#z has(other than 1 & z itself).
#Test f.divide(100) and show the results.

f.divide = function (n) {
  index = 2
  result = c()
  
  while (index < n) {
    if(n %% index == 0) {
      result = append(result, index)
    }
    index = index + 1
  }
  #print(result)
  return(length(result))
}

f.divide(100)

############################################################################
#Q5. (10%) Write a function UNIQUE( ) that takes a vector x as its input.
#The function will return a new vector with all unique numbers in x with duplicated elements removed. 
#Do NOT use unique( ) in R. Show me the results of UNIQUE(x=c(2, 8, 8, 2, 5, 2, 5, 2)).

UNIQUE=function(x){
  x=c(x[which(!duplicated(x))])
  return(x)
}
print(UNIQUE(x=c(2, 8, 8, 2, 5, 2, 5, 2)))
#############################################################################
#Q6. The Babylonian method is famous for getting the square root of any number.
#Suppose we have a positive number S, the Babylonian method suggests that
#X_(n+1) = 0.5(X_n + S / X_n)
#X_n is your current guess of S^(1/2) and X_(n+1) is your next guess of S^(1/2).
#You will STOP searching only if |X_(n+1) - X_n| < tolerance.
#Now, set S = 125348, your initial guess X_0 = 600, and tolerance = 1e-5/
#Use while() to implement the algorithm and show the square root you find.
#The answer should be 354.0452.
#How about S = 9526, X_0 = 87, and tolerance = 1e-5? What's the answer?
#How about S = 5566, X_0 = 78, and tolerance = 1e-5? What's the answer?

tolerance = 10^(-5)

babylonian = function (S, X) {
  Y = 0.5*(X + S/X)
  
  while(abs(Y-X) >= tolerance) {
    X = Y
    Y = 0.5*(X + S/X)
  }
  return(X)
}

#Case 0:
S0 = 125348
X0 = 600
babylonian(S0, X0)  

#Case 1:
S1 = 9526
X1 = 87
babylonian(S1, X1)
#sqrt(S1)

#Case 2:
S2 = 5566
X2 = 78
babylonian(S2, X2)
#sqrt(S2)

###########################################################################
#Q7. (25%) Write a function BesselI_Gen that has five arguments (a, v, z, max, tolerance).

BesselI_Gen=function(a,v,z,max,tolerance){
  
  m=0
  val.last=-Inf
  val=(gamma(m+a+1)*factorial(m))^-v*(z/2)^(2*m+a)
  while(m<max&&abs(val-val.last)>tolerance){
    val.last=val
    m=m+1
    val=val.last+val
  }
  
  return(val)
}

BesselI_Gen(5, 1, 10, 1000, 1e-5)
besselI(10, 5)

