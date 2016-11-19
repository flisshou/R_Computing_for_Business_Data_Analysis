#Homework 3

##Q1. (25%) Two six-sided fair dices are thrown sequentially, and their face values are recorded.
##(a), (b), (c), & (d) do NOT need to be done in R.

###(a) List the sample space.

```
(1,1)(1,2)(1,3)(1,4)(1,5)(1,6)
(2,1)(2,2)(2,3)(2,4)(2,5)(2,6)
(3,1)(3,2)(3,3)(3,4)(3,5)(3,6)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)(5,5)(5,6)
(6,1)(6,2)(6,3)(6,4)(6,5)(6,6)
```

###(b) List the elements that make up the following events:
###   (1) A=the sum of the two values is at least 5,

```
(1,4)(1,5)(1,6)
(2,3)(2,4)(2,5)(2,6)
(3,2)(3,3)(3,4)(3,5)(3,6)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)(5,5)(5,6)
(6,1)(6,2)(6,3)(6,4)(6,5)(6,6)
```

###  (2) B=the value of the first die is higher than the value of the second,

```
(2,1)
(3,1)(3,2)
(4,1)(4,2)(4,3)
(5,1)(5,2)(5,3)(5,4)
(6,1)(6,2)(6,3)(6,4)(6,5)
```

###  (3) C=the first value is 4.

```
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
```

###(c) List the elements of the following events:
###  (1) A and C

```
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
```

###  (2) B or C

```
(2,1)
(3,1)(3,2)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)
(6,1)(6,2)(6,3)(6,4)(6,5)
```

###  (3) A and (B or C)

```
(3,2)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)
(6,1)(6,2)(6,3)(6,4)(6,5)
```

###(d) Based on the classical approach, derive
### P(A and C)

```
count=0
for(i in 1:6)
{
  if(i+4>=5){
    count=count+1
  }
}
prob1=count/6^2
print(prob1)
```

### P(B or C)

```{R}
count=0
for(i in 1:6)
{
  for(j in 1:6){
    if(i>j||i==4){
      count=count+1
    }
  }
}
prob2=count/6^2
print(prob2)
```

### P(A and C)

```{R}
count=0
for(i in 1:6)
{
  for(j in 1:6){
    if(i>j||i==4){
      if(i+j>=5){
        count=count+1
      }
    }
  }
}
prob3=count/6^2
print(prob3)
```

###(e) Use _sample()_ in _R_ to simulate the throwing of two dice 1,000 times. Compute P(A and C),
###P(B or C), and P(A and (B or C)) from the 1,000 runs. How different are the results from (d)?

```{R}
n=1000
d1=sample(6,n,replace=T)
d2=sample(6,n,replace=T)
result=cbind(d1,d2)
```

---

P(A and C)
```{R}
s1=length(result[(d1==4&d1+d2>=5),1])/n
print(s1)
diff=prob1-s1
print(diff)
```

---

P(B or C)
```{R}
s2=length(result[(d1>d2|d1==4),1])/n
print(s2)
diff=prob2-s2
```

---

P(A and (B or C))
```{R}
s3=length(result[d1+d2>=5&(d1>d2|d1==4),1])/n
print(s3)
diff=prob3-s3
print(diff)
```

---




















d
