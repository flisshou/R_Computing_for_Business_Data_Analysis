#Homework 3

###Q1. (25%) Two six-sided fair dices are thrown sequentially, and their face values are recorded. (a), (b), (c), & (d) do NOT need to be done in R.

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
(1) A=the sum of the two values is at least 5,

```
(1,4)(1,5)(1,6)
(2,3)(2,4)(2,5)(2,6)
(3,2)(3,3)(3,4)(3,5)(3,6)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)(5,5)(5,6)
(6,1)(6,2)(6,3)(6,4)(6,5)(6,6)
```

(2) B=the value of the first die is higher than the value of the second,

```
(2,1)
(3,1)(3,2)
(4,1)(4,2)(4,3)
(5,1)(5,2)(5,3)(5,4)
(6,1)(6,2)(6,3)(6,4)(6,5)
```

(3) C=the first value is 4.

```
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
```

###(c) List the elements of the following events:
(1) A and C

```
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
```

(2) B or C

```
(2,1)
(3,1)(3,2)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)
(6,1)(6,2)(6,3)(6,4)(6,5)
```

(3) A and (B or C)

```
(3,2)
(4,1)(4,2)(4,3)(4,4)(4,5)(4,6)
(5,1)(5,2)(5,3)(5,4)
(6,1)(6,2)(6,3)(6,4)(6,5)
```

###(d) Based on the classical approach, derive
P(A and C)

```{R}
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

P(B or C)

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

P(A and C)

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

###(e) Use _sample()_ in _R_ to simulate the throwing of two dice 1,000 times. Compute P(A and C), P(B or C), and P(A and (B or C)) from the 1,000 runs. How different are the results from (d)?

```{R}
n=1000
d1=sample(6,n,replace=T)
d2=sample(6,n,replace=T)
result=cbind(d1,d2)
```


P(A and C)
```{R}
s1=length(result[(d1==4&d1+d2>=5),1])/n
print(s1)
diff=prob1-s1
print(diff)
```


P(B or C)
```{R}
s2=length(result[(d1>d2|d1==4),1])/n
print(s2)
diff=prob2-s2
```


P(A and (B or C))
```{R}
s3=length(result[d1+d2>=5&(d1>d2|d1==4),1])/n
print(s3)
diff=prob3-s3
print(diff)
```

---

###Q2. (20%) A drawer of socks contains seven black socks, eight blue socks, and nine green socks. There is NO difference between left and right for those socks. Two socks are chosen in dark.

###(a) What is the _exact probability_ that they match (i.e., two socks have the same color)? What is the _exact probability_ that a black pair is chosen?

```{R}
socks.total = 7 + 8 + 9
match.socks = (choose(7, 2) + choose(8, 2) + choose(9, 2)) / choose(socks.total, 2)
match.black.socks = choose(7, 2) / choose(socks.total, 2)

print(match.socks)
print(match.black.socks)
```

###(b) Design a simulation experiment (Hint: Use _sample()_ in R), which repeats the random process of choosing two socks for 5,000 times.
###What is the _simulated probability_ that they match? What is the _simulated probability_ that a black pair is chosen?

```{R}
f.socks.sim = function (times, token) {
  s1 = 0
  s2 = 0
  s3 = 0

  for (i in 1:times) {
    result = sample(24, 2)

    has.black = all(result >= 1  & result <= 7)
    has.blue  = all(result >= 8  & result <= 15)
    has.green = all(result >= 16 & result <= 24)

    if (has.black) {
      s1 = s1 + 1
    } else if (has.blue) {
      s2 = s2 + 1
    } else if (has.green){
      s3 = s3 + 1
    }
  }

  switch(token,
         'black' = {
           return(s1 / times)
         },
         'blue' = {
           return(s2 / times)
         },
         'green' = {
           return(s3 / times)
         },
         'all' = {
           return((s1 + s2 + s3) / times)
         })
}
f.socks.sim(5000, 'all')
f.socks.sim(5000, 'black')
```

###Q3. (20%) A committee consists of five Chicanos, two Asians, three African Americans, and two Caucasians. A subcommittee of five is chosen at random.
###(a) What is the _exact probability_ that all the ethnic groups are represented on the subcommittee (please mathematically derive the probability)?

```{R}
committee=5+2+3+2
prob=choose(5,1)*choose(2,1)*choose(3,1)*choose(2,1)*choose(committee-4,1)/factorial(2)/choose(12,5)
print(prob)
```

###(b) Design a simulation experiment (Hint: Use _sample()_ in R), which repeats the random process of choosing five guys for 5,000 times. What is the _simulated probability_ of all the ethnic groups are represented on the subcommittee?

```{R}
f.committee.sim=function(times){
  count=0

  for (i in 1:times) {
    result = sample(12,5)

    has.Chicanos=any(result>=1 & result<=5)
    has.Asians=any(result>=6 & result<=7)
    has.AfricanAmericans=any(result>=8 & result<=10)
    has.Caucasians=any(result>=11 & result<=12)

    if (has.Chicanos&has.Asians&has.AfricanAmericans&has.Caucasians) {
      count=count+1
    }
  }
  return(count / times)

}
f.committee.sim(5000)
```

### Is it close to the probability in part (a)?

```{R}
diff=prob-f.committee.sim(5000)
print(diff)#ans:yes
```

---

###Q4. (20%) Simulating Blackjack
###(a) In the poker game Blackjack, let each of the 4 aces denote 11 points and each of the 16 cards >= 10 (i.e., 10, J, Q, K) denote 10 points. Suppose someone picks 2 cards randomly out of a deck of cards (52 cards total), what is the probability of getting 21 points (i.e., Blackjack)?
###Use the _choose_ function to obtain the answer.

```{R}
denominator = choose(52, 2)
classical = choose(4, 1) * choose(16, 1) / denominator
print(classical)
```

###(b) Now, let the numbers 1-52 represents a deck of cards. Assign the numbers 1-4 to the four aces and the numbers 37-52 to the 16 cards > 10.
###Use the _sample_ function and the _for_ loop to simulate the random draw of 2 cards **50,000** times (hint: with or without replacement?). Create a variable _success_ that represents the number of times you hit 21 points (i.e., Blackjack). Divide success by 50,000 to obtain the relative probability. Is the relative probability close to the classical one from part (a)?

```{R}
f.bj.sim = function(times) {
  success = 0

  for (i in 1:times) {
    result  = sample(52, 2)
    has.ace = any(result <= 4)
    has.ten = any(result >= 37 & result <= 52)

    if (has.ace & has.ten) {
      success = success + 1
    }
  }
  return(success / times)
}

relative = f.bj.sim(50000)

print(relative)

print(paste0("Classical Probability minus Relative Probability is :: " , classical - relative))
```


###What will happen to the relative probability if you only simulate the game for 50 times?

```{R}
f.bj.sim(50)
```









d
