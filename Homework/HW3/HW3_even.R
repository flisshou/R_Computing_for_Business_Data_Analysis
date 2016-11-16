###############################################################################
#Q2. (20%)
#A drawer of socks contains seven black socks, eight blue socks, and nine
#green socks. There is NO difference between left and right for those socks.
#Two socks are chosen in the dark.
###############################################################################
#(a) What is the exact probability that they match (i.e., rwo socks have the 
#same color)? What is the exact probability that a black pair is chosen?
socks.total = 7 + 8 + 9
match.socks = (choose(7, 2) + choose(8, 2) + choose(9, 2)) / choose(socks.total, 2)
match.black.socks = choose(7, 2) / choose(socks.total, 2)

print(match.socks)
print(match.black.socks)
###############################################################################
#(b) Design a simulation experiment, which repeats the random process of
#choosing two socks for 5,000 times.
#What is the simulated probability that they match? What is the simulated 
#probability that a black pair is chosen?
f.socks.sim = function (times, token) {
  s1 = 0; s2 = 0; s3 = 0;
  
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
###############################################################################
###############################################################################
#Q4. (20%) Simulating Blackjack
###############################################################################
#(a) In the poker game Blackjack, let each of the 4 aces denote 11 points and
#each of the 16 cards >= 10 (i.e., 10, J, Q, K) denote 10 points. Suppose someone
#picks 2 cards randomly out of a deck of cards (52 cards total), what is the
#probability of getting 21 points?
#Use the choose function to obtain the answer.
denominator = choose(52, 2)
classical = choose(4, 1) * choose(16, 1) / denominator
print(classical)
###############################################################################
#(b) Now, let the number 1-52 represent a deck of cards. Assign the numbers 1-4 
#to the four aces and the 37-52 to the 16 cards > 10.
#Use the sample function and the for loop to simulate the random draw of 2 cards
#50,000 to obtain the relative probability. Is the relative probability close to
#the classical one from part (a)?
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

print(paste0("Classical Probability minus Relative Probability is :: " , blackjack - relative))
###############################################################################
#What will happen to the relative probability if you only simulate the game for
#50 times?
relative.50 = f.bj.sim(50) / 50
print(relative.minor)

print(paste0("Classical Probability minus Relative Minor Probability is :: ", blackjack - relative.50))
###############################################################################













