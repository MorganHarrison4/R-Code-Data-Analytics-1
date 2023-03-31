#Question 3 Part A

mean = 800
x1 = 1/mean
Probability1 = pexp(900, rate = x1, log.p = FALSE)
Probability2 = pexp(700, rate = x1, log.p = FALSE)

Probability1
Probability2

Answer3A = Probability1 - Probability2


Answer3A

#Question 3 Part B

Probability3 = pexp(850, rate = x1, lower.tail = 850, log.p = FALSE)
Answer3B = 1-Probability3


Answer3B

#Question 3 Part C

Answer3C = qexp(1-0.8, rate = x1, lower.tail = FALSE, log.p = FALSE)


Answer3C

