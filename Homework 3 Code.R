library(ISLR)
head(Carseats)

carseats.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.1)
contrasts(Carseats$Urban)
contrasts(Carseats$US)

carseats.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.2)

par(mfrow = c(2,2))
plot(carseats.1)

par(mfrow = c(2,2))
plot(carseats.2)

confint(carseats.2)