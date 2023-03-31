data.text=read.table("C:/Users/af44m/OneDrive/Desktop/Analytics/Auto-HM1-2022.txt")
names(data.text)
dim(data.text)
mean(data.text$weight, na.rm = T)
sd(data.text$weight, na.rm = T)
var(data.text$weight, na.rm = T)
names (data.text)
hist(data.text$horsepower, breaks = 10)
plot(data.text$horsepower,data.text$weight)
plot(data.text$weight,data.text$year)
boxplot(data.text$horsepower, data.text$origin)


