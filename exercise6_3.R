#exercise 6.3
#189110H - K.A.D.Y.L Kuruppuachchi

hmv = read.csv("Home Market Value.csv",header = TRUE)

hmv$Market.Value = as.numeric(gsub("[\\$,]","", hmv$Market.Value))
hmv$Square.Feet = as.numeric(gsub("[\\,]","", hmv$Square.Feet))

hmv

summary(hmv)




cor(hmv)

z= hmv$Market.Value
y= hmv$House.Age
x = hmv$Square.Feet


## check whether if outlier exists

# using scatter plot
plot(x,z,col="Blue")
plot(y,z,col="Blue")

# using histogram
par(mfrow=c(1,3))
hist(y, ylab="Frequency", xlab = "House Age", main = "Age Vs Frequency", col=rainbow(8))
hist(x, ylab="Frequency", xlab = "House Area", main = "Area Vs Frequency", col=rainbow(10))
hist(z, ylab="Frequency", xlab = "Market Value", main = "Market Value Vs Frequency", col=rainbow(10))
par(mfrow=c(1,1))

# using boxplot
par(mfrow=c(1,3))
boxplot(y, xlab="House Age", ylab="Years", col = "Blue")
boxplot(x, xlab="House Area", ylab="Squre Feets", col = "Green")
boxplot(z, xlab="House Market Value", ylab="$", col = "Red")
par(mfrow=c(1,1))


hmvClean = hmv[hmv$Square.Feet < 2200,]
hmvClean = hmvClean[hmvClean$Market.Value < 110000,]
nrow(hmvClean)

summary(hmvClean)
# After removing outliers

z= hmvClean$Market.Value
y= hmvClean$House.Age
x = hmvClean$Square.Feet


sd(x, na.rm = FALSE)
sd(y, na.rm = FALSE)
sd(z, na.rm = FALSE)

#Corvariance
cor(hmvClean)


#histogram

par(mfrow=c(1,3))
hist(y, ylab="Frequency", xlab = "House Age", main = "Age Vs Frequency", col=rainbow(8))
hist(x, ylab="Frequency", xlab = "House Square Feet", main = "Area Vs Frequency", col=rainbow(12))
hist(z, ylab="Frequency", xlab = "Market Value", main = "Market Value Vs Frequency", col=rainbow(10))
par(mfrow=c(1,1))

# using boxplot
par(mfrow=c(1,3))
boxplot(y, xlab="House Age", ylab="Year", col = "Blue")
boxplot(x, xlab="House Square Feet", ylab="Squre Feet", col = "Green")
boxplot(z, xlab="House Market Value", ylab="$", col = "Red")
par(mfrow=c(1,1))

# using scatter plot
#par(mfrow=c(1,2))
plot(x,z,col="Blue")
plot(y,z,col="Blue")
#par(mfrow=c(1,1))

#skewness
library(e1071) 
skewness(x)
skewness(z)


lm.hmv = lm(z~x+y)
summary(lm.hmv)

newSqureFeet = c(1650,1500,1800,2200,2400)
newHouseAge = c(26,28,29,30,31)

newData = data.frame(x=newSqureFeet, y = newHouseAge)
hmv.pre = predict(lm.hmv, newData, level = 0.95, interval = "confidence")

hmv.pre



plot(x,z,col="Blue")
#points(newX,y.pre[,1], pch = 16)
#abline(lm.hmv)

