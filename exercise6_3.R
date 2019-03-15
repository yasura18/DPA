hmv = read.csv("Home Market Value.csv",header = TRUE)

hmv$Market.Value = as.numeric(gsub("[\\$,]","", hmv$Market.Value))
hmv$Square.Feet = as.numeric(gsub("[\\,]","", hmv$Square.Feet))

hmv

summary(hmv)




cor(hmv)

z= hmv$Market.Value
y= hmv$House.Age
x = hmv$Square.Feet

sd(x, na.rm = FALSE)
sd(y, na.rm = FALSE)


# lm.hmv = lm(hmv$Market.Value ~ hmv$Square.Feet + hmv$House.Age)

lm.hmv = lm(z~x+y)
summary(lm.hmv)

newSqureFeet = c(1650,1500,1800,2200,2400)
newHouseAge = c(26,28,29,30,31)

newData = data.frame(x=newSqureFeet, y = newHouseAge)
hmv.pre = predict(lm.hmv, newData, level = 0.95, interval = "confidence")

hmv.pre

#install.packages("rgl")

# s=interp(x,y,z,duplicate="strip")
# surface3d(s$x,s$y,s$z,color="blue")
# points3d(s)

plot(x,z,col="Blue")
#points(newX,y.pre[,1], pch = 16)
#abline(lm.hmv)

