## Problem 3b.)
sl=read.table(“SleepRem.txt”, header = TRUE)
anova(aov(sl$values~sl$ind))

rem_aov<-aov(sl$values~sl$ind)
resids<-rem_aov$resid
shapiro.test(resids)

boxplot(resids, main = “Box Plot of Residuals”)

qqnorm(resids, main = “Normal Q-Q Plot”)
qqline(resids, col = “RED”, lwd = 2)

## Problem 6a.)
pc = read.table("PorousCarbon.txt", header = T)
carbon_aov = aov(pc$values~as.factor(pc$temp))
anova(carbon_aov)

carbon_aov <-aov(pc$values~pc$temp)
resids <- carbon_aov$resid
shapiro.test(resids)

boxplot(resids, main = "Box Plot of Residuals")

stdresids=rstandard(carbon_aov)
qqnorm(stdresids, main = "Normal Q-Q Plot")
qqline(stdresids, col = "RED", lwd = 2)

##in addition
fit=fitted.values(carbon_aov)
plot(residuals, fit)


## Part B - Problem 4
## 4.a)
pc=read.table("PorousCarbon.txt", header = TRUE)
TukeyHSD(aov(pc$values~as.factor(pc$temp)), conf.level=0.95)
plot(TukeyHSD(aov(pc$values~as.factor(pc$temp)), conf.level=0.95))


a=subset(pc, temp=="300")
u1=mean(a$values)
u1

b=subset(pc, temp == "400")
u2=mean(b$values)
u2

c=subset(pc, temp == "500")
u3=mean(c$values)
u3

d=subset(pc, temp == "600")
u4=mean(d$values)
u4


