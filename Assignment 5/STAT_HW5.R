## Part A.
## 2.a)
cellphone <- read.table("CellPhoneRL.txt", header = TRUE)
Y=cellphone$y
S=cellphone$S
C=cellphone$C
fit=aov(Y~S*C)
anova(fit)

## 2.b)
TukeyHSD(fit)

## 2.d)
interaction.plot(C, S, Y, xlab = "Type of Cellphone", ylab = "SAR Level (W/kg)", trace.label = "Signal Strength", main = "Interaction Plot for Cellphone Radiation Data")
interaction.plot(S, C, Y, xlab = "Signal Strength", ylab = "SAR Level (W/kg)", trace.label = "Cellphone Type", main = "Interaction Plot for Cellphone Radiation Data")

## residuals plot
plot(fit, which = 1)

#Normal Q-Q Plot
plot (fit, which = 2)
qqline(rstandard(fit), col = "RED", lwd = 2)


## Part B
## 3.d)
reactions = read.table("PilotReacTimes.txt", header = TRUE)
attach(reactions)
fit = aov(times ~ design + pilot, data = reactions)
anova(fit)


## Part C
TukeyHSD(fit, "design", conf.level=0.99)

