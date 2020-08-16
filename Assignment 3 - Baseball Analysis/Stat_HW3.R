## Problem 3
## a.)
baseball <- read.csv("Baseball-Salary-Data.csv", header = TRUE)
ls(baseball)
baseball$player <- NULL
par(mfrow = c(1, 2))
hist(baseball$salary, main = "Salary")
fit <- lm((salary)~., data = baseball)
head(baseball[, 14:17])
summary(fit)

## f.)
fit_part_f <- lm((salary) ~. - batting.average - on.base.percent - hits - doubles -
                   triples, data = baseball)

summary(fit_part_f)

## i.) residuals plot
resids <- fit$residuals
preds <- fit$fitted.values
plot(baseball$salary, resid, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot for Error Variance Checking")

## ii. kernel density estimate of the residuals
plot(density(resids), xlab = "Residuals", ylab = "Density", main = "Kernel Density Estimate of Residuals")

## iii. Q-Q plot of the standardized residuals
stdresids = rstandard(fit)
qqnorm(stdresids, main = "Normal Q-Q Plot")
qqline(stdresids, col = "RED", lwd = 2)


## Problem 4
## a.)

library(leaps)

y = (baseball$salary)
X = baseball[,2:17]

out = leaps(X, y, method = 'r2', nbest = 1)

aic = 1:16
bic = 1:16
for(j in 1:16) {
  vec=(1:16)[out$which[j,] == TRUE]
  
  Data = baseball[, c(1, vec+1)]
  
  fit = lm((salary) ~., data = Data)
  aic[j] = AIC(fit)
  bic[j] = AIC(fit, k = log(nrow(baseball)))
}

cbind(aic, bic)

## choosing model based on aic
min_aic_degree <- which.min(aic)
min_aic_degree

variable_mask <- out$which[min_aic_degree, ]
varnames <- colnames(baseball)
varnames[2:length(varnames)][variable_mask]

leaps_X <-X[, variable_mask]
leaps_model_aic <- lm(y ~., data = leaps_X)
summary(leaps_model_aic)

leaps_X2 <- subset(leaps_X, select = - walks)
leaps_model_aic2 <- lm(y ~., data = leaps_X2)
summary(leaps_model_aic2)

leaps_X3 <- subset(leaps_X2, select = - arbitration)
leaps_model_aic3 <- lm(y ~., data = leaps_X3)
summary(leaps_model_aic3)

##choosing model based on bic
min_bic_degree <- which.min(bic)
Min_bic_degree

bic_variable_mask <- out$which[min_bic_degree, ]
varnames <- colnames(baseball)
varnames[2:length(varnames)][variable_mask]

bic_leaps_X <- X[, variable_mask]
leaps_model_bic <- lm(y ~., data = leaps_X)
summary(leaps_model_bic)


## part b.)
resids <- leaps_model_aic3$residuals
preds <- leaps_model_aic3$fitted.values
stdresids = rstandard(leaps_model_aic3)

start = 1
index <- seq(start, 337, 1)

plot(index, stdresids, xlab = "Indexed Player ID", ylab = "Standardized Residuals", main = "Standardized Residuals vs Player IDs")

low_index <- which(stdresids < -3)
baseball[low_index, ]

## no residuals less than -3

high_index <- which(stdresids > 3)
baseball[high_index, ]

## part d.)
qqnorm(stdresids, main = "Normal Q-Q Plot")
qqline(stdresids, col = "RED", lwd = 2)

## part e.)
cdists <- cooks.distance(leaps_model_aic3)
plot(cdists, ylab = "Cook's D", main = "Checking for Outliers in Data")


