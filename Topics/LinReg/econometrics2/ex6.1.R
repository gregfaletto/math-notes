# setwd("/Users/gfaletto/Desktop/econometrics")

rm(list=ls())
dev.off()

library("ggplot2")

data <- read.table("TrainExer61.txt",
	header=T)

n <- nrow(data)

### Part (a)

names <- colnames(data)

data <- data.frame(1:n, data)

colnames(data) <- c("t", names)

plot.1 <- ggplot(data, aes(x=t, y=X)) + geom_point()

plot.2 <- ggplot(data, aes(x=t, y=Y)) + geom_point()

plot.3 <- ggplot(data, aes(x=X, y=Y)) + geom_point()

print(plot.1)

print(plot.2)

print(plot.3)

# These three graphs could easily lead you to conclude
# that X and Y are correlated even though they're
# clearly not.

# (b)

model.b <- lm(EPSY ~ EPSX, data=data)

print("Model of EPSY ~ EPSX:")

print(summary(model.b))

print("t-value of coefficient when EPSY is regressed against EPSX:")

print(summary(model.b)$coefficients["(Intercept)",
	"t value"])

print("")

print("p-value of coefficient:")

print(summary(model.b)$coefficients["(Intercept)",
	"Pr(>|t|)"])

# (c)

print("")
print("")
print("")

print("Model of EPSY ~ EPSX, lagged values:")

EPSY.lag1 <- data$EPSY[3:(n-1)]

EPSY.lag2 <- data$EPSY[2:(n-2)]

EPSY.lag3 <- data$EPSY[1:(n-3)]

EPSX.lag1 <- data$EPSX[3:(n-1)]

EPSX.lag2 <- data$EPSX[2:(n-2)]

EPSX.lag3 <- data$EPSX[1:(n-3)]

EPSX.c <- data$EPSX[4:n]

EPSY.c <- data$EPSY[4:n]


data.c <- data.frame(EPSY.lag1, EPSY.lag2, EPSY.lag3, 
	EPSX.lag1, EPSX.lag2, EPSX.lag3, EPSX.c, EPSY.c)

model.c <- lm(EPSY.c ~., data=data.c)

print(summary(model.c))

#### F-TEST:







# (d)

model.d <- lm(Y ~ X, data=data)

print("")
print("")
print("")

print("Model of Y ~ X:")

print(summary(model.d))

print("t-value of coefficient when Y is regressed against X:")

print(summary(model.d)$coefficients["(Intercept)",
	"t value"])

print("")

print("p-value of coefficient:")

print(summary(model.d)$coefficients["(Intercept)",
	"Pr(>|t|)"])

# If we didn't know how the data were generated, it would
# seem very clear that Y and X must be correlated.

# (e)

e_t <- model.d$residuals[2:n]

e_t.lag <- model.d$residuals[1:(n-1)]

data.e <- data.frame(e_t, e_t.lag)

model.e <- lm(e_t ~ e_t.lag, data=data.e)

print("")
print("")
print("")

print("Model of e_t ~ e_t-1:")

print(summary(model.e))

The standard assumption of regression that is
violated for this regression is the assumption that
