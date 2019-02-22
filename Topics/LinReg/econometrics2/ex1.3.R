# setwd("/Users/gfaletto/Desktop/econometrics")

rm(list=ls())

library("ggplot2")

data <- read.table("TrainExer13.txt", header=T)

####### Part (a)

# Calculate b

G.bar <- mean(data$Game)
W.bar <- mean(data$Winning.time.men)
n <- nrow(data)

b <- (data$Game-G.bar)%*%(data$Winning.time.men-W.bar)/((data$Game-G.bar)%*%(data$Game-G.bar))
print(paste("b=", b))

# Calculate a

# W.bar = a + G.bar*b

a <- W.bar-b*G.bar
print(paste("a=", a))

# Check by re-calculating using built-in lm() function

model <- lm(Winning.time.men ~ Game, data=data)

print(paste("According to the built-in linear regression function lm(), a =",
	as.numeric(model$coefficients[1]), "and b =",
	as.numeric(model$coefficients[2]), "."))

print(paste("In a, difference of",
	a-as.numeric(model$coefficients[1])))
print(paste("In b, difference of",
	b-as.numeric(model$coefficients[2])))

# Calculate r^2

sum.squared.resids <- (data$Winning.time.men-predict(model))%*%(data$Winning.time.men-predict(model))

W.var <- (data$Winning.time.men-W.bar)%*%(data$Winning.time.men-W.bar)

r.squared <- 1-(sum.squared.resids/W.var)

print(paste("r^2 =", r.squared))

print(paste("According to the built-in lm() function, r^2 =",
	summary(model)$r.squared))

print(paste("Difference of",
	r.squared-summary(model)$r.squared))

# Calculate s

s <- sqrt((1/(n-2))*sum.squared.resids)

print(paste("s =", s))
print(paste("According to the built-in lm() function, s =",
	summary(model)$sigma))
print(paste("Difference of ", s-summary(model)$sigma))

# Plot of residuals against year

residuals.df <- data.frame(data$Game,
	as.numeric(data$Winning.time.men-predict(model)))
colnames(residuals.df) <- c("Game", "Residual")
plot <- ggplot(data=residuals.df, aes(x=Game, y=Residual)) +
geom_point() + geom_smooth(method="lm", se=F)
print(plot)

model.resids <- lm(Residual ~ Game, data=residuals.df)
print("Line of best fit for the residuals:")
print(model.resids)

# Part (b)

# This model seems to be reasonably useful for predicting
# the winning time based on year, because the r^2 value is
# fairly large at about 0.673. Because the residuals seem 
# to be homoscedastic, linear, and centered at 0, it seems 
# like a linear model is a reasonable fit for the data.

# Part (c)

# Predictions by "hand"

predictions <- sapply(c(16, 17, 18), function(x) {a+b*x})
predictions <- data.frame(c(2008, 2012, 2016), predictions)
colnames(predictions) <- c("Year",
	"Expected.Winning.Time.Men")
print("My predictions:")
print(predictions)

# Predictions using linear model

predictions.lm <- as.numeric(predict.lm(model,
	data.frame(Game=c(16, 17, 18))))
predictions.lm <- data.frame(c(2008, 2012, 2016),
	predictions.lm)
colnames(predictions.lm) <- c("Year",
	"Expected.Winning.Time.Men")
print("Predictions using built-in model:")
print(predictions.lm)

print("Differences:")
print(predictions[, 2]-predictions.lm[,2])

