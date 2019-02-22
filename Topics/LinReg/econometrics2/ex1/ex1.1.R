# setwd("/Users/gregfaletto/Documents/R/Econometrics/ex1")

library(ggplot2)

rm(list=ls())

data <- read.table("TrainExer11.txt", header=T)

n <- dim(data)[1]
factors <- dim(data[2])

# Part (a): histograms, scatter plot

hist_exp <- ggplot(data=data, aes(Expenditures)) +
	geom_histogram(bins=n)
hist_age <- ggplot(data=data, aes(Age)) +
	geom_histogram(bins=n)
scatter <- ggplot(data=data, aes(x=Age, y=Expenditures)) +
	geom_point()

print(hist_exp)
print(hist_age)
print(scatter)

# Parts (d) and (e): histograms, scatter plot

mean.exp <- mean(data[, 3])


forty.and.over <- data[which(data$Age>=40), ]
under.forty <- data[which(data$Age<40), ]

mean.forty.and.over <- mean(forty.and.over[, 3])
mean.under.forty <- mean(under.forty[, 3])

print(paste("Mean expenditures:", mean.exp))
print(paste("Mean expenditures for clients >=40:", mean.forty.and.over))
print(paste("Mean expenditures for clients <40:", mean.under.forty))

# Part (f): predictions

model <- lm(Expenditures ~ Age, data=data)

line.plot <- ggplot(data=data, aes(x=Age, y=Expenditures)) +
	geom_point() + geom_abline(slope=model$coefficients[2],
		intercept=model$coefficients[1], color="red")

print(line.plot)

fifty_pred <- predict.lm(model, data.frame(Age=50))[[1]]
twentyfive_pred <- predict.lm(model, data.frame(Age=25))[[1]]

print(paste("Predicted daily expenditures for a 50-year-old client:",
	fifty_pred))

print(paste("Predicted daily expenditures for a 25-year-old client:",
	twentyfive_pred))

print(paste("r^2 for the line of best fit:", summary(model)$r.squared))