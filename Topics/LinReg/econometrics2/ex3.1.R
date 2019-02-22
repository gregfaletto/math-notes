# setwd("/Users/gregfaletto/Documents/R/econometrics")

rm(list=ls())

library("ggplot2")

data <- read.table("TrainExer 3-1-corrected.txt",
	header=T)

n <- nrow(data)

# (a)

change.log.Index <- diff(log(data$Index))

data.a <- data.frame(data$BookMarket[2:n],
	change.log.Index)

colnames(data.a) <- c("BookMarket", "change.log.Index")

data.plot <- data.frame(rep(data$Year[2:n], 2),
	c(data$BookMarket[2:n], change.log.Index),
	c(rep("BookMarket", n-1), rep("change.log.Index",
	n-1)))

colnames(data.plot) <- c("Year", "Data", "Legend")

plot.a <- ggplot(data=data.plot, aes(x=Year, y=Data,
	color=Legend)) + geom_line()

print(plot.a)

model.a <- lm(change.log.Index ~ BookMarket, data=data.a)

print(summary(model.a))

# (b)

model.b <- lm(Index ~ BookMarket, data=data)

print(summary(model.b))

# Yes, BookMarket seems to be significant in predicting 
# Index (p = 4e-12, r^2 = 0.4341)

# (c)

data.resids.a <- data.frame(data$Year[2:n], model.a$residuals)

colnames(data.resids.a) <- c("Year", "Residuals.A")

data.resids.b <- data.frame(data$Year, model.b$residuals)

colnames(data.resids.b) <- c("Year", "Residuals.B")

resids.a.plot <- ggplot(data=data.resids.a, aes(x=Year,
	y=Residuals.A)) + geom_point()

resids.b.plot <- ggplot(data=data.resids.b, aes(x=Year,
	y=Residuals.B)) + geom_point()

print(resids.a.plot)

print(resids.b.plot)

# The residuals for the plot in part (a) are much more flat
# and seem more likely to be homoskedastic and normally
# distributed with mean 0. The part (b) residuals don't
# seem to meet any of these specifications.