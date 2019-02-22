# setwd("/Users/gregfaletto/Documents/R/econometrics")

rm(list=ls())

# library("ggplot2")

# (a)

data <- read.table("TrainExer21.txt", header=T)
# model.a <- lm(LogWage ~ Female, data=data)
model.a <- lm(log(Wage) ~ Female, data=data)
print(model.a)

# (b)

# (i)

predictions <- as.numeric(predict.lm(model.a))

e <- data$LogWage - predictions

data <- data.frame(data, e)

model.i <- lm(e ~ Educ, data=data)

print(summary(model.i))

# (ii)

model.ii <- lm(e ~ Parttime, data=data)

print(summary(model.ii))

# (c)

# Regressions (i) and (ii) suggest that education
# alone as well as whether one has a part-time
# job can both provide additional predictive value
# in the wage one will earn beyond what can be
# predicted with one's gender alone.