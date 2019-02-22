# setwd("/Users/gfaletto/Desktop/econometrics")

rm(list=ls())

library("ggplot2")

data <- read.table("TrainExer15.txt", header=T)

n <- nrow(data)

### Part (a)

######### Linear trend for women

# Calculate b

G.bar <- mean(data$Game)
W.bar <- mean(data$Winwomen)

b.women <- (data$Game-G.bar)%*%(data$Winwomen-W.bar)/((data$Game-
	G.bar)%*%(data$Game-G.bar))
print("Regression line for women:")
print(paste("b=", b.women))

# Calculate a

a.women <- W.bar-b.women*G.bar
print(paste("a=", a.women))

# Check by re-calculating using built-in lm() function

model.w <- lm(Winwomen ~ Game, data=data)

# print(paste("According to the built-in linear regression function lm(), a =",
# 	as.numeric(model.w$coefficients[1]), "and b =",
# 	as.numeric(model.w$coefficients[2]), "."))

# print(paste("In a, difference of",
# 	a.women-as.numeric(model.w$coefficients[1])))
# print(paste("In b, difference of",
# 	b.women-as.numeric(model.w$coefficients[2])))

######### Linear trend for men

# Calculate b

M.bar <- mean(data$Winmen)

b.men <- (data$Game-G.bar)%*%(data$Winmen-M.bar)/((data$Game-
	G.bar)%*%(data$Game-G.bar))
print("Regression line for men:")
print(paste("b=", b.men))

# Calculate a

a.men <- M.bar-b.men*G.bar
print(paste("a=", a.men))

# Check by re-calculating using built-in lm() function

model.m <- lm(Winmen ~ Game, data=data)

# print(paste("According to the built-in linear regression function lm(), a =",
# 	as.numeric(model.m$coefficients[1]), "and b =",
# 	as.numeric(model.m$coefficients[2]), "."))

# print(paste("In a, difference of",
# 	a.men-as.numeric(model.m$coefficients[1])))
# print(paste("In b, difference of",
# 	b.men-as.numeric(model.m$coefficients[2])))


### find point of intersection

t.intersection <- optimize(function(t) abs(a.men + b.men*t -
	(a.women + b.women*t)), interval=c(0:100))$minimum

print(paste("intersection (linear):", t.intersection))
print(paste("(this means a year of", round(t.intersection*4+1944), ")"))


##### Plot of data

games.gg <- rep(data$Game, 2)
times.gg <- c(data$Winmen, data$Winwomen)
gender.gg <- c(rep("Men", n), rep("Women", n))

df <- data.frame(games.gg, times.gg, gender.gg)

# plot <- ggplot(data=df, aes(x=games.gg, y=times.gg, color=gender.gg)) +
# geom_point() + geom_smooth(method="lm", se=F) + xlim(0, 70)

plot <- ggplot(data=df, aes(x=games.gg, y=times.gg, color=gender.gg)) +
geom_point() + stat_function(fun=function(x)(a.men+b.men*x),
	geom="line") + stat_function(fun=function(x)(a.women+
		b.women*x), geom="line") + xlim(0, 70)

print(plot)

# Plot of residuals against year

residuals.m.df <- data.frame(data$Game,
	as.numeric(data$Winmen-predict(model.m)))
colnames(residuals.m.df) <- c("Game", "Residual.Men")
plot.resid.m <- ggplot(data=residuals.m.df, aes(x=Game, y=Residual.Men)) +
geom_point() + geom_smooth(method="lm", se=F)
print(plot.resid.m)

model.resids.m <- lm(Residual.Men ~ Game, data=residuals.m.df)
print("Line of best fit for the residuals (men):")
print(model.resids.m)

#

residuals.w.df <- data.frame(data$Game,
	as.numeric(data$Winwomen-predict(model.w)))
colnames(residuals.w.df) <- c("Game", "Residual.Women")
plot.resid.w <- ggplot(data=residuals.w.df, aes(x=Game, y=Residual.Women)) +
geom_point() + geom_smooth(method="lm", se=F)
print(plot.resid.w)

model.resids.w <- lm(Residual.Women ~ Game, data=residuals.w.df)
print("Line of best fit for the residuals (women):")
print(model.resids.w)


# Part (b)

######### Non-Linear trend for women

# Calculate b

W.nonlinear <- log(data$Winwomen)

# G.bar <- mean(data$Game)
W.bar.nl <- mean(W.nonlinear)

b.women.nl <- (data$Game-G.bar)%*%(W.nonlinear-W.bar.nl)/((data$Game-
	G.bar)%*%(data$Game-G.bar))
print("Non-linear regression line for women:")
print(paste("b=", b.women.nl))

# Calculate a

a.women.nl <- W.bar.nl-b.women.nl*G.bar
print(paste("a=", a.women.nl))

# Check by re-calculating using built-in lm() function

df.nl.w <- data.frame(data$Game, W.nonlinear)

model.nl.w <- lm(W.nonlinear ~ data.Game, data=df.nl.w)

# print(paste("According to the built-in linear regression function lm(), a =",
# 	as.numeric(model.nl.w$coefficients[1]), "and b =",
# 	as.numeric(model.nl.w$coefficients[2]), "."))

# print(paste("In a, difference of",
# 	a.women.nl-as.numeric(model.nl.w$coefficients[1])))
# print(paste("In b, difference of",
# 	b.women.nl-as.numeric(model.nl.w$coefficients[2])))

######### Non-Linear trend for men

# Calculate b

M.nonlinear <- log(data$Winmen)

# G.bar <- mean(data$Game)
M.bar.nl <- mean(M.nonlinear)

b.men.nl <- (data$Game-G.bar)%*%(M.nonlinear-M.bar.nl)/((data$Game-
	G.bar)%*%(data$Game-G.bar))
print("Non-linear regression line for men:")
print(paste("b=", b.men.nl))

# Calculate a

a.men.nl <- M.bar.nl-b.men.nl*G.bar
print(paste("a=", a.men.nl))

# Check by re-calculating using built-in lm() function

df.nl.m <- data.frame(data$Game, M.nonlinear)

model.nl.m <- lm(M.nonlinear ~ data.Game, data=df.nl.m)

# print(paste("According to the built-in linear regression function lm(), a =",
# 	as.numeric(model.nl.m$coefficients[1]), "and b =",
# 	as.numeric(model.nl.m$coefficients[2]), "."))

# print(paste("In a, difference of",
# 	a.men.nl-as.numeric(model.nl.m$coefficients[1])))
# print(paste("In b, difference of",
# 	b.men.nl-as.numeric(model.nl.m$coefficients[2])))



### find point of intersection

t.intersection.nl <- optimize(function(t) abs(exp(a.men.nl + b.men.nl*t) -
	exp(a.women.nl + b.women.nl*t)), interval=c(0:100))$minimum

print(paste("intersection (nonlinear):", t.intersection.nl))
print(paste("(this means a year of", round(t.intersection.nl*4+1944), ")"))

##### Plot of data

plot.nl <- ggplot(data=df, aes(x=games.gg, y=times.gg, color=gender.gg)) +
geom_point() + stat_function(fun=function(x)exp(a.men.nl+b.men.nl*x),
	geom="line") + stat_function(fun=function(x)exp(a.women.nl+
		b.women.nl*x), geom="line") + xlim(0, 70)

print(plot.nl)

# Plot of residuals against year

residuals.m.nl.df <- data.frame(data$Game,
	as.numeric(data$Winmen-predict(model.nl.m)))
colnames(residuals.m.nl.df) <- c("Game", "Residual.Men.NL")
plot.resid.m.nl <- ggplot(data=residuals.m.nl.df, aes(x=Game,
	y=Residual.Men.NL)) + geom_point() + geom_smooth(method="lm",
	se=F)
print(plot.resid.m.nl)

model.resids.m.nl <- lm(Residual.Men.NL ~ Game, data=residuals.m.nl.df)
print("Line of best fit for the residuals (men) (non-linear):")
print(model.resids.m.nl)

#

residuals.w.nl.df <- data.frame(data$Game,
	as.numeric(data$Winwomen-predict(model.nl.w)))
colnames(residuals.w.nl.df) <- c("Game", "Residual.Women.NL")
plot.resid.w.nl <- ggplot(data=residuals.w.nl.df, aes(x=Game,
	y=Residual.Women.NL)) + geom_point() + geom_smooth(method="lm",
	se=F)
print(plot.resid.w.nl)

model.resids.w.nl <- lm(Residual.Women.NL ~ Game, data=residuals.w.nl.df)
print("Line of best fit for the residuals (women) (non-linear):")
print(model.resids.w.nl)

#
#
#
#
############ Quadratic fit for women
#
#
#
#

model.quad.w <- lm(Winwomen ~ Game + I(Game^2), data=data)

# print(model.quad.w)

plot.quad <- ggplot(data=df, aes(x=games.gg, y=times.gg, color=gender.gg)) +
geom_point() + stat_function(fun=function(x)(a.men+b.men*x),
	geom="line") + stat_function(fun=function(x)(model.quad.w$coefficients[[1]]+
	model.quad.w$coefficients[[2]]*x+model.quad.w$coefficients[[3]]*x^2),
	geom="line") + xlim(0, 20)

print(plot.quad)

# residuals

residuals.w.quad.df <- data.frame(data$Game,
	as.numeric(data$Winwomen-predict(model.quad.w)))
colnames(residuals.w.quad.df) <- c("Game", "Residual.Women.Quad")
plot.resid.w <- ggplot(data=residuals.w.quad.df, aes(x=Game,
	y=Residual.Women.Quad)) + geom_point() + geom_smooth(method="lm",
	se=F)
print(plot.resid.w)

# model.resids.w <- lm(Residual.Women ~ Game, data=residuals.w.df)
# print("Line of best fit for the residuals (women):")
# print(model.resids.w)




# Part (c)

print(paste("Predicted equal win time (linear model):",
	a.men+b.men*t.intersection))

print(paste("Predicted equal win time (non-linear model):",
	exp(a.men.nl+b.men.nl*t.intersection.nl)))