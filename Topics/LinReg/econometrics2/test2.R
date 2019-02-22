# setwd("/Users/gfaletto/Desktop/econometrics")

rm(list=ls())

library("ggplot2")

data <- read.table("TestExer2-GPA-round2.txt",
	header=T)

n <- nrow(data)

digits <- 5 # number of digits I'm rounding to

MyCovMatrix <- function(data) {
	vars <- data[, 2:ncol(data)]
	means <- rep(0, ncol(vars))
	for (i in 1:ncol(vars)) {
		means[i] <- mean(vars[, i])
	}
	labels <- colnames(vars)
	mat <- matrix(rep(0, (length(means))^2),
		ncol=length(means))
	for (i in 1:length(means)) {
		for (j in 1:length(means)) {
			mat[i, j] <- 1/(nrow(data)-1)*
				((vars[, i]-means[i])%*%(vars[, j]-
				means[j]))
		}
	}
	colnames(mat) <- labels
	rownames(mat) <- labels
	return(mat)
}

MyCorrMatrix <- function(data) {
	covs <- MyCovMatrix(data)
	vars <- data[, 2:ncol(data)]
	means <- rep(0, ncol(vars))
	for (i in 1:ncol(vars)) {
		means[i] <- mean(vars[, i])
	}
	labels <- colnames(vars)
	mat <- matrix(rep(0, (length(means))^2),
		ncol=length(means))
	for (i in 1:length(means)) {
		for (j in 1:length(means)) {
			mat[i, j] <- sqrt((1/(nrow(data)-1))^2*
				((vars[, i]-means[i])%*%(vars[, i]-
				means[i]))*((vars[, j]-means[j])%*%
				(vars[, j]-means[j])))
		}
	}
	ans <- covs/mat
	colnames(ans) <- labels
	rownames(ans) <- labels
	for (i in 1:length(means)){
		for (j in 1:length(means)){
			ans[i, j] <- round(ans[i, j], digits=3)
		}
	}
	return(ans)
}

MyCorrMatrix2 <- function(data) {
	vars <- data[, 2:ncol(data)]
	means <- rep(0, ncol(vars))
	for (i in 1:ncol(vars)) {
		means[i] <- mean(vars[, i])
	}
	labels <- colnames(vars)
	mat <- matrix(rep(0, (length(means))^2),
		ncol=length(means))
	for (i in 1:length(means)) {
		for (j in 1:length(means)) {
			mat[i, j] <- ((vars[, i]-means[i])%*%(vars[, j]-
				means[j]))/sqrt(((vars[, i]-means[i])%*%
				(vars[, i]-means[i]))*((vars[, j]-means[j])%*%
				(vars[, j]-means[j])))
			mat[i, j] <- round(mat[i, j], digits=3)
		}
	}
	colnames(mat) <- labels
	rownames(mat) <- labels
	return(mat)
}

MyCoefficients <- function(model) {
	for(i in 2:nrow(coef(summary(model)))){
		l.lim <- model$coefficients[[i]]-
		2*coef(summary(model))[i,2]

		u.lim <- model$coefficients[[i]]+
		2*coef(summary(model))[i,2]

		print(paste("95% CONFIDENCE INTERVAL FOR",
			rownames(coef(summary(model))))[i])
		print(paste("Lower limit:", round(l.lim, digits=digits)))
		print(paste("Upper limit:", round(u.lim, digits=digits)))
		print("")
	}
}

# (a)

# (i)

model.a <- lm(FGPA ~ SATV, data=data)

print(summary(model.a))

# (ii)

MyCoefficients(model.a)

# (b)

# (i)

model.b <- lm(FGPA ~ SATV + SATM + FEM, data=data)

print(summary(model.b))

# (ii)

MyCoefficients(model.b)

# (c)

print(MyCorrMatrix(data))

print(MyCorrMatrix2(data))

# (d)

# Unrestricted model: contains SATV, SATM, and FEM

e1 <- model.b$residuals

# Restricted model: contains SATM and FEM but not SATV

model.d <- lm(FGPA ~ SATM + FEM, data=data)

e0 <- model.d$residuals

# H0: coefficient on SATV = 0. H1: coefficient on SATM =/= 0

g <- 1

k <- 3

F1 <- ((e0%*%e0 - e1%*%e1)/g)/((e1%*%e1)/(n-k))

R1 <- summary(model.d)$r.squared
R0 <- summary(model.b)$r.squared

F2 <- ((R1 - R0)/g)/((1-R1)/(n-k))

print(paste("F1 =", round(F1, digits=7)))

print(paste("F2 =", round(F2, digits=7)))