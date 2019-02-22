# setwd("/Users/gregfaletto/Documents/R/Econometrics/ex8")

library(ggplot2)

rm(list=ls())

data <- read.table("TrainExerS1.txt")[,1]
n <- length(data)
start <- 5

MyError <- function(k) {
	mean <- (1/k)*sum(data[1:k])
	var <- as.numeric((data[1:k]-mean)%*%(data[1:k]-mean))/(k-1)
	# errs <- sapply(1:k, function(x){(data[x]-mean)^2})
	# var <- sum(errs)/(k-1)
	sqrt(var/k)
}

means <- sapply(start:n, function(x){(1/x)*sum(data[1:x])})    # length: n - 4
vars <- sapply(start:n, MyError)			   # length: n - 4
ns <- start:n   											   # length: n - 4

means.df <- data.frame(cbind(ns, means, vars))

means.plus <- means+2*vars
means.minus <- means-2*vars

all.means <- c(means, means.plus, means.minus)
labels <- c(rep("Mean", length(means)), rep("Upper.Limit",
	length(means.plus)), rep("Lower.Limit", length(means.minus)))
ns.errs <- rep(ns, 3)

means.errs.df <- data.frame(ns.errs, all.means,
	labels)

# vars.df <- data.frame(cbind(ns, vars))

# scatter <- ggplot(data=means.df, aes(x=ns, y=means)) +
# 	geom_point()

scatter <- ggplot(data=means.df, aes(x=ns, y=means)) +
	geom_point() +
	geom_errorbar(aes(ymin=means-2*vars,
		ymax=means+2*vars, alpha=0.5))

scatter.2 <- ggplot(data=means.errs.df, aes(x=ns.errs,
	y=all.means, color=labels)) + geom_point()


scatter.vars <- ggplot(data=means.df, aes(x=ns, y=vars)) +
	geom_point()

print(scatter)
print(scatter.2)
print(scatter.vars)

## Exercise S2

# Part (a)

n.start <- 5
n.end <- 30


TestStatistic <- function(n, mu) {
	m <- (1/n)*sum(data[1:n])
	var <- as.numeric((data[1:n]-m)%*%(data[1:n]-m))/(n-1)
	return(sqrt(n/var)*(m - mu))
}

# PValue <- function(n) {
# 	2*pt(TestStatistic(n), df=n-1)
# }

StatsMu <- function(mu, n.start, n.end) {
	t.stats <- 1:n.end
	for (i in n.start:n.end) {
		t.stats[i] <- TestStatistic(i, mu)
	}

	ps <- 1:n.end

	for (i in n.start:n.end) {
		ps[i] <- 2*pt(t.stats[i], df=(i+n.start)-1,
			lower.tail=t.stats[i]<0)
	}
	t.stats <- t.stats[n.start:n.end]
	ps <- ps[n.start:n.end]
	return(list(t.stats, ps))
}

# t.stats.0 <- sapply(n.start:n.end, TestStatistic)    
# ps.0 <- sapply(n.start:n.end, PValue)

results.0 <- StatsMu(0, n.start, n.end)	

t.stats.0 <- results.0[[1]]
ps.0 <- results.0[[2]]

rm(results.0)		   

df.0 <- data.frame(cbind(n.start:n.end, t.stats.0, ps.0))
colnames(df.0) <- c("n", "Test.Statistic.mu=0", "p-value")

print(df.0)


# mu <- .06

# t.stats.6 <- n.start:n.end

# for (i in 1:(n.end - n.start + 1)) {
# 	t.stats.6[i] <- TestStatistic(i, mu)
# }

# ps.6 <- n.start:n.end

# for (i in 1:(n.end - n.start + 1)) {
# 	ps.6[i] <- 2*pt(TestStatistic(n, mu), df=n-1)
# }

# t.stats.6 <- sapply(n.start:n.end, TestStatistic)    
# ps.6 <- sapply(n.start:n.end, PValue)		

results.6 <- StatsMu(6, n.start, n.end)	

t.stats.6 <- results.6[[1]]
ps.6 <- results.6[[2]]

rm(results.6)		   

df.6 <- data.frame(cbind(n.start:n.end, t.stats.6, ps.6))
colnames(df.6) <- c("n", "Test.Statistic.mu=0.06", "p-value")

print(df.6)

# Part (b)

n.start <- 5
n.end <- 200

results.200.0 <- StatsMu(0, n.start, n.end)	

t.stats.200.0 <- results.200.0[[1]]
ps.200.0 <- results.200.0[[2]]
rm(results.200.0)

results.200.6 <- StatsMu(6, n.start, n.end)	

t.stats.200.6 <- results.200.6[[1]]
ps.200.6 <- results.200.6[[2]]
rm(results.200.6)


df.200 <- data.frame(rep(n.start:n.end, 2), c(t.stats.200.0,
	t.stats.200.6), c(rep("mu=0", n.end-n.start+1), rep("mu=0.06",
	n.end-n.start+1)))
colnames(df.200) <- c("ns", "t.stat", "mu")

CriticalValues <- function(alpha, df) {
	abs(qt(alpha, df))
}

scatter.3 <- ggplot(data=df.200, aes(x=ns,
	y=t.stat, color=mu)) + geom_point() + geom_line()

print(scatter.3)


