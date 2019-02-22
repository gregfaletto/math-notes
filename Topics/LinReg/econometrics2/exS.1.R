# setwd("/Users/gregfaletto/Documents/R/Econometrics/ex8")

# setwd("/Users/gfaletto/Desktop/econometrics")

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

means <- sapply(start:n, function(x){(1/x)*sum(data[1:x])}) # length: n - 4
vars <- sapply(start:n, MyError)			                # length: n - 4
ns <- start:n   											# length: n - 4

means.df <- data.frame(cbind(ns, means, vars))

means.plus <- means+2*vars
means.minus <- means-2*vars

all.means <- c(means, means.plus, means.minus)
labels <- c(rep("Mean", length(means)), rep("Upper.Limit",
	length(means.plus)), rep("Lower.Limit",
	length(means.minus)))
ns.errs <- rep(ns, 3)

means.errs.df <- data.frame(ns.errs, all.means, labels)

# vars.df <- data.frame(cbind(ns, vars))

# scatter <- ggplot(data=means.df, aes(x=ns, y=means)) +
# 	geom_point()

scatter <- ggplot(data=means.df, aes(x=ns, y=means)) +
	geom_point() +
	geom_errorbar(aes(ymin=means.minus,
		ymax=means.plus, alpha=0.5))

scatter.2 <- ggplot(data=means.errs.df, aes(x=ns.errs,
	y=all.means, color=labels)) + geom_point()


scatter.vars <- ggplot(data=means.df, aes(x=ns, y=vars)) +
	geom_point()

print(scatter)
print(scatter.2)
print(scatter.vars)

#
#
#
#
#
############ Exercise S2 ##############
#
#
#
#
#
#

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
		ps[i] <- 2*pt(t.stats[i], df=i-1,
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

rm(t.stats.0)
rm(ps.0)

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

rm(t.stats.6)
rm(ps.6)

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


# df.200 <- data.frame(rep(n.start:n.end, 2), c(t.stats.200.0,
# 	t.stats.200.6), c(rep("mu=0", n.end-n.start+1), rep("mu=0.06",
# 	n.end-n.start+1)))
# colnames(df.200) <- c("ns", "t.stat", "mu")

# scatter.3 <- ggplot(data=df.200, aes(x=ns,
# 	y=t.stat, color=mu)) + geom_point() + geom_line()

# CriticalValues <- function(alpha, df) {
# 	abs(qt(alpha, df))
# }

MyCriticalValues <- function(alpha, n.start, n.end) {
	sapply(n.start:n.end, function(i){abs(qt(alpha, i-1))})
}

cs.10 <- MyCriticalValues(.1/2, n.start, n.end)

cs.05 <- MyCriticalValues(.05/2, n.start, n.end)

cs.01 <- MyCriticalValues(.01/2, n.start, n.end)

df.200 <- data.frame(rep(n.start:n.end, 8), c(t.stats.200.0,
	t.stats.200.6, cs.10, -1*cs.10,  cs.05, -1*cs.05, cs.01, 
	-1*cs.01), c(rep("mu=0", length(t.stats.200.0)), 
	rep("mu=0.06", length(t.stats.200.6)), rep("t.crit=.10",
	2*length(cs.10)), rep("t.crit=.05", 2*length(cs.05)), 
	rep("t.crit=.01", 2*length(cs.01))))

colnames(df.200) <- c("n", "T.value", "Legend")

scatter.3 <- ggplot(data=df.200, aes(x=n,
	y=T.value, color=Legend)) + geom_point()

print(scatter.3)

# Part (c)

# We have sufficient evidence to reject the null hypothesis that
# mu = 0 at the alpha = 0.1 significance level with n = around 27
# observations, at the alpha = 0.05 significance level at around
# n = 30 observations, and at the alpha = .01 significance level at
# around n = 35 observations. We do not have significant evidence
# to reject the null hypothesis that mu = 6%.
#
# For a hypothesis of mu = 6.5%, I would expect that a graph of 
# t-values vs. n would look similar to the graph of t-values
# for the hypothesis mu = 6%, except shifted down a little on the
# vertical axis. That would suggest that we would likely have
# insufficient evidence to reject this hypothesis at any 
# significance level as well.
#
# We never accept null hypotheses because a statistical analysis
# presumes the null hypothesis to be true without evidence. A 
# significance test can tell whether our data is sufficiently
# unlikely under the assumption that the null hypothesis is true, but it
# can't tell us how likely our null hypothesis was to be true in
# the first place.

# Part (d)

TestStatisticChi <- function(n, sigma) {
	m <- (1/n)*sum(data[1:n])
	var <- as.numeric((data[1:n]-m)%*%(data[1:n]-m))/(n-1)
	(n-1)*var/sigma^2
	# sqrt(var)
}

# PValue <- function(n) {
# 	2*pt(TestStatistic(n), df=n-1)
# }

StatsChi <- function(sigma, n.start, n.end) {
	chi.stats <- 1:n.end
	for (i in n.start:n.end) {
		chi.stats[i] <- TestStatisticChi(i, sigma)
	}

	ps <- 1:n.end

	for (i in n.start:n.end) {
		ps[i] <- pchisq(chi.stats[i], df=i-1,
			lower.tail=T)
		if(ps[i]>0.5){
			ps[i] <- 2*(1-ps[i])
		} else {
			ps[i] <- 2*ps[i]
		}
	}
	chi.stats <- chi.stats[n.start:n.end]
	ps <- ps[n.start:n.end]
	return(list(chi.stats, ps))
}

n.start <- 5
n.end <- 30

results.18 <- StatsChi(18, n.start, n.end)	

chi.sq.stats.18 <- results.18[[1]]
ps.18 <- results.18[[2]]

rm(results.18)		   

df.18 <- data.frame(cbind(n.start:n.end, chi.sq.stats.18,
	ps.18))
colnames(df.18) <- c("n", "Test.Statistic.Chi.sq=18",
	"p-value")

rm(chi.sq.stats.18)
rm(ps.18)

print(df.18)

#

results.15 <- StatsChi(15, n.start, n.end)	

chi.sq.stats.15 <- results.15[[1]]
ps.15 <- results.15[[2]]

rm(results.15)		   

df.15 <- data.frame(cbind(n.start:n.end, chi.sq.stats.15,
	ps.15))
colnames(df.15) <- c("n", "Test.Statistic.Chi.sq=18",
	"p-value")

rm(chi.sq.stats.15)
rm(ps.15)

print(df.15)

# Part (e)

n.start <- 5
n.end <- 200

results.200.18 <- StatsChi(18, n.start, n.end)	

chi.stats.200.18 <- results.200.18[[1]]
ps.200.18 <- results.200.18[[2]]
rm(results.200.18)

results.200.15 <- StatsChi(15, n.start, n.end)	

chi.stats.200.15 <- results.200.15[[1]]
ps.200.15 <- results.200.15[[2]]
rm(results.200.15)

MyCriticalValuesChi <- function(alpha, n.start, n.end) {
	sapply(n.start:n.end, function(i){abs(qchisq(alpha,
		i-1))})
}

cs.chi.10 <- MyCriticalValuesChi(.1/2, n.start, n.end)

cs.chi.05 <- MyCriticalValuesChi(.05/2, n.start, n.end)

cs.chi.01 <- MyCriticalValuesChi(.01/2, n.start, n.end)

cs.chi.90 <- MyCriticalValuesChi(1-.1/2, n.start, n.end)

cs.chi.95 <- MyCriticalValuesChi(1-.05/2, n.start, n.end)

cs.chi.99 <- MyCriticalValuesChi(1-.01/2, n.start, n.end)

df.chi.200 <- data.frame(rep(n.start:n.end, 8), c(chi.stats.200.18,
	chi.stats.200.15, cs.chi.10, cs.chi.90, cs.chi.05, cs.chi.95, 
	cs.chi.01, cs.chi.99), c(rep("sigma=18", length(chi.stats.200.18)), 
	rep("sigma=15", length(chi.stats.200.15)), rep("t.crit=.10",
	2*length(cs.chi.10)), rep("t.crit=.05", 2*length(cs.chi.05)), 
	rep("t.crit=.01", 2*length(cs.chi.01))))

colnames(df.chi.200) <- c("n", "Sigma.value", "Legend")

scatter.4 <- ggplot(data=df.chi.200, aes(x=n,
	y=Sigma.value, color=Legend)) + geom_point()

print(scatter.4)

# Part (f)

# We have signficant evidence to suggest that sigma is not equal to 18%,
# but we do not have significant evidence to suggest that sigma is not
# equal to 15%.

