# setwd("/Users/gregfaletto/Google Drive/Data Science/GRE Math Subject Test/Notes/Topics/DSO Screening Exam/2018 Take Home")
# dev.off()

library(ggplot2)

# Set seed for reproducibility
set.seed(54343)

# number of games per value of p
n <- 1000
# grid of values of p
p.vec <- (0:99)/99

simulateGame <- function(p){
	# Simulates a game of tennis given a value of p that the server wins a
	# given point. Returns 1 if the server wins the game, or 0 otherwise.

	# Start by simulating 6 games. If either player has won more than 3 games, 
	# that player wins the game.
	wins <- rbinom(n=1, size=6, prob=p)
	if(wins > 3){
		return(1)
	} else if(wins < 3){
		return(0)
	} else{
		# If wins == 3, we go to deuce. Return 1 with probability given 
		# from part (b) answer.

		# probability of winning a deuce, per part (b)
		p.deuce.win <- p^2/(1-2*p*(1-p))
		if(runif(1) < p.deuce.win){
			return(1)
		} else{
			return(0)
		}
	}
}


simulateNGames <- function(p, n){
	# Simulations n games of tennis according to the simulateGame function
	# for one value of p. Returns the proportion of games won out of n.


	# Calculate number of games won in n games.
	num.wins <- 0

	for(i in 1:n){
		num.wins <- num.wins + simulateGame(p)
	}

	# Return proportion of games won out of n
	return(num.wins/n)
}

probWinning <- function(p){
	# Analytical probability of winning, according to work done earlier
	return(p^4+4*p^4*(1-p)+10*p^4*(1-p)^2+20*p^5*(1-p)^3/(1-2*p*(1-p)))
}

# Calculate number of wins for each p

prob.winning <- sapply(p.vec, simulateNGames, n=n)

# Provide a plot with p on the x-axis and the probability of winning the game on
# the y-axis. 

data.plot <- data.frame(p.vec, prob.winning)
colnames(data.plot) <- c("p", "Prob.winning")

# plot <- ggplot(data.plot, aes(x=p, y=Prob.winning)) + geom_point()
plot <- ggplot(data.plot, aes(x=p, y=Prob.winning)) + geom_point() +
	layer(stat = "function",
          fun = probWinning,
          mapping = aes(color = "fun.1")
          )

print(plot)


# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + stat_function(fun = fun.1) + xlim(-5,5)

