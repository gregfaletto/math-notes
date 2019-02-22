# setwd("/Users/gfaletto/Desktop/econometrics")

rm(list=ls())

# library("ggplot2")

data <- read.table("TrainExer 3-3-corrected.txt",
	header=T)

n <- nrow(data)

### Part (a)

# log(a) − log(b) = log(a/b) = log(1 + (a − b)/b)
# = log((a-b/b)) (for small (a-b)/b)

# b^(log((a-b)/b)) = b^(a-b)

# growth rate: y = P0*B^(r^t)

# b = P0*B^(r^t2)
# a = P0*B^(r*t1)

# a - b = PO*(B^(r*t1) - B^(r*t2))

# a - b = P0*B^(r*t2)*(B^(r*(t1 - t2)) - 1)

# log(a - b) = log(P0) + r*t2*log(B) +
#      log(B^(r*(t1 - t2)) - 1)
# log((a - b)/P0) = r*t2*log(B) 




# log(y/P0)/(t*log(B)) = r

# Part (b)

change.log.Index <- diff(log(data$Index))

data.b <- data.frame(data$BookMarket[2:n],
	change.log.Index)

colnames(data.b) <- c("BookMarket", "change.log.Index")

# data.plot <- data.frame(rep(data$Year[2:n], 2),
# 	c(data$BookMarket[2:n], change.log.Index),
# 	c(rep("BookMarket", n-1), rep("change.log.Index",
# 	n-1)))

# colnames(data.plot) <- c("Year", "Data", "Legend")

# plot.b <- ggplot(data=data.plot, aes(x=Year, y=Data,
# 	color=Legend)) + geom_line()

# print(plot.b)

model.b <- lm(change.log.Index ~ BookMarket +
	I(BookMarket^2), data=data.b)

print(summary(model.b))

# (c)

dummy <- as.integer(data$Year>1979)

data.c <- data.frame(data$BookMarket[2:n],
	change.log.Index, dummy[2:n])

colnames(data.c) <- c("BookMarket", "change.log.Index",
	"Year.1980")

model.c <- lm(change.log.Index ~ BookMarket +
	BookMarket:Year.1980, data=data.c)

print(summary(model.c))


