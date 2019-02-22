# setwd("/Users/gfaletto/Desktop/econometrics")

rm(list=ls())

# library("ggplot2")

data <- read.table("TrainExer5-5.txt",
	header=T)

n <- nrow(data)

### Part (a)