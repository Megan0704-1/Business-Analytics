#rolling a die
rolls <- sample(1:6, 6, replace=FALSE)
hist(rolls, prob=TRUE, col="lightgray", breaks=0:6, main="Fair Dicr Roll")

num_dice <- c(1, 2, 10)
die_rolls <- function(){
  round(runif(10000, min=0.5, max=6.5))
}
dice_means <- function(num_dice){
  rowMeans(replicate(num_dice, die_rolls()))
}
plt_density <- function(dice_games){
  plot(density(dice_games))
}

# rowmeans of rolling one die 10000 times and rolling 10 dice 10000 times
dice_games = lapply(num_dice, dice_means)

par(mfrow=c(3, 3))
invisible(sapply(dice_games, hist, col="lightgray", main=NULL, prob=TRUE))
invisible(sapply(dice_games, plt_density))

# list all possible means that two dice can possibly produce together
mean_rolls <- matrix(NA, nrow=6, ncol=6)
for(i in 1:6){
  for(j in 1:6){
    mean_rolls[i, j] = mean(c(i ,j))
  }
}
# we can see that some means are easier to arrive than others
# THis illustrate the Central Limit Theorem.
# Values that are towards the center of possible outcomes are more likely to occur because there are more random combinations with which they can be produced.
