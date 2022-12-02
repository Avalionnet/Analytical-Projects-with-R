# Question 1
### Chance of winning is 0.0241%

probabilityMultiplier <- function(iter, numerator, denom){
  if(iter <= 0) {
    return(1)
  }
  return((numerator/denom)*probabilityMultiplier(iter-1, numerator - 1, denom))
}

winningChance <- probabilityMultiplier(30,365,365) * (30/365)
print(winningChance*100)

# Question 2


winRound <- function(iter, numInQ){
  bdays <- c(0)
  counter <- 1
  
  while (counter <= numInQ){
    currentBday <- sample(1:365, 1, replace = T)
    if (currentBday %in% bdays & counter != numInQ){
      return(FALSE)
    } 
    if (currentBday %in% bdays & counter == numInQ){
      return(TRUE)
    }
    bdays <- c(bdays, currentBday) 
    counter <- counter + 1
  }
  return(FALSE)
}

chanceOfQPosition <- function(numOfPpl, numOfIter) {
  results <- data.frame(iter = 1:numOfIter, winner = sapply(1:numOfIter, winRound, numOfPpl))
  chance <- mean(results$winner)
}

numOfIter <- 10000
numOfPpl <- 31
positionProbability <- data.frame(Q.Position = 1:numOfPpl, chance = sapply(1:numOfPpl, chanceOfQPosition, numOfIter))
positionProbability[positionProbability$chance == max(positionProbability$chance),]
