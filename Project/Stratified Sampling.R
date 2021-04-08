# Create a population matrix with 10 rows and 1000 columns. Every row is a stratum.
population <- matrix(NA, nrow = 10, ncol = 1000)

p <- c(0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005) # Infection rate of each stratum.
#k <- c(12, 10, 8, 6, 5, 4, 2, 1, 1, 1) # Number of positive samples in each stratum.

B <- 10 # Number of the simulation, we'll produce n for B times.
starting_point <- 500 # The starting point could be found with a demo experiment with a small B.
ending_point <- 1000 # The ending point could also be found with a demo experiment with a small B.
stepsize <- 20 # Update n by adding stepsize, and do the simulation again.

test <- c() # Store all the samples within a single experiment
ans <- c() # Store all the sample sizes

for(i in 1:B){
  repeat{
    for(row in 1:10){
      #pos_class <- rep(1, k[i])
      #neg_class <- rep(0, 1000 - k[i])
      #population[row,] <- c(pos_class, neg_class)
      population[row,] <- rbinom(1000, 1, p[row])
    }
    
    if(mean(population) > 0) # Make sure that population is non-zero
      break
  }
  
  n <- starting_point
  repeat 
  {
    test <- c()
    for(j in 1:1000) # Monte Carlo simulation with 1000 experiments
    {
      # Sample from the strata according to the stratify ratio (1:1:...:1)
      n_i = n/10
      sample <- matrix(NA, nrow = 10, ncol = n_i)
      for(l in 1:10){
        sample[l,] <- sample(population[l,], n_i)
      }
      
      if(mean(sample) == 0) # No positive samples
        test[j] <- 0
      else # There are positive samples
        test[j] <- 1
    }
    #print(mean(test))
    #print(n)
    if(mean(test) < 0.99)
    #if(mean(test) < 0.99 && n < ending_point)
      n <- n + stepsize
    
    else
      break
  }
  print(i)
  ans <- c(ans, n)
}

n_hat <- mean(ans)
ci.lower <- n_hat - qnorm(1 - 0.05/2) * sd(ans)
ci.upper <- n_hat + qnorm(1 - 0.05/2) * sd(ans)
paste("The estimation of n is ", n_hat, ", with 95% CI: (", ci.lower, ",", ci.upper, ").")