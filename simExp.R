simExp <- function(){
      
      ## Set all of the known starting parameters of the problem
      n <- 40
      lambda <- 0.2
      numSim <- 1000
      mu <- 1 / lambda
      stdDev <- 1 / lambda
      var.exp <- stdDev ^ 2
      stdErr <- stdDev / sqrt(n)
      
      ## Calculate the mean of 40 exponentials, and do this 1000 times
      sample <- apply(matrix(rexp(numSim * n, rate = lambda), numSim), 1, mean)
      
      ## *** COMPARING MEANS ***
      ## Calculate the sample mean of this data set
      smean <- mean(sample)
      ## Compare the theoretical mean to the sample mean
      delMean <- abs(mu - smean)
      
      ## ** COMPARING VARIANCE ***
      
      
      ## Set up the function to take the mean of the sample, subtract the actual mean,
      ## and divide by the standard error
      cfunc <- function(x, n) mean(x) - mu / stdErr
      
      ## Perform the calculation above for 1000 simulations, putting the result
      ## into a data frame
      dat <- data.frame(x = apply(matrix(rexp(numSim * n, rate = lambda), numSim), 
                                  1, cfunc, n))
      
      
      library(ggplot2)
      g <- ggplot(dat, aes(x = x)) + geom_histogram(alpha = .20, binwidth=.3, 
                                                    colour = "black", aes(y = ..density..))
      g <- g + stat_function(fun = dnorm, size = 2)
      print(g)
      

}

