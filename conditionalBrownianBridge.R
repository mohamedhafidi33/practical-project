# Author: Mohamed Hafidi
sort_dict <- function(dic) {
  myKeys <- sort(names(dic))
  sorted_dict <- dic[myKeys]
  return(sorted_dict)
}

conditionalBrownianBridge <- function(t, dic) {
  #Generates a conditional Brownian bridge at a given time point 't' based on a dictionary of values.
  
  #Args:
   # t (float): The target time point for generating the bridge.
  #dic (dict): A dictionary containing key-value pairs representing time points and corresponding values.
  
  #returns:
   # dict: A sorted dictionary with the original values and the newly generated value at the target time point.
  nt <- t
  list_dic <- as.numeric(names(dic))
  
  u_index <- which.min(abs(list_dic - t))
  u <- list_dic[u_index]
  
  if (u == 0 || u >= nt) {
    while (u >= nt) {
      u_index <- u_index - 1
      u <- list_dic[u_index]
    }
  }
  
  t_index <- u_index + 1
  t <- list_dic[t_index]
  
  s <- (t + u) / 2
  
  x <- dic[[as.character(u)]]
  y <- dic[[as.character(t)]]
  
  dic[[as.character(nt)]] <- rnorm(1, ((t - s) * x + (s - u) * y) / (t - u), sqrt((s - u) * (t - s) / (t - u)))
  return(sort_dict(dic))
}

dictionary <- list(
  "0" = 0,
  "1" = rnorm(1,0,1)
)

#Testing conditionalBrownianBridge function
set.seed(3245)
for (i in 1:8) {
  u <- runif(1, 0, 1)
  dictionary <- conditionalBrownianBridge(u, dictionary)
}

print(dictionary)

trapezoidal <- function(array) {
  #Calculates the numerical integral of an array using the trapezoidal rule.
  n <- length(array)
  a <- 0
  b <- 1
  h <- (b - a) / n
  sumval <- 0
  for (i in 2:(n-1)) {
    sumval <- sumval + 2 * array[i]
  }
  sumval <- h * (sumval + array[1] + array[n]) / 2
  return(sumval)
}

#Test trapezoidal function
trapezoidal(unlist(dictionary))

montecarlo <- function(dic) {
  #Performs Monte Carlo simulation by calculating the average value of a dictionary.
  return(sum(dic) / length(dic))
}

# Testing montecarlo function and comare it to trapezoidal function.
dictionar <- list(
  "0" = 0,
  "1" = rnorm(1,0,1)
)

for (i in 1:1000) {
  x <- runif(1, 0, 1)
  dictionar <- conditionalBrownianBridge(x, dictionar)
}

Mt <- montecarlo(unlist(dictionar))
print(Mt)
Tl <- trapezoidal(unlist(dictionar))
print(Tl)