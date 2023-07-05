generatePolynom <- function() {
  N <- rpois(2, lambda = 4)
  N1 <- N[1]
  N2 <- N[2]
  
  C1 <- rbinom(N1+1, size = 10, prob = 0.4)
  C2 <- rbinom(N2+1, size = 10, prob = 0.4)
  
  P <- convolve(C1, C2)
  return(P)
}

convolve <- function(x, y) {
  n <- length(x)
  m <- length(y)
  result <- vector(mode = "numeric", length = n + m - 1)
  
  for (i in 1:(n + m - 1)) {
    start <- max(1, i - (m - 1))
    end <- min(i, n)
    temp_sum <- sum(x[start:end] * rev(y[(i - end + 1):(i - start + 1)]))
    result[i] <- temp_sum
  }
  
  return(result)
}

polynomial <- function(p, default_argument_name = 'x') {
  result <- ''
  
  for (power in seq_along(p)) {
    coefficient <- p[power]
    result <- paste(result, paste0(coefficient, "*", default_argument_name, "^", power-1), "+")
  }
  
  # Remove the trailing `+` sign
  result <- substr(result, 1, nchar(result) - 1)
  
  cat(result)
}

get_polynomial_value <- function(p, x) {
  result <- 0.0
  
  for (power in seq_along(p)) {
    coefficient <- p[power]
    result <- result + coefficient * x^power
  }
  
  return(result)
}


sort_dict <- function(dic) {
  # Get the keys of the dictionary as a list
  myKeys <- names(dic)
  
  # Sort the list of keys
  myKeys <- sort(myKeys)
  
  # Create a new dictionary with sorted keys
  sorted_dict <- dic[myKeys]
  return(sorted_dict)
}

trapezoidal <- function(array) {
  n <- length(array)
  a <- 0
  b <- 1
  h <- (b - a) / n
  sumval <- 0
  for (i in 2:(n - 1)) {
    sumval <- sumval + 2 * array[i]
  }
  sumval <- h * (sumval + array[1] + array[n]) / 2
  return(sumval)
}

trapezoidal_inequivalent <- function(x, y) {
  n <- length(x) - 1  # Number of subintervals
  integral <- 0.0  # Initialize the integral
  
  # Iterate over the subintervals
  for (i in 1:n) {
    h <- x[i+1] - x[i]  # Width of the current subinterval
    area <- (y[i] + y[i+1]) * (h / 2.0)  # Area of the trapezoid
    integral <- integral + area  # Add the area to the integral
  }
  
  return(integral)
}

montecarlo <- function(x, y, a, b) {
  n <- length(x)  # Number of samples
  integral_sum <- 0.0
  
  for (i in 1:n) {
    if (x[i] >= a && x[i] <= b) {  # Only consider samples within the integration bounds
      integral_sum <- integral_sum + y[i]
    }
  }
  
  average <- integral_sum / n
  integral <- (b - a) * average
  return(integral)
}

calculate_relative_error <- function(approximation, exact_value) {
  return(abs(approximation - exact_value) / abs(exact_value))
}


#Test
#for (i in 1:5) {
 # p = generatePolynom()
  #print(p)
  #polynomial(p)
#}

set.seed(42967)
g = generatePolynom()
polynomial(g)
get_polynomial_value(g,0)

x <- rep(0, 1000)
dct <- list()
for (i in 1:1000) {
  x[i] <- runif(1, 0, 1)
  dct[[as.character(x[i])]] <- get_polynomial_value(g,x[i])
}

dct <- sort_dict(dct)
result <- unlist(dct)
x <- as.numeric(names(dct))

Mc <- montecarlo(x, result, 0, 1)
Tr <- trapezoidal(result)
Tr2 <- trapezoidal_inequivalent(x, result)

print(Mc)
print(Tr)
print(Tr2)