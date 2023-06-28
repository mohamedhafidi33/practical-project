g <- function(x) {
  return(3*x^3 + x^2 - 8*x + 4)
}

f <- function(x) {
  return(4*x^2 - 3*x + 7)
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


x <- rep(0, 1000)
dct <- list()
for (i in 1:1000) {
  x[i] <- runif(1, 0, 1)
  dct[[as.character(x[i])]] <- g(x[i]) * f(x[i])
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

exact_value <- 6.9933
print(calculate_relative_error(Mc, exact_value))
print(calculate_relative_error(Tr, exact_value))
print(calculate_relative_error(Tr2, exact_value))
