#' @description 
#' The generatePolynom function generates a random polynomial by performing convolution on two sets of coefficients. The degrees of the polynomials can be provided as arguments to the function or randomly generated if not specified.
#' @param N1.
#' @param N2.
#' @returns The function returns the vector P, which represents the coefficients of the generated polynomial.
generatePolynom <- function(N1=NULL,N2=NULL) {
  N <- rpois(2, lambda = 4)
  if(is.null(N1)){ 
    N1 <- N[1]
  }
  if(is.null(N2)){ 
    N2 <- N[2]
  }
  C1 <- sample(-10:10,N1+1)
  C2 <- sample(-10:10,N2+1)
  
  P <- convolve(C1, C2)
  return(P)
}

#' @description 
#' The convolve function performs convolution on two input vectors x and y, and returns the resulting convolution vector.
#' @param x.
#' @param y.
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


#' @description 
#' The polynomial function takes a vector of coefficients and creates a string representation of a polynomial expression using the specified variable name. The resulting polynomial expression is then printed to the console.
#'  @param p.
#'  @param default_argument_name.
#'  
polynomial <- function(p, default_argument_name = 'x') {
  result <- ''
  
  for (power in seq_along(p)) {
    coefficient <- p[power]
    result <- paste(result, paste0(coefficient, "*", default_argument_name, "^", power-1), "+")
  }
  
  # Remove the trailing `+` sign
  result <- substr(result, 1, nchar(result) - 1)
  
  print(result)
}


#'@author Mohamed Hafidi
#' @description 
#' The get_polynomial_value function takes a vector of coefficients p and an input value x, and calculates the value of the polynomial for the given input. The function returns the calculated value.
#' @param p.
#' @param x.
#' @returns result.
get_polynomial_value <- function(p, x) {
  result <- 0.0
  
  for (power in seq_along(p)) {
    coefficient <- p[power]
    result <- result + coefficient * x^(power-1)
  }
  
  return(result)
}

#' @description 
#' The integrate_polynom function takes a vector of polynomial coefficients A and calculates the definite integral of the polynomial over the interval [X_1, X_2]. The function returns the calculated value of the definite integral.
#'  @param A.
#'  @param X_1.
#'  @param X_2.
#'  @returns integral.
integrate_polynom <- function(A, X_1, X_2) {
  integral <- 0
  n <- length(A)
  A <- rev(A)
  for (i in 1:n) {
    integral <- integral + A[i] * (X_2^((n - i + 1)) - X_1^((n - i + 1))) / (n - i + 1)
  }
  
  return(integral)
}

#' @description 
#' The derivate_polynom function takes a vector of polynomial coefficients polynom and calculates the derivative of the polynomial. The function returns a vector representing the coefficients of the derivative polynomial.
#' @param polynom.
#' @return deriv_poly.
derivate_polynom <- function(polynom) {
  polynom <- rev(polynom)
  deriv_poly <- polynom[-1] * (1:length(polynom[-1]))
  return(rev(deriv_poly))
}

#' @description 
#' The sort_dict function takes a dictionary dic and sorts it based on its keys in ascending order. The function returns the sorted dictionary.
#' @param dic.
#' @return sorted_dict.
sort_dict <- function(dic) {
  # Get the keys of the dictionary as a list
  myKeys <- names(dic)
  
  # Sort the list of keys
  myKeys <- sort(myKeys)
  
  # Create a new dictionary with sorted keys
  sorted_dict <- dic[myKeys]
  return(sorted_dict)
}

#' @description 
#' The trapezoidal function approximates the definite integral using the trapezoidal rule based on a vector of function values. The function returns the calculated approximation of the integral.
#' @param array.
#' @return sumval.
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

#' @description 
#' The trapezoidal_inequivalent function takes vectors of x-values x and corresponding function values y and approximates the definite integral using the trapezoidal rule with unequal subintervals. The function returns the calculated approximation of the integral.
#' @param x.
#' @param y.
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

#' @describeIn 
#' The montecarlo function estimates the definite integral using the Monte Carlo method based on samples and their corresponding function values within specified integration bounds. The function returns the calculated approximation of the integral.
#' @param x.
#' @param y.
#' @param a.
#' @param b.
#' @return integral.
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

#' @description 
#' The calculate_relative_error function takes an approximation and an exact value, and calculates the relative error between them. The function returns the calculated relative error.
#' @param approximation.
#' @param exact_value.
calculate_relative_error <- function(approximation, exact_value) {
  return(abs(approximation - exact_value) / abs(exact_value))
}


#'
#'Testing script that evaluates the performance of different methods for approximating the definite integral of randomly generated polynomials.
#'

set.seed(42967)
times=100
Mc_fehler=0
Tr_fehler=0
Tr2_fehler=0
for (i in 1:times) {
  p = generatePolynom()
  polynomial(p)
  x <- rep(0, 1000)
  dct <- list()
  for (i in 1:1000) {
    x[i] <- runif(1, 0, 1)
    dct[[as.character(x[i])]] <- get_polynomial_value(p,x[i])
  }
  
  dct <- sort_dict(dct)
  result <- unlist(dct)
  x <- as.numeric(names(dct))
  
  Mc <- montecarlo(x, result, 0, 1)
  Tr <- trapezoidal(result)
  Tr2 <- trapezoidal_inequivalent(x, result)
  true_value = integrate_polynom(p,0,1)
  Mc_fehler <- Mc_fehler + calculate_relative_error(Mc,true_value)
  Tr_fehler <- Tr_fehler + calculate_relative_error(Tr,true_value)
  Tr2_fehler <- Tr2_fehler + calculate_relative_error(Tr2,true_value)
  print(true_value)
  print(Mc)
  print(Tr)
  print(Tr2)
}

# Finally, the average relative errors for each method are calculated by dividing the cumulative errors by the number of iterations times, and the results are printed to the console.
print(Mc_fehler/times)
print(Tr_fehler/times)
print(Tr2_fehler/times)