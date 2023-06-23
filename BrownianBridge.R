# Author : Mohamed Hafidi
# Dieses Programm wird für die Simulation der „Brownschen Bridge“ verwendet.

set.seed(3245)

brownianBride <- function(k = 8) {
  y <- rnorm(1, 0, 1)
  
  dic <- list()
  dic[["0"]] <- 0
  dic[["1"]] <- y
  
  for (i in 1:k) {
    cat("==============", i, "=============\n")
    d <- 1
    u <- 0
    t <- 1
    
    x <- dic[["0"]]
    
    s <- d / 2^i
    cat("u =", u, "\n")
    cat("s =", d / 2^i, "\n")
    
    b <- 1 / 2^(i - 1)
    sc <- b
    
    while ((d + 1) != 2^i) {
      d <- d + 2
      t <- sc
      y <- dic[[as.character(t)]]
      
      cat("y =", y, "\n")
      cat("x =", x, "\n")
      cat("t =", t, "and u =", u, "\n")
      
      dic[as.character(s)] <- rnorm(
        1,
        mean = ((t - s) * x + (s - u) * dic[[as.character(t)]]) / (t - u),
        sd = ((s - u) * (t - s)) / (t - u)
      )
      
      x <- y
      u <- sc
      
      cat("t =", t, "\n")
      cat("u =", u, "\n")
      
      s <- d / 2^i
      sc <- sc + b
      
      cat("s =", s, "\n")
      dic[as.character(s)] <- 0
    }
    
    t <- 1
    y <- dic[["1"]]
    
    cat("t =", t, "\n")
    y <- rnorm(
      1,
      mean = ((t - s) * x + (s - u) * y) / (t - u),
      sd = ((s - u) * (t - s)) / (t - u)
    )
    
    dic[as.character(s)] <- y
  }
  
  return(dic)
}

# wir nennen die Funktion 
result <- brownianBride()
print(result)

# Hier zeichnen wir das Ergebnis auf
library(ggplot2)

dic <- brownianBride()

myKeys <- sort(as.numeric(names(dic)))
sorted_dict <- dic[as.character(myKeys)]

print(sorted_dict)

x <- as.numeric(names(sorted_dict))
y <- as.numeric(sorted_dict)

df <- data.frame(x = x, y = y)
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(x = "t", y = "Wt") +
  ggtitle("Brownian Bridge Plot")

# Diese Methode gibt den Wert des angegebenen Zeitpunkts t im Parameter der Funktion „conditionalBrownianBridge“ zurück
conditionalBrownianBridge <- function(t) {
  dic <- brownianBride()
  
  myKeys <- sort(as.numeric(names(dic)))
  sorted_dict <- dic[as.character(myKeys)]
  
  u_index <- which.min(abs(as.numeric(names(sorted_dict)) - t))
  u <- names(sorted_dict)[u_index]
  t_val <- as.numeric(names(sorted_dict))[u_index + 1]
  s <- (t_val + as.numeric(u)) / 2
  x <- sorted_dict[[u]]
  y <- sorted_dict[[as.character(t_val)]]
  
  return(rnorm(1, mean = ((t_val - s) * x + (s - as.numeric(u)) * y) / (t_val - as.numeric(u)), 
               sd = ((s - as.numeric(u)) * (t_val - s)) / (t_val - as.numeric(u))))
}

result <- conditionalBrownianBridge(0.1843)
print(result)

#Trapeze Funktion die Integral zwichen 0 und 1 berechnet
trapeze <- function(array) {
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

result <- trapeze(y)
print(result)
