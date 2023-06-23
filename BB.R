set.seed(123)
N <- 100
T <- 1
Delta <- T/N
W <- numeric(N+1)
t <- seq (0,T,length=N+1)
for (i in 2:(N+1))
 W[i] <- W[i-1]+rnorm(1)*sqrt(Delta)
x <- 0
y <- 2
BB <- x + W - t /T * (W[N+1] - y + x)
plot (t,BB,type="l")

n <- 10
u <- 0
t <- 1
x <- 0
y <- 1
rnorm(1,0.482,1/8)
tm = seq(0,1,1/4)
tm
BB <- rep(0,5)
BB[5] <- 0.24
BB[1] <- 0
BB[2] <- 0.447
BB[3] <- 0.724
BB[4] <- 0.426
  #z <- rnorm(1)
x <- BB[1]
y <- BB[5]
s <- (t+u)/2
BB[3] <- ((t-s)*x+(s-u)*y)/(t-u) + sqrt(((s-u)*(t-s))/(t-u))*rnorm(1)
t <- s
y <- BB[3]
s <- (t+u)/2
BB[2] <- ((t-s)*x+(s-u)*y)/(t-u) + sqrt(((s-u)*(t-s))/(t-u))*rnorm(1)
t <- 1
u <- 1/2
s <- (t+u)/2
x <- BB[3]
y <- BB[5]
BB[4] <- ((t-s)*x+(s-u)*y)/(t-u) + sqrt(((s-u)*(t-s))/(t-u))*rnorm(1)

BB
plot(tm,BB,'l')
u <- s
x <- BB[i]

BB
plot(tm,BB,'l')
