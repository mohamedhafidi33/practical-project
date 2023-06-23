n <- 10000
dt <- 1/n
x <- sqrt(dt)*cumsum(sample(c(-1, 1), n, TRUE))
plot((0:(n-1))/n,x,type = 'l')