set.seed(54)
alpha=0
sigma=1
X0=0
t = seq(0,1,0.001)
n = length(t)
X = rep(0,n)
Z = rnorm(n,mean=0,sd=1)
X[1] = X0+alpha*t[1]+sigma*sqrt(t[1])*Z[1]
for(i in 2:n){
  X[i] = X[i-1]+alpha*(t[i]-t[i-1])+sigma*sqrt(t[i]-t[i-1])*Z[i]
}
plot(t,X,type='l')