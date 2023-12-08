## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  generater_model<-function(B=matrix(c(.2,.3,.3,.1),nrow = 2),p=c(0.3,0.7),n=1000){
#    if(nrow(B)!=ncol(B))
#      return("ERROR")
#    m<-nrow(B)
#    for (i in 1:m) {
#      for (j in 1:m) {
#        if(B[i,j]!=B[j,i])
#          return("ERROR")
#        if(B[i,j]<0)
#          return("ERROR")
#      }
#  
#    }
#    g<- sample(1:m, size = n, replace = TRUE, prob = p)
#    A<-matrix(0,ncol = n,nrow = n)
#    for (i in 1:(n-1)) {
#      for (j in (i+1):n) {
#        A[i,j]<-rbinom(1,1,B[g[i],g[j]])
#        A[j,i]<-A[i,j]
#      }
#    }
#    return(A)
#  }
#  
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  ASE <- function(A,d,lapace="FALSE") {
#  if(nrow(A)!=ncol(A))
#    return("ERROR")
#  n=nrow(A)
#  for (i in 1:n) {
#    for (j in 1:n) {
#      if(A[i,j]!=A[j,i])
#        return("ERROR")
#      if(A[i,j]<0)
#        return("ERROR")
#    }
#  
#  }
#  for (i in 1:n) {
#    A[i,i]<-0
#  }
#  
#  if(lapace=="TRUE"){
#    D<-matrix(0,nrow = n,ncol = n)
#    for (i in 1:n) {
#      D[i,i]=1/sqrt(sum(A[,i]))
#    }
#    A<-D%*%A%*%D
#  }
#  
#  E<-eigen(A)
#  s<-E$values
#  v<-E$vectors
#  v1<-matrix(0,nrow = n,ncol = d)
#  for (i in 1:d) {
#    v1[,i]<-v[,i]*sqrt(abs(s[i]))
#  }
#  return(v1)
#  }
#  
#  
#  

## -----------------------------------------------------------------------------
set.seed(12345)
my.sample<-function(n,A,p){## n numbers of times you wan to test,A the value that the random virable can take, p the probablity coressponding to A it should be as long as A,and sumed to 1.
  if(sum(p)!=1||(length(A)!=length(p)))
    return("ERROR")
  else
    b<-runif(n)
    c<-rep(A[1],n)
    d=0
    for (i in 1:n) {d=0
      for (j in 1:length(p)) {
        d=d+p[j]
        if(d>b[i]){
          c[i]<-A[j]
          break
        }
      }
    }
  return(c)
}
##some examples of this function
my.sample(50,c(1,2,3),c(0.1,0.2,0.7))
my.sample(40,letters[1:4],c(0.1,0.2,0.3,0.4))


## -----------------------------------------------------------------------------
n <- 1000
set.seed(12345)
u <- runif(n)
x <- sign(1/2-u)*log(1-abs(1-2*u)) # F(x) = 1/2*exp(-x), 0<=x<=1/2,F(x)=1-1/2*exp(-x),1/2<x<=1
hist(x, prob = TRUE)
z<-seq(-10,0,.01)
fz<-1/2*exp(z)
lines(-z,fz)
lines(z,fz)

## -----------------------------------------------------------------------------
set.seed(1235)
my.beta<-function(n,a,b){## Let g(x=1),c=beta(a,b)^(-1)
j<-k<-0;y <- numeric(n)
while (k < n) {
u <- runif(1)
j <- j + 1
x <- runif(1) #random variate from g(.)
if (x^(a-1) * (1-x)^(b-1) > u) {
#we accept x
k <- k + 1
y[k] <- x
}
}
  return(y)

}
y<-my.beta(1000,3,2)
hist(y, prob = TRUE)
z<-seq(0,1,0.01)
fz<-beta(3,2)^(-1)*(z^2)*(1-z)
lines(z,fz)

## -----------------------------------------------------------------------------
set.seed(12453)
my.fe<-function(n){
  x<-runif(3*n,min = -1,max = 1)
  y<-numeric(n)
  for (i in 1:n) {
    if((abs(x[3*i])>abs(x[3*i-1]))&&(abs(x[3*i])>abs(x[3*i-2])))
      y[i]<-x[3*i-1]
    else
      y[i]<-x[3*i]
  }
  return(y)
}
y<-my.fe(10000)
hist(y, prob = TRUE)
z<-seq(-1,1,0.01)
fz<-3/4*(1-z^2)
lines(z,fz)

## -----------------------------------------------------------------------------
K=100
p<-matrix(0,nrow = 3,ncol = K)
l <- c(0.5,0.8,1)
d <- 1
n <- 1e6
for (i in 1:K) {
     X <- runif(n,0,d/2)
     Y <- runif(n,0,pi/2)
  for (j in 1:3) {
    p[j,i]<-2*l[j]/d/mean(l[j]/2*sin(Y)>X)
  }
}
cat("l/d:",l,"\n","var:",c(var(p[1,]),var(p[2,]),var(p[3,])))


## -----------------------------------------------------------------------------
K=100
n=1e5
p<-matrix(0,nrow = 2,ncol = K)
for (i in 1:K) {
  U<-runif(n)
  p[1,i]=var((exp(U)+exp(1-U))/2)
  p[2,i]=var(exp(U))
}
cat("simple MC approach","  antithetic variate approach","\n","var:",c(mean(p[2,])/n,mean(p[1,])/n),"\n")
cat("percent reduction:",1-mean(p[1,])/mean(p[2,]))


## -----------------------------------------------------------------------------
n=1e5
A<-matrix(0,nrow = 2,ncol = n)
a<-rnorm(n)
a<-1+abs(a)
theta<-a^{2}*exp(-0.5*a^{2})/(2*exp(-0.5*(1-a)^{2}))
print("the integration is ")
print(mean(theta))


## -----------------------------------------------------------------------------
n=1e5
a=0
b=rep(0,5)
k=1
A<-matrix(0,nrow = 5,ncol = 2000)
for (j in 1:5) {
  b[j]=integrate(exp,lower = -1*j/5,upper = -1*(j-1)/5)$value
  k=1
  while (k<2001) {
    a=rexp(1)
    if((a<j/5)&&(a>(j-1)/5)){
      A[j,k]<-a
      k<-k+1
    }
  }
}

Mean<-rep(0,5)
for (j in 1:5) {
  Mean[j]<-mean(b[j]/(1+A[j,]^2))
}
Var<-rep(0,5)
for (j in 1:5) {
  Var[j]<-var(b[j]/(1+A[j,]^2))
}
print("the integration is ")
print(sum(Mean))
print("the variance of the estimate is ")
print(sum(Var)/2000)


## -----------------------------------------------------------------------------
n <- 20
alpha <- .05
U <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
c(mean(x)+sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE),mean(x)-sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE))
} )
k=0
for (i in 1:1000) {
  if(U[1,i]>2&&U[2,i]<2){
    k<-k+1
  }
}
print(k/1000)


## -----------------------------------------------------------------------------
cat("In this simulation,we choose alpha=0.05","\n")
n <- 20
alpha <- .05
U <- replicate(1000, expr = {
x <- rchisq(n, df = 1)
c(mean(x)+sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE),mean(x)-sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE))
} )
k=0
for (i in 1:1000) {
  if(U[1,i]>1&&U[2,i]<1){
    k<-k+1
  }
}
cat("The Type I error rate with χ2(1) being the sample population:",1-k/1000,"\n")


n <- 20
alpha <- .05
U <- replicate(1000, expr = {
x <- runif(n,min = 0,max = 2)
c(mean(x)+sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE),mean(x)-sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE))
} )
k=0
for (i in 1:1000) {
  if(U[1,i]>1&&U[2,i]<1){
    k<-k+1
  }
}
cat("The Type I error rate with Uniform(0,2) being the sample population:",1-k/1000,"\n")

n <- 20
alpha <- .05
U <- replicate(1000, expr = {
x <- rexp(n)
c(mean(x)+sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE),mean(x)-sqrt(var(x)/n)*qt(alpha/2,df=n-1,lower.tail = FALSE))
} )
k=0
for (i in 1:1000) {
  if(U[1,i]>1&&U[2,i]<1){
    k<-k+1
  }
}
cat("The Type I error rate with Exponential(1) being the sample population:",1-k/1000,"\n")

## -----------------------------------------------------------------------------
set.seed(12345)
M=1000
m=1000
BF<-matrix(0,ncol = 3,nrow = M)
BH<-matrix(0,ncol = 3,nrow = M)
a<-numeric(m*(1-0.05))
b<-numeric(m*0.05)
c<-numeric(m)
for (i in 1:M) {
  a<-runif(m*(1-0.05))
  b<-rbeta(m*0.05,shape1=0.1,shape2=1)
  c<-c(a,b)
  bf<-p.adjust(c,method = "bonferroni")
  bh<-p.adjust(c,method = "BH")
  BF[i,1]<-sum(bf[1:950]<0.1)>0
  BF[i,2]<-sum(bf[1:950]<0.1)/sum(bf<0.1)
  BF[i,3]<-sum(bf[1:950]>0.1)/950
  BH[i,1]<-sum(bh[1:950]<0.1)>0
  BH[i,2]<-sum(bh[1:950]<0.1)/sum(bf<0.1)
  BH[i,3]<-sum(bh[1:950]>0.1)/950
}

## ----results='hold'-----------------------------------------------------------
cat("TYPE\t","FWER\t","FDR\t","TPR","\n")
cat("BF \t",round(mean(BF[,1]),3),"\t",round(mean(BF[,2]),3),"\t",round(mean(BF[,3]),3),"\n")
cat("BH \t",round(mean(BH[,1]),3),"\t",round(mean(BH[,2]),3),"\t",round(mean(BH[,3]),3),"\n")

## -----------------------------------------------------------------------------
n<-c(5,10,20)
B=1000
m=1000
 MEAN<-matrix(0,ncol =3 ,nrow =m )
 VAR<-matrix(0,ncol = 3,nrow = m)
for (i in 1:m) {
 
  for (j in 1:3) {
    x<-rexp(n[j],rate = 2)
    lam<-numeric(B)
    for (k in 1:B) {
      y<-sample(x,replace = TRUE)
      lam[k]<-1/mean(y)
    }
    MEAN[i,j]<-mean(lam)-1/mean(x)
    VAR[i,j]<-sqrt(var(lam))
  }
}
me<-c(mean(MEAN[,1]),mean(MEAN[,2]),mean(MEAN[,3]))
va<-c(mean(VAR[,1]),mean(VAR[,2]),mean(VAR[,3]))

## ----results='hold'-----------------------------------------------------------
cat("n","\t\t\t","5","\t","10","\t","20","\n")
cat("theoretical bias","\t",round(2/(n[1]-1),3),"\t",round(2/(n[2]-1),3),"\t",round(2/(n[3]-1),3),"\n")
cat("bootstrap bias","\t\t",round(me[1],3),"\t",round(me[2],3),"\t",round(me[3],3),"\n")
cat("theoretical sd","\t\t",round(2*n[1]/((n[1]-1)*sqrt(n[1]-2)),3),"\t",round(2*n[2]/(n[2]-1)/sqrt(n[2]-2),3),"\t",round(2*n[3]/(n[3]-1)/sqrt(n[3]-2),3),"\n")
cat("bootstrap sd","\t\t",round(va[1],3),"\t",round(va[2],3),"\t",round(va[3],3),"\n")

## -----------------------------------------------------------------------------
library(bootstrap)

dat1<-matrix(0,nrow = 15,ncol = 2)
for (i in 1:15) {
  dat1[i,1]<-law[i,1]
  dat1[i,2]<-law[i,2]
}
boot.t.ci <-
function(x, B = 2000, R = 200, level = .95, statistic){
#compute the bootstrap t CI
x <- as.matrix(x)
n <- nrow(x)
stat <- numeric(B)
se1 <- numeric(B)
boot.se <- function(x, R, f) {
#local function to compute the bootstrap
#estimate of standard error for statistic f(x)
x <- as.matrix(x); m <- nrow(x)
th <- replicate(R, expr = {
i <- sample(1:m, size = m, replace = TRUE)
f(x[i, ])
})
return(sd(th))
}
for (b in 1:B) {
j <- sample(1:n, size = n, replace = TRUE)
y <- x[j, ]
stat[b] <- statistic(y)
se1[b] <- boot.se(y, R = R, f = statistic)
}
stat00 <- statistic(x)
t.stats <- (stat - stat00) / se1
se00 <- sd(stat)
alpha <- 1 - level

Qt <- quantile(t.stats, probs=c(alpha/2, 1-alpha/2),na.rm = TRUE, type = 1)
names(Qt) <- rev(names(Qt))
CI <- rev(stat00 - Qt * se00)
}


stat <- function(p) {
cor(p[,1],p[,2]) }
ci <- boot.t.ci(dat1, statistic = stat, B=2000, R=200)
print(ci)




## -----------------------------------------------------------------------------
x<-c(3,5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
library("boot")
ci.norm<-ci.basic<-ci.perc<-ci.bca<-numeric(2)
boot.mean<-function(y,i) mean(y[i])
de <- boot(data=x,statistic=boot.mean, R = 1000)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci.norm<-ci$norm[2:3];ci.basic<-ci$basic[4:5]
ci.perc<-ci$percent[4:5];ci.bca<-ci$bca[4:5]


## ----results='hold'-----------------------------------------------------------
cat("bootstrap interval\n")
cat("norm\t",ci.norm,"\n")
cat("basic\t",ci.basic,"\n")
cat("perc\t",ci.perc,"\n")
cat("bca\t",ci.bca,"\n")

## -----------------------------------------------------------------------------
library("bootstrap")
S<-cov(scor)
en<-eigen(S)
theta<-en$values[1]/sum(en$values)


n <- 100
theta.hat1 <- numeric(n)
for (i in 1:n) {
  scor.jack1 <- scor[-i, ]
  sigma.hat1 <- cov(scor.jack1)
  s1 <- eigen(sigma.hat1)
  v1 <- s1$values
  theta.hat1[i] <- v1[1]/sum(v1)
}
bias.jack <- (n-1)*(mean(theta.hat1)-theta)
se.jack <- sqrt((n-1)*mean((theta.hat1-theta)^2))
print(round(c(original = theta, bias.jack = bias.jack, se.jack = se.jack), 4))

## -----------------------------------------------------------------------------
set.seed(12346)
ironslag<-read.csv("ironslag.csv")
m<-ironslag$magnetic
c<-ironslag$chemical
n<-length(m)

e1 <- e2 <- e3 <- e4 <- numeric(n*(n-1)/2)
# for n-fold cross validation
# fit models on leave-one-out samples
for (i in 1:(n-1)) {
  for(j in (i+1):n){
y <- m[-c(i,j)]
x <- c[-c(i,j)]
J1 <- lm(y ~ x)
yhat11 <- J1$coef[1] + J1$coef[2] * c[i]
e1[i*(i-1)/2+j-i] <- (m[i] - yhat11)^2
yhat12 <- J1$coef[1] + J1$coef[2] * c[j]
e1[i*(i-1)/2+j-i] <-e1[i*(i-1)/2+j-i] +(m[j] - yhat12)^2

J2 <- lm(y ~ x + I(x^2))
yhat21 <- J2$coef[1] + J2$coef[2] * c[i] +
J2$coef[3] * c[i]^2
e2[i*(i-1)/2+j-i] <- (m[i] - yhat21)^2
yhat22 <- J2$coef[1] + J2$coef[2] * c[j] +
J2$coef[3] * c[j]^2
e2[i*(i-1)/2+j-i] <-e2[i*(i-1)/2+j-i] +(m[j] - yhat22)^2

J3 <- lm(log(y) ~ x)
logyhat31 <- J3$coef[1] + J3$coef[2] * c[i]
yhat31 <- exp(logyhat31)
e3[i*(i-1)/2+j-i] <- (m[i] - yhat31)^2
logyhat32 <- J3$coef[1] + J3$coef[2] * c[j]
yhat32 <- exp(logyhat32)
e3[i*(i-1)/2+j-i] <- e3[i*(i-1)/2+j-i] +(m[j] - yhat32)^2


J4 <- lm(log(y) ~ log(x))
logyhat41 <- J4$coef[1] + J4$coef[2] * log(c[i])
yhat41 <- exp(logyhat41)
e4[i*(i-1)/2+j-i] <- (m[i] - yhat41)^2
logyhat42 <- J4$coef[1] + J4$coef[2] * log(c[j])
yhat42 <- exp(logyhat42)
e4[i*(i-1)/2+j-i] <- e4[i*(i-1)/2+j-i] +(m[j] - yhat42)^2
  }
}


## ----results='hold'-----------------------------------------------------------
cat("model1\t","model2\t","model3\t","model4\n")
cat(round(mean(e1),2),"\t",round(mean(e2),2),round(mean(e3),2),round(mean(e4),2))

## -----------------------------------------------------------------------------
X<-c(158,171 ,193, 199, 230, 243, 248, 248, 250, 267, 271, 316, 327, 329)
Y<-c(141 ,148, 169, 181, 203, 213, 229, 244, 257, 260, 271, 309)
library("twosamples")
out1<-cvm_test(X,Y)
print(out1)
summary(out1)


## -----------------------------------------------------------------------------
n1 <- 100
n2 <- 150
set.seed(12456)
m <- 500
c5test <- function(x, y, s) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))

return(as.integer(max(c(outx, outy)) > s))
}
x <- rnorm(n1)
y <- rnorm(n2)
s <- 5:15
R <- 100
q <- numeric(R)
alphahat <- pwr <- numeric(length(s))
for (j in 1:length(s)) {
  ss1 <- s[j]
  alphahat[j] <- c5test(x, y, ss1) 
  z <- c(x, y)
  K <- 1:(n1+n2)
  n<-length(x)
  for (i in 1:R) {
  k <- sample(K, size = n, replace = FALSE)
  x1 <- z[k]; y1 <- z[-k] 
  x1 <- x1 - mean(x1) 
 
  y1 <- y1 - mean(y1)
  q[i] <- c5test(x1, y1, ss1)
 }
 pwr[j] <- mean(c(alphahat[j], q))
}
plot(s, pwr, col = "black")




## -----------------------------------------------------------------------------
fa1<-function(N,b1,b2,b3,f0){
  an<-numeric(N)
  for (i in 1:N) {
    x1<-rpois(n=1,lambda = 1)
    x2<-rexp(n=1,rate = 1)
    x3<-rbinom(1,1,prob = 0.5)
    an[i]<-log(f0/(1-f0))-b1*x1-b2*x2-b3*x3
  }
  return(mean(an))
  
}

f<-c(0.1,0.01,0.001,0.0001)
N<-1e6
b1<-0
b2<-1
b3<--1
s<--log(f)
a<-numeric(4)
for (j in 1:4) {
  a[j]<-fa1(N,b1,b2,b3,f[j])
}


## -----------------------------------------------------------------------------
plot(s,a,xlab = "-logf_0")

## -----------------------------------------------------------------------------
set.seed(12445)
rw.Metropolis <- function( sigma, x0, N) {
x <- numeric(N)
x[1] <- x0
u <- runif(N)
k <- 0
for (i in 2:N) {
y <- rnorm(1, x[i-1], sigma)
if (u[i] <=((0.5*exp(-abs(y))) / (0.5*exp(-abs(x[i-1])))))
x[i] <- y else {
x[i] <- x[i-1]
k <- k + 1
}
}
return(list(x=x, k=k))
}


N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis( sigma[1], x0, N)
rw2 <- rw.Metropolis( sigma[2], x0, N)
rw3 <- rw.Metropolis( sigma[3], x0, N)
rw4 <- rw.Metropolis( sigma[4], x0, N)



## -----------------------------------------------------------------------------
print(c(rw1$k, rw2$k, rw3$k, rw4$k))


## -----------------------------------------------------------------------------
print(c(1-rw1$k/N, 1-rw2$k/N,1-rw3$k/N, 1-rw4$k/N))


## -----------------------------------------------------------------------------
 #display 4 graphs together
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
for (j in 1:4) {
plot(rw[,j], type="l",
xlab=bquote(sigma == .(round(sigma[j],3))),
ylab="X", ylim=range(rw[,j]))

}
#reset to default



## -----------------------------------------------------------------------------

#initialize constants and parameters
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- 0.9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
###### generate the chain #####
X[1, ] <- c(mu1, mu2) #initialize
for (i in 2:N) {
x2 <- X[i-1, 2]
m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
X[i, 1] <- rnorm(1, m1, s1)
x1 <- X[i, 1]
m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
X[i, 2] <- rnorm(1, m2, s2)
}
b <- burn + 1
x <- X[b:N, ]




## -----------------------------------------------------------------------------
x1<-x[,1]
y<-x[,2]
model<-lm(y~x1)
summary(model)

## -----------------------------------------------------------------------------

set.seed(1234)
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)

psi.means <- rowMeans(psi) #row means
B <- n * var(psi.means) #between variance est.

psi.w <- apply(psi, 1, "var") #within variances
W <- mean(psi.w) #within est.

v.hat <- W*(n-1)/n + (B/n) #upper variance est.

r.hat <- v.hat / W #G-R statistic

return(r.hat)
}


f <- function(x, sigma) {
if (any(x < 0)) return (0)
stopifnot(sigma > 0)
return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
}

k <- 4 #number of chains to generate
m<-1e5
X <- matrix(0, nrow=k, ncol=m)

for (l in 1:k) {
  m <- 10000
sigma <- 4
x <- numeric(m)
x[1] <- rchisq(1, df=1)
k <- 0
u <- runif(m)
for (i in 2:m) {
xt <- x[i-1]
y <- rchisq(1, df = xt)
num <- f(y, sigma) * dchisq(xt, df = y)
den <- f(xt, sigma) * dchisq(y, df = xt)
if (u[i] <= num/den) x[i] <- y else {
x[i] <- xt
k <- k+1 #y is rejected
}
}

X[l,]<-x
}




k <- 4 #number of chains to generate

b <- 1000 #burn-in length



## -----------------------------------------------------------------------------

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))


#plot the sequence of R-hat statistics
rhat <- rep(0, m)
for (j in (b+1):m)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):m], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)


## ----warning=FALSE------------------------------------------------------------
library("stats4")
n<-10

u<-c(11,8,27,13,16,0,23,10,24,2)
v<-c(12,9,28,14,17,1,24,11,25,3)

mlogL <- function(theta=1) {
# minus log-likelihood
return(-(sum(log(exp(-theta*u)-exp(-theta*v)))))
}
fit<-mle(mlogL)

cat("the observed data likelihood estimate",fit@coef,"\n")

g<-function(x){
  c=sum((u*exp(-x*u)-v*exp(-x*v))/(exp(-x*u)-exp(-x*v)))
  return(x*n/(n+x*c))
}
error<-1e-5
a=1
lambda=1

while (a>error) {
  a<-abs(lambda-g(lambda))
  lambda<-g(lambda)
}

cat("the EM estimate",lambda)

## -----------------------------------------------------------------------------
solve.game <- function(A) {
#solve the two player zero-sum game by simplex method
#optimize for player 1, then player 2
#maximize v subject to ...
#let x strategies 1:m, and put v as extra variable
#A1, the <= constraints
#
min.A <- min(A)
A <- A - min.A #so that v >= 0
max.A <- max(A)
A <- A / max(A)
m <- nrow(A)
n <- ncol(A)
it <- n^3
a <- c(rep(0, m), 1) #objective function
A1 <- -cbind(t(A), rep(-1, n)) #constraints <=
b1 <- rep(0, n)
A3 <- t(as.matrix(c(rep(1, m), 0))) #constraints sum(x)=1
b3 <- 1
sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=TRUE, n.iter=it)
#the ’solution’ is [x1,x2,...,xm | value of game]
#
#minimize v subject to ...
#let y strategies 1:n, with v as extra variable
a <- c(rep(0, n), 1) #objective function
A1 <- cbind(A, rep(-1, m)) #constraints <=
b1 <- rep(0, m)
A3 <- t(as.matrix(c(rep(1, n), 0))) #constraints sum(y)=1
b3 <- 1
sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=FALSE, n.iter=it)
soln <- list("A" = A * max.A + min.A,
"x" = sx$soln[1:m],
"y" = sy$soln[1:n],
"v" = sx$soln[m+1] * max.A + min.A)
soln
}

A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
2,0,0,0,-3,-3,4,0,0,
2,0,0,3,0,0,0,-4,-4,
-3,0,-3,0,4,0,0,5,0,
0,3,0,-4,0,-4,0,5,0,
0,3,0,0,4,0,-5,0,-5,
-4,-4,0,0,0,5,0,0,6,
0,0,4,-5,-5,0,0,0,6,
0,0,4,0,0,5,-6,-6,0), 9, 9)

library(boot) #needed for simplex function

B<-A+2

s1<-solve.game(A)
s2<-solve.game(B)







## -----------------------------------------------------------------------------
 round(cbind(s2$x, s2$y), 7)


## -----------------------------------------------------------------------------
cat("value of game A:",s1$v,"\t value of game B:",s2$v)



## -----------------------------------------------------------------------------
a<-list(c(1,1,3),c(letters[1:3]))
b<-as.vector(a)
c<-unlist(a)
print(b)
print(c)

## -----------------------------------------------------------------------------
a<-numeric(10)
print(dim(a))


## -----------------------------------------------------------------------------
A<-matrix(1:12,ncol = 3)
print(is.matrix(A))
print(is.array(A))



## -----------------------------------------------------------------------------
a<-1:3
b<-letters[1:3]
C<-data.frame(a,b)
D<-as.matrix(C)
print(D)

## -----------------------------------------------------------------------------
m<-matrix(ncol = 0,nrow = 0)

d<-data.frame(m)

print(dim(d))


## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}
a<-matrix(1:20,nrow = 5)

b<-apply(a, 2, scale01)
print(b)



## -----------------------------------------------------------------------------
a<-rnorm(10)
b<-letters[1:10]
c<-rexp(10)
A<-data.frame(a,c)
B<-data.frame(a,b,c)
vapply(A,FUN=sd,FUN.VALUE = 1)
d<-vapply(B, sd, FUN.VALUE =1)
d[!is.na(d)]

## -----------------------------------------------------------------------------
library("Rcpp")
library("microbenchmark")

a=0.5
b=0.5
n=20

gibbsR <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rbinom(1,n,y)
      y <- rbeta(1,x+a,n+b-x)
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

cppFunction('#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int thin) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0,a=0.5,b=0.5,n=20;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rbinom(1,n,y)[0];
      y = rbeta(1,x+a,n+b-x)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}
')




ts <- microbenchmark(gibbR=gibbsR(100,10),
gibbC=gibbsC(100,10))
summary(ts)[,c(1,3,5,6)]



