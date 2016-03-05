# This script replicates the results in Nevo's Practicioners guide
# the original data was available at http://www.rasmusen.org/zg604/lectures/blp/frontpage.htm
# as of Feb 29 2016 or in the repository from where this file was downloaded


library(dplyr)
library(MASS)


rm(list = ls()) # makes sure the workspace is clean and tidy


# PRE-ESTIMATION----
 # this section does some housekeeping and tidying up to get all the things needed

# WRANGLING DATA
#reads data, creates factors and fixes the brand problem (2 brands got the same value "6")
df.c <- read.csv(file = 'cereal.csv') # price, shares, identifiers, sugar, mushy, and instruments.
df.d <- read.csv(file = 'demogr.csv') # income, incomesq, age, child, for 20 individuals drawn from CPS
index.dv <- read.csv(file = 'index.vd.csv') # identifier for demografics and individual shocks
v <- read.csv(file = 'v.csv') # unobserved individual shocks

# adds identifier column to observed and unobserved individual characteristics (demographics and nus)
df.v <- cbind(index.dv,v)
vd <- bind_cols(df.d,df.v)
df.c <- inner_join(df.c,vd,by=c("city", "quarter", "year"))


df.c$id <- as.character(df.c$id)
df.c$city  <- as.factor(df.c$city)
df.c$quarter  <- as.factor(df.c$quarter)
df.c$firm <- factor(substr(as.character(df.c$id),0,1))
df.c$brand[df.c$brand == 6 & df.c$firm == 3] <- 1 # avoids having to brands labeled as '6'
df.c$brand <- factor(as.character(df.c$brand))
df.c <- df.c %>%
  group_by(city, quarter)%>%
  mutate(outshr = 1 - sum(share))



# numbers that is handy to have around
ns <- 20     # number of simulated "indviduals" per market %
nmkt <- 94    # number of markets = (# of cities)*(# of quarters)  %
nbrn <- 24    # number of brands per market. if the numebr differs by
n.inst <-20    # number of instruments for price
obs <- 2256 # number of observations

cdid <- sort(rep(1:nmkt,nbrn))

cdindex <- seq(nbrn,obs,nbrn)

# creates dummies for brand
for(t in unique(df.c$brand)) {
  df.c[paste('brand',t,sep='')] <- ifelse(df.c$brand==t,1,0)
}

# needed matrices
iv <- data.matrix(df.c[,c(grep('z[0-9]+',colnames(df.c)), grep('brand[0-9]+',colnames(df.c)))])# matrix of instruments
s_jt <- data.matrix(df.c[,'share']) # matrix with shares
x1 <- data.matrix(df.c[c(10,grep('brand[0-9]+',colnames(df.c)))]) #matrix with data for the linear part
x2 <- data.matrix(df.c[,c(9,10,11,12)]) # matrix for the non-linear part
outshr <- data.matrix(df.c[,'outshr']) # share for the outside option

print(paste('The dimensions of iv are', dim(iv)[1],dim(iv)[2], sep = ' '))


# STARTING VALUES

# the following matrix contains the starting values of the parameters. 
# 7 are set to zero by assumption the 13 others are: 4 characteristics and 9 interactions.

theta2w <- matrix(c(0.3302,2.4526,0.0163,0.2441,5.4819, 15.8935,-0.2506,1.2650,
                    0,-1.2000,0,0,0.2037,0, 0.0511,-0.8091,0,2.6342,0,0),nrow = 4, ncol = 5)
colnames(theta2w) <- c('sigmas', 'inc','incsq','age','child')

here <- which(!theta2w==0) # indices of parameters that will be estimated. 

theta2 <- as.vector(theta2w[!theta2w==0]) # gets the non-zero elements of theta2w

oldt2 <- c(rep(0,13)) # initializes an empty matrix to store old results to be compared with newer iterations


print(paste('The dimensions of theta2w are', dim(theta2w)[1],dim(theta2w)[2], sep = ' '))


# WEIGHTING MATRIX
invA = ginv(t(iv) %*% iv)
print(paste('The dimensions of invA are', dim(invA)[1],dim(invA)[2], sep = ' '))


# LOGIT
y <- log(s_jt) - log(outshr)
colnames(y) <- 'y'
mid <- t(x1)%*%iv%*%invA%*%t(iv)
t <- solve(mid%*%x1)%*%mid%*%y # parameters of the simple logit
mvalold <- x1%*%t
mvalold <- exp(mvalold) # fitted mean utilities to be used as initial values for the deltas
colnames(mvalold) <- 'mvalold'


# INDIVIDUAL CHARACTERISTICS
# creates matrices with random draws (80: 20 individuals * 4 columns in x2) for each market (94)
vfull <- data.matrix(df.c[,grep('v[0-9]+',colnames(df.c))])
dfull <- data.matrix(df.c[,paste('d',1:80, sep = '')])


# This function computes the non-linear part of the utility (mu_ijt in the Guide)----
mufunc <- function(x2, theta2w){
  # takes the observed good's characteristics, the individual's observed (demo) and unoobserved
  # characteristics (v) to compute the individual utility (mu) te obtain later the individual
  # 'shares'
  obs <- dim(x2)[1]
  k <- dim(x2)[2]
  j <- dim(theta2w)[2]-1
  mu <- matrix(,nrow = obs, ncol = ns) # empty matrix to store results of the for-loop
  
  for (i in 1:ns){
    vi <- vfull[,c(i,i+ns,i+2*ns, i+3*ns)] # first 20 colums are age for 20 individuals, next 20
    di <- dfull[,c(i,i+ns,i+2*ns, i+3*ns)] # are income, next 20 are income sq ...
    mu[,i] <- (x2*vi)%*%as.matrix(theta2w[,1]) + (x2*(di %*% t(theta2w[,2:(j+1)]))) %*% matrix(rep(1,k),nrow = k,1)
  }
  
  return(mu)
}



# This function computes individual market shares----
ind.sh <- function(expmval, expmu){
  eg <- expmu * matrix(rep(expmval,ns),nrow = obs, ncol = ns )
  temp <- apply(eg, 2, cumsum) 
  sum1 <- temp[cdindex,]
  sum1[2:nmkt,] <- diff(sum1)
  
  denom1 <- 1/(1+sum1)
  cdid <- sort(rep(1:nmkt,nbrn))

  denom <- denom1[cdid,]
  f <- eg*denom
}



# This function computes average market shares----
mktsh <- function(expmval, expmu){
  share <- matrix(rowMeans(ind.sh(expmval, expmu)),obs,1)
  return(share)
}



# This function computes the mean value (this is the contraction on delta)----
meanval <- function(theta2){
  
  if (max(abs(theta2-oldt2)) < 0.01){
    tol <- 1e-9
    flag <- 0
  } else {
    tol <- 1e-6
    flag <- 1
  }
  
  # next 2 lines turn the vector theta2 into a 4x5 matrix like theta2w
  theta3 <- matrix(0,nrow = 4, ncol = 5)
  theta3[here] <- theta2

  expmu <- exp(mufunc(x2,theta3))
  norm <- 1
  avgnorm <- 1


  i <- 0
  
  # next step is the actual contraction that iterates until finding a delta^(h+1) = delta^h
  while(norm > tol*10^(flag*floor(i/50)) & avgnorm > 1e-3*tol*10^(flag*floor(i/50))){
    
    mval <- mvalold*s_jt/mktsh(mvalold,expmu)
    
    t <- abs(mval - mvalold)
      norm <- max(t)
      avgnorm <- mean(t)
      mvalold <- mval
    i <- i+1
  }
  print(paste('# of iteration for delta convergence', as.character(i) ,sep = ' '))
  
  if (flag==1 & max(is.na(mval)) < 1) {
    mvalold <- mval
    oldt2 <- theta2
  }
   return(log(mval))
}




# This function computes the GMM objective function, f is the objective and df is the gradient----

gmmobj <- function(theta2){
  delta <- meanval(theta2)
  # the folloowing deals with cases were the min algorithm drifts into regions
  # where the objective is not defined
  if(max(is.na(delta)) == 1){
    f = 1e10
  } else{
    temp1 <- t(x1)%*%iv
    temp2 <- t(delta)%*%iv
    theta1 <<- ginv(temp1%*%invA%*%t(temp1)) %*% temp1 %*% invA%*% t(temp2) # alpha and brand dummies
    rm(temp1,temp2)
    gmmresid <<- delta - x1%*%theta1 # gmm residual are needed for var.cov()
    temp1 <- t(gmmresid)%*%iv
    f1 <- temp1%*%invA%*%t(temp1)
    f <- f1[1,1]
  }
  return(f)
  print(paste('GMM objective: ', as.character(f)))
  
}

# ESTIMATION ----

# theta2 (the nonlinear parameters as table 1 in the Guide)
ptm <- proc.time()
a <- optim(par = theta2, fn = gmmobj, control = list(trace=1))
(proc.time()-ptm)/60



# POST-ESTIMATION ----

# This function estimates the Jacobian 
# (I should try and remove the for-loop if possible, it maybe faster if vecotrized)

jacob <- function(mval,theta2){
  
  theta3 <- matrix(0,nrow = 4, ncol = 5)
  theta3[here] <- theta2
  
  expmu <- exp(mufunc(x2,theta3))
  shares <- ind.sh(mval,expmu)
  K <- dim(x2)[2]
  J <- dim(theta3)[2] - 1
  f1 <- matrix(,nrow = 2256, K*(J+1))
  
# this part computes (partial share)/ (partial sigma)
  for (i in 1:K){
    xv <- matrix(rep(x2[,i],20),nrow = 2256, ncol = 20) * v[cdid, (ns*(i-1)+1):(ns*i)]
    temp <- cumsum(xv*shares)
    sum1 <- temp[cdindex, ]
    sum1[2:94,] <- apply(sum1, 2, diff)
    f1[,i] <- t(apply(t(shares*(xv-sum1[cdid,])), 2, mean))
  }
  
# this part computes (partial share) / (partial pi). Comment out if no demographics
  for (j in 1:J){
    d <- df.d[cdid, ((j-1)+1):(ns*j)]
    temp1 <- matrix(, nrow = 2256, K)
    for (i in 1:K){
      xd <- matrix(rep(x2[,i],20),nrow = 2256, ncol = ns) * d
      temp <- cumsum(xd*shares)
      sum1 <- temp[cdindex, ]
      sum1[2:94,] <- apply(sum1, 2, diff)
      temp1[, i] <- t(apply(t(shares*(xd-sum1[cdid,])),2,mean))
      }
  
  f1[, (K*j+1):(K*(j+1))] <- temp1
  }
  f <- matrix(,2256,13)
  n <- 1
# this part computes (partial delta) / (partial theta2)
  for (i in 1:94) {
    temp <- shares[n:cdindex[i],]
    H1 <- temp %*% t(temp)
    H <- (diag(apply(t(temp), 2, sum))-H1)/ns
    f[n:cdindex[i],] <- -ginv(H) %*% f1[n:cdindex[i],c(1, 2, 3, 4, 5, 6, 7, 8, 10, 13, 15, 16, 18)]
    n <- cdindex[i] + 1
  }
  return(f)
}



# This function creates the matrix of variances covariances of the estimates
var.cov <- function(theta2){
  obs <- length(s_jt)
  nz <- nbrn+n.inst
  temp <- jacob(mvalold,theta2)
  a <- t(cbind(x1,temp)) %*% iv
  ivres <- iv * (gmmresid %*% matrix(rep(1,44),nrow = 1, ncol = 44))
  b <- t(ivres) %*% ivres
  
  f = ginv(a %*% invA %*% t(a))%*%a%*%invA%*%b%*%invA%*%t(a)%*%ginv(a %*% invA %*% t(a))
  
}



# obtaining the SE for te nonlinear parameters
vcov <- var.cov(a$par)
se <- sqrt(diag(vcov))

theta2w.hat <- matrix(0,nrow = 4, ncol = 5)
theta2w.hat[here] <- as.vector(a$par)
se2w <- se[26:38]

se2w.hat<- matrix(0,nrow = 4,ncol = 5)
se2w.hat[here] <- se2w


# the minimum distance estimates

omega <- ginv(vcov[2:25,2:25])
xmd <- x2[1:24,-2]
ymd <- theta1[2:25]
beta <- ginv(t(xmd)%*%omega%*%xmd)%*%t(xmd)%*%omega%*%ymd
resmd <- ymd - xmd%*%beta  
semd <- sqrt(diag(ginv(t(xmd)%*%omega%*%xmd)))
  
  
