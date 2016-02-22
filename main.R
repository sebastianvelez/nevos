# Attempt to replicate Nevo's results
library(dplyr)
library(MASS)

rm(list = ls())

#reads data, creates factors and fixes the brand problem (2 brands got the same value "6")----
df.c <- read.csv(file = 'cereal.csv')
df.d <- read.csv(file = 'demogr.csv')
index.dv <- read.csv(file = 'index.vd.csv')
df.v <- read.csv(file = 'v.csv')

df.v <- cbind(index.dv,df.v)

vd <- bind_cols(df.d,df.v)

df.c <- inner_join(df.c,vd,by=c("city", "quarter", "year"))

rm(df.d,df.v,vd,index.dv)

df.c$id <- as.character(df.c$id)
df.c$city  <- as.factor(df.c$city)
df.c$quarter  <- as.factor(df.c$quarter)
df.c$firm <- factor(substr(as.character(df.c$id),0,1))
df.c$brand[df.c$brand == 6 & df.c$firm == 3] <- 1 # avoids having to brands labeled as '6'
df.c$brand <- factor(as.character(df.c$brand))
df.c <- df.c %>%
  group_by(city, quarter)%>%
  mutate(outshr = sum(share))





# numbers that is handy to have around
ns <- 20     # number of simulated "indviduals" per market %
nmkt <- 94    # number of markets = (# of cities)*(# of quarters)  %
nbrn <- 24    # number of brands per market. if the numebr differs by
n_inst <-20    # number of instruments for price


# creates matrices needed----

# creates dummies for brand----
for(t in unique(df.c$brand)) {
  df.c[paste('brand',t,sep='')] <- ifelse(df.c$brand==t,1,0)
}

# needed matrices
iv <- data.matrix(df.c[,c(grep('brand[0-9]+',colnames(df.c)), grep('z[0-9]+',colnames(df.c)))])# matrix of instruments

s_jt <- data.matrix(df.c[,'share']) # matrix with shares

x1 <- data.matrix(df.c[c(10,grep('brand[0-9]+',colnames(df.c)))]) #matrix with data for the linear part

x2 <- data.matrix(df.c[,c(9,10,11,12)]) # matrix for the non-linear part

outshr <- data.matrix(df.c[,'outshr']) # share for the outside option



message <- paste('The dimensions of iv are', dim(iv)[1],dim(iv)[2], sep = ' ')

print(message)



# starting values----
theta2w <- matrix(c(0.2581,2.4317,0.0075,0.1238,3.5819,21.6313,-0.2216,1.4420,0,
                    -1.0233,0,0,0.2847,0,0.0536,-0.9902,0,4.5870,0,0),nrow = 4, ncol = 5)
colnames(theta2w) <- c('sigmas', 'pi1','pi2','pi3','pi4')

# this are the parameters to be estimated. 
#7 are set to zero by assumption the 13 others are: 4 characteristics and 9 interactions.


message <- paste('The dimensions of theta2w are', dim(theta2w)[1],dim(theta2w)[2], sep = ' ')

print(message)



# creates waiting matrix----
invA = ginv(t(iv) %*% iv)
message <- paste('The dimensions of invA are', dim(invA)[1],dim(invA)[2], sep = ' ')

print(message)


# logit results and save mean utility for initial value----

y <- log(s_jt) - log(outshr)
colnames(y) <- 'y'
mid <- t(x1)%*%iv%*%invA%*%t(iv)
t <- solve(mid%*%x1)%*%mid%*%y
mvalold <- x1%*%t
mvalold <- exp(mvalold) # fitted mean utilities to be used as initial values
colnames(mvalold) <- 'mvalold'
oldt2 <- matrix(rep(0,20),nrow = dim(theta2w)[1], ncol = dim(theta2w)[2]) # some empty matrix of size theta2


# creates matrices with random draws (80: 20 individuals * 4 columns in x2) for each market (94)

vfull <- data.matrix(df.c[,grep('v[0-9]+',colnames(df.c))])
dfull <- data.matrix(df.c[,paste('d',1:80, sep = '')])


# This function computes the non-linear part of the utility (mu_ijt in the Guide)----
mufunc <- function(x2, theta2w){
  obs <- dim(x2)[1]
  k <- dim(x2)[2]
  j <- dim(theta2w)[2]-1
  mu <- matrix(,nrow = obs, ncol = ns)
  
  for (i in 1:ns){
    vi <- vfull[,c(i,i+ns,i+2*ns, i+3*ns)]
    di <- dfull[,c(i,i+ns,i+2*ns, i+3*ns)]
    mu[,i] <- (x2*vi)%*%as.matrix(theta2w[,1]) + (x2*(di %*% t(theta2w[,2:(j+1)]))) %*% matrix(rep(1,k),nrow = k,1)
  }
  
  return(mu)
}

# This function computes individual market shares---
ind.sh <- function(expmval, expmu){
  eg <- expmu * matrix(rep(expmval,ns),nrow = obs, ncol = ns )
  temp <- apply(eg, 2, cumsum) 
  sum1 <- temp[seq(nbrn,obs,nbrn),]
  sum1[2:nmkt,] <- diff(sum1)
  
  denom1 <- 1/(1+sum1)
  cdid <- sort(rep(1:nmkt,nbrn))

  denom <- denom1[cdid,]
  f <- eg*denom
}


# This function computes predicted market shares----
mkths <- function(mval, expmu){
  share <- sum(t(indsh(mval, expmu)))/ns
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

  expmu <- exp(mufunc(x2,theta2w))
  norm <- 1
  avgnorm <- 1


  i <- 0
  
  while(norm > tol*10^(flag*floor(i/50) & avgnorm > 1e-3*tol*10^(flag*floor(i/50)))){
    
  }
  
  
}




