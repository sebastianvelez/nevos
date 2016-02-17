# Attempt to replicate Nevo's results
library(dplyr)
library(MASS)

#reads data, creates factors and fixes the brand problem (2 brands got the same value "6")----
df.c <- read.csv(file = 'cereal.csv')
df.c$id <- as.character(df.c$id)
df.c$firm <- factor(substr(as.character(df.c$id),0,1))
df.c$brand[df.c$brand == 6 & df.c$firm == 3] <- 1 # avoids having to brands labeled as '6'
df.c$brand <- factor(as.character(df.c$brand))

df.c <- df.c %>%
  group_by(city, quarter) %>%
  mutate(outshr = 1 - sum(share))

df.d <- read.csv(file = 'demog.csv')


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
s_jt <- data.matrix(df.c[,8]) # matrix with shares
x1 <- data.matrix(df.c[c(9,grep('brand[0-9]+',colnames(df.c)))]) #matrix with data for the linear part
x2 <- data.matrix(df.c[,c(9,10,11)]) # matrix for the non-linear part
outshr <- data.matrix(df.c[,56]) # share for the outside option



message <- paste('The dimensions of iv are', dim(df.c[,iv])[1],dim(df.c[,iv])[2], sep = ' ')

print(message)



# starting values----
theta2w <- matrix(c(0.3302, 5.4819,   0,      0.2037, 0,
                               2.4526, 15.8935, -1.2000, 0,      2.6342,
                               0.0163, -0.2506,  0,      0.0511, 0,
                               0.2441, 1.2650,   0,     -0.8091, 0),nrow = 4, ncol = 5)

# this are the parameters to be estimated. 
#7 are set to zero by assumption the 13 others are: 4 characteristics and 9 interactions.


message <- paste('The dimensions of theta2w are', dim(theta2w)[1],dim(theta2w)[2], sep = ' ')

print(message)



# creates waiting matrix----
invA = solve(t(iv) %*% iv)
message <- paste('The dimensions of invA are', dim(invA)[1],dim(invA)[2], sep = ' ')

print(message)


# logit results and save mean utility for initial value

y <- log(s_jt) - log(outshr)
colnames(y) <- 'y'
mid <- t(x1)%*%iv%*%invA%*%t(iv)
t = solve(mid%*%x1)%*%mid%*%y
mvalold <- x1%*%t
mvalold <- exp(mvalold)# fitted mean utilities to be used as initial values
oldt2 <- matrix(,nrow = dim(theta2)[1], ncol = dim(theta2)[2]) # some empty matrix of size theta2




















