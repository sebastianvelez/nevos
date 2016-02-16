# Attempt to replicate Nevo's results


#reads data, creates factors and fixes the brand problem (2 brands got the same value "6")
df.c <- read.csv(file = 'cereal.csv')
df.c$id <- as.character(df.c$id)
df.c$firm <- factor(substr(as.character(df.c$id),0,1))
df.c$brand[df.c$brand == 6 & df.c$firm == 3] <- 1
df.c$brand <- factor(as.character(df.c$brand))

df.d <- read.csv(file = 'demog.csv')

ns <- 20     # number of simulated "indviduals" per market %
nmkt <- 94    # number of markets = (# of cities)*(# of quarters)  %
nbrn <- 24    # number of brands per market. if the numebr differs by
