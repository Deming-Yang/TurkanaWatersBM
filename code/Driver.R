library(readxl)
library(dplyr)
library(coda)
# library(rjags)
library(R2jags)
library(mcmcplots)
library(bayestestR)

# Load data
waters <- read_excel("./data/Saslaw_et_al_2024.xlsx", sheet = "TableS1")
lakewater <- waters %>%
  filter(Water_type == "Lake")

# extract water isotope values as model input
lw.dD <- lakewater$δD

lw.d18O <- lakewater$δ18O

N <- length(lw.dD) # record the number of lake water measurements

range.TC <- c(25, 31)
range.rh <- c(0.2, 0.8)

# input water isotopes should be close to the value of the omo river
range.d18Oi <- c(-3, 1) #this can be smaller
range.dDi <- c(-30, 15)
range.d18Op <- c(-7, 5) #this can be smaller, too
range.dDp <- c(-40, 30)
range.k <- c(0.2, 0.8)
range.x <- c(0, 1)

# measurement precision
sd.dD <- 0.3

sd.d18O <- 0.1

# parameters to monitor
parameters <- c("TC", "rh", "x", "k", "dDp", "d18Op", "dDi","d18Oi",
                "eD","e18O", "ekD", "ek18O", "dDA","d18OA", "dstarD", "dstar18O",
                "mD", "m18O", "intc", "sl")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( range.TC = range.TC, range.rh = range.rh, range.d18Oi = range.d18Oi,
            range.dDi = range.dDi, range.d18Op = range.d18Op, range.dDp = range.dDp,
            range.k = range.k, range.x = range.x,
            lw.dD = lw.dD, lw.d18O = lw.d18O , N = N, sd.dD = sd.dD, sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS.R", 
                                                     parameters.to.save = parameters, 
                                                     data = dat, n.chains=5, n.iter = n.iter, 
                                                     n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #10 mins

save(post.lw.evp, file = "out/post.lw.evp.RData")

load("out/post.lw.evp.RData")

post.lw.evp$BUGSoutput$summary

# check trace plot for convergence

# check rhat for convergence