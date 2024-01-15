library(readxl)
library(dplyr)
library(coda)
library(R2jags)
library(ggplot2)
library(mcmcplots)


# Load data
waters <- read_excel("./data/Saslaw_et_al_2024.xlsx", sheet = "TableS1")
lakewater <- waters %>%
  filter(Water_type == "Lake")

# extract water isotope values as model input
lw.dD <- lakewater$δD

lw.d18O <- lakewater$δ18O

N <- length(lw.dD) # record the number of lake water measurements

range.TC <- c(24, 32)
range.rh <- c(0.4, 0.9)
range.k <- c(0.2, 0.8) 
range.x <- c(0, 1)

# normally distributed parameters with prior
mean.d18Oi <- -1 #this is mean inflow isotopes

# this is mean precipitation isotopes, OIPC
# Estimates for latitude 3.496°, longitude 36.0407°, altitude 365 m
mean.d18Op <- 1.2 
sd.d18Op <- 0.7
mean.dDp <- 19
sd.dDp <- 6

# measurement precision
sd.dD <- 0.3

sd.d18O <- 0.1

# parameters to monitor
parameters <- c("TC", "rh", "x", "k", "dDp", "d18Op", "dDi","d18Oi",
                "eD","e18O", "ekD", "ek18O", "dDA","d18OA", "dstarD", "dstar18O",
                "dDv", "d18Ov","mD", "m18O", "pre.d18OL", "pre.d18Oi", "pre.d18Op", 
                "intc", "sl")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( range.TC = range.TC, range.rh = range.rh, mean.d18Oi = mean.d18Oi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, mean.dDp = mean.dDp,
            sd.dDp = sd.dDp, range.k = range.k, range.x = range.x,
            lw.dD = lw.dD, lw.d18O = lw.d18O , N = N, sd.dD = sd.dD, sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 5e6    # 5 million interations
n.burnin = 2e6  # 2 million burnin
n.thin = 1000 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS.R", 
                                                     parameters.to.save = parameters, 
                                                     data = dat, n.chains=5, n.iter = n.iter, 
                                                     n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp, file = "out/post.lw.evp.RData")

load("out/post.lw.evp.RData")

post.lw.evp$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp))

# check trace plot for convergence
mcmc_trace(as.mcmc(post.lw.evp), pars = c("intc", "sl", "k", "x"))

################### try version with beta distribution ##################
# extract water isotope values as model input
lw.dD <- lakewater$δD

lw.d18O <- lakewater$δ18O

N <- length(lw.dD) # record the number of lake water measurements

range.TC <- c(24, 32)
# range.rh <- c(0.4, 0.9)
# range.k <- c(0.2, 0.8) 
# range.x <- c(0, 1)

# normally distributed parameters with prior
mean.d18Oi <- -1 #this is mean inflow isotopes
sd.d18Oi <- 1

# this is mean precipitation isotopes, OIPC
# Estimates for latitude 3.496°, longitude 36.0407°, altitude 365 m
mean.d18Op <- 1.2 
sd.d18Op <- 0.7
mean.dDp <- 19
sd.dDp <- 6

# measurement precision
sd.dD <- 0.3

sd.d18O <- 0.1

# parameters to monitor
parameters <- c("TC", "rh", "x", "k", "dDp", "d18Op", "dDi","d18Oi",
                "eD","e18O", "ekD", "ek18O", "dDA","d18OA", "dstarD", "dstar18O",
                "dDv", "d18Ov","mD", "m18O", "pre.d18OL", 
                "intc", "sl")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( range.TC = range.TC, mean.d18Oi = mean.d18Oi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, mean.dDp = mean.dDp,
            sd.dDp = sd.dDp, sd.d18Oi = sd.d18Oi,
            lw.dD = lw.dD, lw.d18O = lw.d18O , N = N, sd.dD = sd.dD, sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 5e6    # 5 million interations
n.burnin = 2e6  # 2 million burnin
n.thin = 1000 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.b = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS beta.R", 
                                         parameters.to.save = parameters, 
                                         data = dat, n.chains=5, n.iter = n.iter, 
                                         n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp.b, file = "out/post.lw.evp.b.RData")

load("out/post.lw.evp.b.RData")

post.lw.evp.b$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.b$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp.b))

# check trace plot for convergence
mcmc_trace(as.mcmc(post.lw.evp.b), pars = c("intc", "sl", "k", "x"))
