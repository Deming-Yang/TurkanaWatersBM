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

######## Sensitivity tests on model priors ########

##########################################################
######## Test 1: sensitivity to Temperature prior ########
##########################################################
# uniformly distributed Temp prior
range.TC <- c(24, 32)

# other priors are the same as the main model

# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 29
sd.TC <- 1

# this is mean inflow isotopes of Omo River, Rickett and Johnson, 1996

mean.d18Oi <- -1
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
post.lw.evp.senTC = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS SenTC.R", 
                                           parameters.to.save = parameters, 
                                           data = dat, n.chains=5, n.iter = n.iter, 
                                           n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp.senTC, file = "out/post.lw.evp.senTC.RData")

load("out/post.lw.evp.senTC.RData")

post.lw.evp.senTC$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.senTC$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp.senTC))

# check trace plot for convergence
mcmc_trace(as.mcmc(post.lw.evp.senTC), pars = c("intc", "sl", "k", "x"))

#################################################
######## Test 2: sensitivity to rh prior ########
#################################################
# uniformly distributed rh prior
range.rh <- c(0.2, 0.9)


# other priors are the same as the main model
# this is mean inflow isotopes of Omo River, Rickett and Johnson, 1996

mean.d18Oi <- -1
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
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, range.rh = range.rh, mean.d18Oi = mean.d18Oi,
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
post.lw.evp.senrh = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Senrh.R", 
                                               parameters.to.save = parameters, 
                                               data = dat, n.chains=5, n.iter = n.iter, 
                                               n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp.senrh, file = "out/post.lw.evp.senrh.RData")

load("out/post.lw.evp.senrh.RData")

post.lw.evp.senrh$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.senrh$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp.senrh))

# check trace plot for convergence
mcmc_trace(as.mcmc(post.lw.evp.senrh), pars = c("intc", "sl", "k", "x"))

##########################################################
######## Test 3: sensitivity to d18O inflow prior ########
##########################################################

# normally distributed parameters with prior
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 29
sd.TC <- 1

# uninformative inflow isotopes 
range.d18Oi <- c(-5, 3) 

range.dDi <- c(-30, 30) 

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
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, range.d18Oi = range.d18Oi, range.dDi = range.dDi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, mean.dDp = mean.dDp,
            sd.dDp = sd.dDp, lw.dD = lw.dD, lw.d18O = lw.d18O , N = N, sd.dD = sd.dD, sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 5e6    # 5 million interations
n.burnin = 2e6  # 2 million burnin
n.thin = 1000 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.Sen18Oi = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Sen18Oi.R", 
                                           parameters.to.save = parameters, 
                                           data = dat, n.chains=5, n.iter = n.iter, 
                                           n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp.Sen18Oi, file = "out/post.lw.evp.Sen18Oi.RData")

load("out/post.lw.evp.Sen18Oi.RData")

post.lw.evp.Sen18Oi$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.Sen18Oi$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp.Sen18Oi))

# check trace plot for convergence
mcmc_trace(as.mcmc(post.lw.evp.Sen18Oi), pars = c("intc", "sl", "k", "x"))

##########################################################
######## Test 4: sensitivity to d18O precip prior ########
##########################################################

# normally distributed parameters with prior
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 29
sd.TC <- 1

# this is mean inflow isotopes of Omo River, Rickett and Johnson, 1996

mean.d18Oi <- -1
sd.d18Oi <- 1

# uninformative precip isotopes 
range.d18Op <- c(-4, 4) 

range.dDp <- c(-20, 40) 

# measurement precision
sd.dD <- 0.3

sd.d18O <- 0.1

# parameters to monitor
parameters <- c("TC", "rh", "x", "k", "dDp", "d18Op", "dDi","d18Oi",
                "eD","e18O", "ekD", "ek18O", "dDA","d18OA", "dstarD", "dstar18O",
                "dDv", "d18Ov","mD", "m18O", "pre.d18OL", 
                "intc", "sl")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, range.d18Op = range.d18Op, range.dDp = range.dDp,
            mean.d18Oi = mean.d18Oi, sd.d18Oi = sd.d18Oi, 
            lw.dD = lw.dD, lw.d18O = lw.d18O , N = N, sd.dD = sd.dD, sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 5e6    # 5 million interations
n.burnin = 2e6  # 2 million burnin
n.thin = 1000 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.Sen18Op = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Sen18Op.R", 
                                                 parameters.to.save = parameters, 
                                                 data = dat, n.chains=5, n.iter = n.iter, 
                                                 n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp.Sen18Op, file = "out/post.lw.evp.Sen18Op.RData")

load("out/post.lw.evp.Sen18Op.RData")

post.lw.evp.Sen18Op$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.Sen18Op$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp.Sen18Op))

# check trace plot for convergence
mcmc_trace(as.mcmc(post.lw.evp.Sen18Op), pars = c("intc", "sl", "k", "x"))
