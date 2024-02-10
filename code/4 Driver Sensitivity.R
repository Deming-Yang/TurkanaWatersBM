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
mean.TC <- 28
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

# how many time steps the vapor evolve around the mixing between atmosphere and evaporated lake 
t <- 6 
# The model is not sensitive to t, as it will converge towards a d18Ov that result in the LEL

# parameters to monitor
parameters <- c("TC", "f.ev", "k", "dDp", "d18Op", "dDi","d18Oi","f","dstarD", "dstar18O",
                "eD","e18O", "ekD", "ek18O", "dstarD.ev", "dstar18O.ev","dDL.ev","d18OL.ev",
                "dDv.ev", "d18Ov.ev","mD", "m18O", "pre.d18OL", "pre.dDL","dDA","d18OA",
                "intc", "sl", "rh.ev", "dDL","d18OL", "rh","dDA.ev", "d18OA.ev",
                "sl.ev", "intc.ev")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( range.TC = range.TC,
            mean.d18Oi = mean.d18Oi, sd.d18Oi = sd.d18Oi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, 
            mean.dDp = mean.dDp, sd.dDp = sd.dDp,  t = t, N = N, 
            lw.dD = lw.dD, sd.dD = sd.dD, 
            lw.d18O = lw.d18O , sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400 k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.senTC = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS SenTC.R", 
                                           parameters.to.save = parameters, 
                                           data = dat, n.chains=5, n.iter = n.iter, 
                                           n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~10 mins

save(post.lw.evp.senTC, file = "out/post.lw.evp.senTC.RData")

load("out/post.lw.evp.senTC.RData")

post.lw.evp.senTC$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.senTC$BUGSoutput$summary[,8]

#################################################
######## Test 2: sensitivity to rh prior ########
#################################################
# uniformly distributed rh prior
range.rh <- c(0.3, 0.95)

# other priors are the same as the main model

# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 28
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

# how many time steps the vapor evolve around the mixing between atmosphere and evaporated lake 
t <- 6 
# The model is not sensitive to t, as it will converge towards a d18Ov that result in the LEL


# parameters to monitor
parameters <- c("TC", "f.ev", "k", "dDp", "d18Op", "dDi","d18Oi","f","dstarD", "dstar18O",
                "eD","e18O", "ekD", "ek18O", "dstarD.ev", "dstar18O.ev","dDL.ev","d18OL.ev",
                "dDv.ev", "d18Ov.ev","mD", "m18O", "pre.d18OL", "pre.dDL","dDA","d18OA",
                "intc", "sl", "rh.ev", "dDL","d18OL", "rh","dDA.ev", "d18OA.ev",
                "sl.ev", "intc.ev")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, range.rh = range.rh,
            mean.d18Oi = mean.d18Oi, sd.d18Oi = sd.d18Oi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, 
            mean.dDp = mean.dDp, sd.dDp = sd.dDp,  t = t, N = N, 
            lw.dD = lw.dD, sd.dD = sd.dD, 
            lw.d18O = lw.d18O , sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400 k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.senrh = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Senrh.R", 
                                               parameters.to.save = parameters, 
                                               data = dat, n.chains=5, n.iter = n.iter, 
                                               n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~10 mins

save(post.lw.evp.senrh, file = "out/post.lw.evp.senrh.RData")

load("out/post.lw.evp.senrh.RData")

post.lw.evp.senrh$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.senrh$BUGSoutput$summary[,8]

################################################
######## Test 3: sensitivity to k prior ########
################################################
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 28
sd.TC <- 1

# uniformly distributed k prior
range.k <- c(0.5, 1)


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

# how many time steps the vapor evolve around the mixing between atmosphere and evaporated lake 
t <- 6 
# The model is not sensitive to t, as it will converge towards a d18Ov that result in the LEL

# parameters to monitor
parameters <- c("TC", "f.ev", "k", "dDp", "d18Op", "dDi","d18Oi","f","dstarD", "dstar18O",
                "eD","e18O", "ekD", "ek18O", "dstarD.ev", "dstar18O.ev","dDL.ev","d18OL.ev",
                "dDv.ev", "d18Ov.ev","mD", "m18O", "pre.d18OL", "pre.dDL","dDA","d18OA",
                "intc", "sl", "rh.ev", "dDL","d18OL", "rh","dDA.ev", "d18OA.ev",
                "sl.ev", "intc.ev")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, range.k = range.k, 
            mean.d18Oi = mean.d18Oi, sd.d18Oi = sd.d18Oi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, 
            mean.dDp = mean.dDp, sd.dDp = sd.dDp,  t = t, N = N, 
            lw.dD = lw.dD, sd.dD = sd.dD, 
            lw.d18O = lw.d18O , sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400 k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.senk = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Senk.R", 
                                               parameters.to.save = parameters, 
                                               data = dat, n.chains=5, n.iter = n.iter, 
                                               n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~10 mins

save(post.lw.evp.senk, file = "out/post.lw.evp.senk.RData")

load("out/post.lw.evp.senk.RData")

post.lw.evp.senk$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.senk$BUGSoutput$summary[,8]

##########################################################
######## Test 4: sensitivity to d18O inflow prior ########
##########################################################

# normally distributed parameters with prior
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 28
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

# how many time steps the vapor evolve around the mixing between atmosphere and evaporated lake 
t <- 6 
# The model is not sensitive to t, as it will converge towards a d18Ov that result in the LEL

# parameters to monitor
parameters <- c("TC", "f.ev", "k", "dDp", "d18Op", "dDi","d18Oi","f","dstarD", "dstar18O",
                "eD","e18O", "ekD", "ek18O", "dstarD.ev", "dstar18O.ev","dDL.ev","d18OL.ev",
                "dDv.ev", "d18Ov.ev","mD", "m18O", "pre.d18OL", "pre.dDL","dDA","d18OA",
                "intc", "sl", "rh.ev", "dDL","d18OL", "rh","dDA.ev", "d18OA.ev",
                "sl.ev", "intc.ev")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, 
            range.d18Oi = range.d18Oi, range.dDi = range.dDi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, 
            mean.dDp = mean.dDp, sd.dDp = sd.dDp,  t = t, N = N, 
            lw.dD = lw.dD, sd.dD = sd.dD, 
            lw.d18O = lw.d18O , sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 2e6    # 1 million interations
n.burnin = 8e5  # 400 k burnin
n.thin = 400 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.Sen18Oi = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Sen18Oi.R", 
                                           parameters.to.save = parameters, 
                                           data = dat, n.chains=5, n.iter = n.iter, 
                                           n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~10 mins

save(post.lw.evp.Sen18Oi, file = "out/post.lw.evp.Sen18Oi.RData")

load("out/post.lw.evp.Sen18Oi.RData")

post.lw.evp.Sen18Oi$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.Sen18Oi$BUGSoutput$summary[,8]


##########################################################
######## Test 5: sensitivity to d18O precip prior ########
##########################################################

# normally distributed parameters with prior
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 28
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

# how many time steps the vapor evolve around the mixing between atmosphere and evaporated lake 
t <- 6 
# The model is not sensitive to t, as it will converge towards a d18Ov that result in the LEL

# parameters to monitor
parameters <- c("TC", "f.ev", "k", "dDp", "d18Op", "dDi","d18Oi","f","dstarD", "dstar18O",
                "eD","e18O", "ekD", "ek18O", "dstarD.ev", "dstar18O.ev","dDL.ev","d18OL.ev",
                "dDv.ev", "d18Ov.ev","mD", "m18O", "pre.d18OL", "pre.dDL","dDA","d18OA",
                "intc", "sl", "rh.ev", "dDL","d18OL", "rh","dDA.ev", "d18OA.ev",
                "sl.ev", "intc.ev")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, 
            mean.d18Oi = mean.d18Oi, sd.d18Oi = sd.d18Oi,
            range.d18Op = range.d18Op, range.dDp = range.dDp, 
            mean.dDp = mean.dDp, sd.dDp = sd.dDp,  t = t, N = N, 
            lw.dD = lw.dD, sd.dD = sd.dD, 
            lw.d18O = lw.d18O , sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400 k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.Sen18Op = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS Sen18Op.R", 
                                                 parameters.to.save = parameters, 
                                                 data = dat, n.chains=5, n.iter = n.iter, 
                                                 n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~10 mins

save(post.lw.evp.Sen18Op, file = "out/post.lw.evp.Sen18Op.RData")

load("out/post.lw.evp.Sen18Op.RData")

post.lw.evp.Sen18Op$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.Sen18Op$BUGSoutput$summary[,8]

###################################################################
######## Test 6: sensitivity to more evaporated lake water ########
###################################################################

# this is trying to explain the negative shifts in the posterior of lake inflow isotopes
# it is possible that the lake water samples of this study is less evaporated than the
# average lake condition, due to the lack of southern sample locations
# and southern locations likely have more evaporated water

# create a model data set with slightly more evaporated water
# along the LEL
lw.d18O.evap <- lw.d18O + 1 

lw.dD.evap <- lw.dD + 1 * sl.map # plus 1 * slope

# normally distributed parameters with prior
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 28
sd.TC <- 1

# this is mean inflow isotopes of Omo River, Rickett and Johnson, 1996

mean.d18Oi <- -1
sd.d18Oi <- 1

# this is mean precipitation isotopes, OIPC not a good assumption?
# Estimates for latitude 3.496°, longitude 36.0407°, altitude 365 m
mean.d18Op <- 1.2 
sd.d18Op <- 0.7
mean.dDp <- 19
sd.dDp <- 6

# measurement precision
sd.dD <- 0.3

sd.d18O <- 0.1

# how many time steps the vapor evolve around the mixing between atmosphere and evaporated lake 
t <- 6 
# The model is not sensitive to t, as it will converge towards a d18Ov that result in the LEL

# parameters to monitor
parameters <- c("TC", "f.ev", "k", "dDp", "d18Op", "dDi","d18Oi","f","dstarD", "dstar18O",
                "eD","e18O", "ekD", "ek18O", "dstarD.ev", "dstar18O.ev","dDL.ev","d18OL.ev",
                "dDv.ev", "d18Ov.ev","mD", "m18O", "pre.d18OL", "pre.dDL","dDA","d18OA",
                "intc", "sl", "rh.ev", "dDL","d18OL", "rh","dDA.ev", "d18OA.ev",
                "sl.ev", "intc.ev")

# input data, including all environmental parameters and measured lake water isotopes
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, 
            mean.d18Oi = mean.d18Oi, sd.d18Oi = sd.d18Oi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, 
            mean.dDp = mean.dDp, sd.dDp = sd.dDp,  t = t, N = N, 
            lw.dD = lw.dD.evap, sd.dD = sd.dD, 
            lw.d18O = lw.d18O.evap , sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400 k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.Sen.evap = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS final.R", 
                                                 parameters.to.save = parameters, 
                                                 data = dat, n.chains=5, n.iter = n.iter, 
                                                 n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~10 mins

save(post.lw.evp.Sen.evap, file = "out/post.lw.evp.Sen.evap.RData")

load("out/post.lw.evp.Sen.evap.RData")

post.lw.evp.Sen.evap$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.Sen.evap$BUGSoutput$summary[,8]
