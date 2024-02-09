library(readxl)
library(dplyr)
library(coda)
library(R2jags)
library(ggplot2)
library(mcmcplots)
library(bayesplot)


# Load data
waters <- read_excel("./data/Saslaw_et_al_2024.xlsx", sheet = "TableS1")
lakewater <- waters %>%
  filter(Water_type == "Lake")

# extract water isotope values as model input
lw.dD <- lakewater$δD

lw.d18O <- lakewater$δ18O

N <- length(lw.dD) # record the number of lake water measurements

# normally distributed parameters with prior
# surface temperature data approximated from Thirumalai et al. 2023
mean.TC <- 28
sd.TC <- 1

# this is mean inflow isotopes of Omo River, Rickett and Johnson, 1996

mean.d18Oi <- -1
sd.d18Oi <- 1

#meteoric source
mean.dDi <- mean.d18Oi * 8 + 10
sd.dDi <- 8

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
dat = list( mean.TC = mean.TC, sd.TC = sd.TC, mean.d18Oi = mean.d18Oi,
            mean.dDi = mean.dDi, sd.dDi = sd.dDi,
            mean.d18Op = mean.d18Op, sd.d18Op = sd.d18Op, mean.dDp = mean.dDp,
            sd.dDp = sd.dDp, sd.d18Oi = sd.d18Oi, t = t,
            lw.dD = lw.dD, lw.d18O = lw.d18O , N = N, sd.dD = sd.dD, sd.d18O = sd.d18O)

#Start time
t1 = proc.time()

set.seed(t1[3])
n.iter = 1e6    # 1 million interations
n.burnin = 4e5  # 400 k burnin
n.thin = 200 #record data every 200 iterations, total data points: 15000

#Run it
post.lw.evp.f = do.call(jags.parallel,list(model.file = "code/Ev mod JAGS final.R", 
                                           parameters.to.save = parameters, 
                                           data = dat, n.chains=5, n.iter = n.iter, 
                                           n.burnin = n.burnin, n.thin = n.thin))

#Time taken
proc.time() - t1 #~40 mins

save(post.lw.evp.f, file = "out/post.lw.evp.f.RData")

load("out/post.lw.evp.f.RData")

post.lw.evp.f$BUGSoutput$summary

# check rhat for convergence: Rhat < 1.01 means ample sampling with good convergence
post.lw.evp.f$BUGSoutput$summary[,8]

# check the density of parameters
denplot(as.mcmc(post.lw.evp.f), parms =c("TC", "rh.ev","dDi","d18Oi","k",
                                          "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", "dDL.ev","d18OL.ev",
                                          "f.ev","sl.ev", "intc.ev"))
