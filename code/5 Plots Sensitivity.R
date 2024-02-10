library(scales)
library(viridisLite)
library(ggplot2)
library(mcmcplots)
library(bayestestR)
library(bayesplot)

##########################################################
######## Test 1: sensitivity to Temperature prior ########
##########################################################
# check the density of parameters
###### Fig S6 ######
denplot(as.mcmc(post.lw.evp.senTC), 
        parms =c("TC", "rh.ev","dDi","d18Oi","k", 
                 "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", 
                 "dDL.ev","d18OL.ev","dDp","d18Op",
                 "f.ev","sl.ev", "intc.ev"))
# f is not sensitive to TC prior

post.f.TC <- post.lw.evp.senTC$BUGSoutput$sims.list$f.ev

# maximum a posteriori estimate
f.map.TC <- map_estimate(post.f.TC)[[1]]

# highest density interval, CI = 0.95
f.hdi025.TC <- hdi(post.f.TC, ci = 0.95)[[2]]
f.hdi975.TC <- hdi(post.f.TC, ci = 0.95)[[3]]


#################################################
######## Test 2: sensitivity to rh prior ########
#################################################
# check the density of parameters
###### Fig S7 ######
denplot(as.mcmc(post.lw.evp.senrh), 
        parms =c("TC", "rh.ev","dDi","d18Oi","k", 
                 "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", 
                 "dDL.ev","d18OL.ev","dDp","d18Op",
                 "f.ev","sl.ev", "intc.ev"))
#f is not sensitive to rh priors. Both rh and f center around the same posteriors

post.f.rh <- post.lw.evp.senrh$BUGSoutput$sims.list$f.ev

# maximum a posteriori estimate
f.map.rh <- map_estimate(post.f.rh)[[1]]

# highest density interval, CI = 0.95
f.hdi025.rh <- hdi(post.f.rh, ci = 0.95)[[2]]
f.hdi975.rh <- hdi(post.f.rh, ci = 0.95)[[3]]

################################################
######## Test 3: sensitivity to k prior ########
################################################
# check the density of parameters
###### Fig S8 ######
denplot(as.mcmc(post.lw.evp.senk), 
        parms =c("TC", "rh.ev","dDi","d18Oi","k", 
                 "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", 
                 "dDL.ev","d18OL.ev","dDp","d18Op",
                 "f.ev","sl.ev", "intc.ev"))
# f is not sensitive to k. the posterior of k is about the same, regardless of the prior

post.f.k <- post.lw.evp.senk$BUGSoutput$sims.list$f.ev

# maximum a posteriori estimate
f.map.k <- map_estimate(post.f.k)[[1]]

# highest density interval, CI = 0.95
f.hdi025.k <- hdi(post.f.k, ci = 0.95)[[2]]
f.hdi975.k <- hdi(post.f.k, ci = 0.95)[[3]]

##########################################################
######## Test 4: sensitivity to d18O inflow prior ########
##########################################################

# check the density of parameters
###### Fig S9 ######
denplot(as.mcmc(post.lw.evp.Sen18Oi), 
        parms =c("TC", "rh.ev","dDi","d18Oi","k", 
                 "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", 
                 "dDL.ev","d18OL.ev","dDp","d18Op",
                 "f.ev","sl.ev", "intc.ev"))
# yes, x is sensitive to inflow water isotope values, but this is expected, 
# inflow isotope values essentially determines the evaporated lake water isotopes
# since inflow prior is well constrained, there is little concern about this sensitivity
# compile posterior distribution for the variables
# post.d18OA1.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18OA[,1]
# 
# post.dDA1.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDA[,1]
# 
# post.d18OA.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18OA.ev
# 
# post.dDA.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDA.ev
# 
# post.d18Ov.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18Ov.ev
# 
# post.dDv.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDv.ev
# 
# post.d18Oi.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18Oi
# 
# post.dDi.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDi
# 
# post.d18Op.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18Op
# 
# post.dDp.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDp
# 
# post.dstar18O.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dstar18O[,t]
# 
# post.dstarD.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dstarD[,t]
# 
# post.intc.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$intc.ev
# 
# post.sl.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$sl.ev

post.f.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$f.ev

# maximum a posteriori estimate
# sl.map.i <- map_estimate(post.sl)[[1]]
# 
# # highest density interval, CI = 0.95
# sl.hdi025.i <- hdi(post.sl, ci = 0.95)[[2]]
# sl.hdi975.i <- hdi(post.sl, ci = 0.95)[[3]]
# 
# # maximum a posteriori estimate
# intc.map.i <- map_estimate(post.intc)[[1]]
# 
# # highest density interval, CI = 0.95
# intc.hdi025.i <- hdi(post.intc, ci = 0.95)[[2]]
# intc.hdi975.i <- hdi(post.intc, ci = 0.95)[[3]]

# maximum a posteriori estimate
f.map.i <- map_estimate(post.f.i)[[1]]

# highest density interval, CI = 0.95
f.hdi025.i <- hdi(post.f.i, ci = 0.95)[[2]]
f.hdi975.i <- hdi(post.f.i, ci = 0.95)[[3]]

# cloud plot for simulated sources
# par(mfrow=c(1,1))
# plot(x = lw.d18O, y = lw.dD, xlab = "d18O", ylab = "dD",
#      xlim = c(-10,10), ylim = c(-80,60), 
#      col= alpha("cyan4", 0.5), pch = 16,
#      main = "Simulated water isotopes, Lake Turkana evaporation model")
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi.i, y = post.dDi, col= alpha("green2", 0.01))
# 
# points(x = post.d18Op.i, y = post.dDp, col= alpha("blue", 0.01))
# 
# # evaporated lake
# points(x = post.d18Ov.i, y = post.dDv, col= alpha("magenta2", 0.01))
# 
# points(x = post.d18OA1.i, y = post.dDA1, col= alpha("red", 0.01))
# 
# # after mixing with evaporated lake
# points(x = post.d18OA.i, y = post.dDA, col= alpha("red4", 0.01))
# 
# # there are a lot of uncertainties in the values of dstar
# # the values seem to depend on three things: 
# # the vapor, the input, and x, which can all be variable
# points(x = post.dstar18O.i, y = post.dstarD.i, col= alpha("orange", 0.01))
# 
# abline(a = intc.map.i, b = sl.map.i, lwd = 2, col = "orange4")
# # sl.map = 5.13 (95% CI: 4.47, 5.85)
# # intc.map = 10.32 (95% CI: 6.44, 13.65)
# 
# abline(a = intc.hdi025.i, b = sl.hdi975.i, lwd = 1.5, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975.i, b = sl.hdi025.i, lwd = 1.5, col = "orange4", lty = 2)
# 
# legend(-10, 60, c("Lake water", "Inflow", "Precipitation", 
#                   "Evap. lake water", "Limiting delta",
#                   "Precip. Eq. air", "Air-Vapor mixture"),
#        pch = c(16,16,16,16,16,16,16), 
#        col = c("cyan4", "green2", "blue", "magenta2", "orange",
#                "red", "red4"))

##########################################################
######## Test 5: sensitivity to d18O precip prior ########
##########################################################

# check the density of parameters
###### Fig S10 ######
denplot(as.mcmc(post.lw.evp.Sen18Op), 
        parms =c("TC", "rh.ev","dDi","d18Oi","k", 
                 "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", 
                 "dDL.ev","d18OL.ev","dDp","d18Op",
                 "f.ev","sl.ev", "intc.ev"))
# f is sensitive to precipitation priors as expected,
# which is tied to the atmospheric vapor isotopes that
# are tied to the mass balance calculation of f.ev

post.f.p <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$f.ev

# maximum a posteriori estimate
f.map.p <- map_estimate(post.f.p)[[1]]

# highest density interval, CI = 0.95
f.hdi025.p <- hdi(post.f.p, ci = 0.95)[[2]]
f.hdi975.p <- hdi(post.f.p, ci = 0.95)[[3]]

###################################################################
######## Test 6: sensitivity to more evaporated lake water ########
###################################################################

# check the density of parameters
###### Fig S11 ######
denplot(as.mcmc(post.lw.evp.Sen.evap), 
        parms =c("TC", "rh.ev","dDi","d18Oi","k", 
                 "dDA.ev","d18OA.ev", "dDv.ev","d18Ov.ev", 
                 "dDL.ev","d18OL.ev","dDp","d18Op",
                 "f.ev","sl.ev", "intc.ev"))
# yes, f is sensitive to more evaporated lake water, but this is expected

# compile posterior distribution for the variables
# post.d18OA1.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$d18OA[,1]
# 
# post.dDA1.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dDA[,1]
# 
# post.d18OA.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$d18OA.ev
# 
# post.dDA.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dDA.ev
# 
# post.d18Ov.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$d18Ov.ev
# 
# post.dDv.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dDv.ev
# 
# post.d18Oi.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$d18Oi
# 
# post.dDi.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dDi
# 
# post.d18Op.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$d18Op
# 
# post.dDp.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dDp
# 
# post.dstar18O.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dstar18O[,t]
# 
# post.dstarD.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$dstarD[,t]
# 
# post.intc.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$intc.ev
# 
# post.sl.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$sl.ev

post.f.evap <- post.lw.evp.Sen.evap$BUGSoutput$sims.list$f.ev

# maximum a posteriori estimate
# sl.map.evap <- map_estimate(post.sl.evap)[[1]]
# 
# # highest density interval, CI = 0.95
# sl.hdi025.evap <- hdi(post.sl.evap, ci = 0.95)[[2]]
# sl.hdi975.evap <- hdi(post.sl.evap, ci = 0.95)[[3]]
# 
# # maximum a posteriori estimate
# intc.map.evap <- map_estimate(post.intc.evap)[[1]]
# 
# # highest density interval, CI = 0.95
# intc.hdi025.evap <- hdi(post.intc.evap, ci = 0.95)[[2]]
# intc.hdi975.evap <- hdi(post.intc.evap, ci = 0.95)[[3]]

# maximum a posteriori estimate
f.map.evap <- map_estimate(post.f.evap)[[1]]

# highest density interval, CI = 0.95
f.hdi025.evap <- hdi(post.f.evap, ci = 0.95)[[2]]
f.hdi975.evap <- hdi(post.f.evap, ci = 0.95)[[3]]

# cloud plot for simulated sources
# par(mfrow=c(1,1))
# plot(x = lw.d18O.evap, y = lw.dD.evap, xlab = "d18O", ylab = "dD",
#      xlim = c(-10,10), ylim = c(-80,60), 
#      col= alpha("cyan4", 0.5), pch = 16,
#      main = "Simulated water isotopes, Lake Turkana evaporation model")
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi.evap, y = post.dDi.evap, col= alpha("green2", 0.01))
# 
# points(x = post.d18Op.evap, y = post.dDp.evap, col= alpha("blue", 0.01))
# 
# # evaporated lake
# points(x = post.d18Ov.evap, y = post.dDv.evap, col= alpha("magenta2", 0.01))
# 
# points(x = post.d18OA1.evap, y = post.dDA1.evap, col= alpha("red", 0.01))
# 
# # after mixing with evaporated lake
# points(x = post.d18OA.evap, y = post.dDA.evap, col= alpha("red4", 0.01))
# 
# # there are a lot of uncertainties in the values of dstar
# # the values seem to depend on three things: 
# # the vapor, the input, and x, which can all be variable
# points(x = post.dstar18O.evap, y = post.dstarD.evap, col= alpha("orange", 0.01))
# 
# abline(a = intc.map.evap, b = sl.map.evap, lwd = 2, col = "orange4")
# # sl.map = 5.13 (95% CI: 4.47, 5.85)
# # intc.map = 10.32 (95% CI: 6.44, 13.65)
# 
# abline(a = intc.hdi025.evap, b = sl.hdi975.evap, lwd = 1.5, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975.evap, b = sl.hdi025.evap, lwd = 1.5, col = "orange4", lty = 2)
# 
# legend(-10, 60, c("Lake water", "Inflow", "Precipitation", 
#                   "Evap. lake water", "Limiting delta",
#                   "Precip. Eq. air", "Air-Vapor mixture"),
#        pch = c(16,16,16,16,16,16,16), 
#        col = c("cyan4", "green2", "blue", "magenta2", "orange",
#                "red", "red4"))

###### Fig S5 ######
###### summary of posterior distributions of f #######
par(mfrow=c(2,3))
# density plot for the posterior of f
plot(density(post.f.TC), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-LST", xlab = "f.ev")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$f.ev), col = "blue")
abline(v = f.map.TC, lwd = 2)
abline(v = f.hdi025.TC, lwd = 1.5 , lty = 2)
abline(v = f.hdi975.TC, lwd = 1.5 , lty = 2)
legend(0.5,7,c("Model","Sens-param"),lty = c(1,1), col = c("blue", "red"))

# density plot for the posterior of f
plot(density(post.f.rh), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-rh", xlab = "f.ev")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$f.ev), col = "blue")
abline(v = f.map.rh, lwd = 2)
abline(v = f.hdi025.rh, lwd = 1.5 , lty = 2)
abline(v = f.hdi975.rh, lwd = 1.5 , lty = 2)

# density plot for the posterior of f
plot(density(post.f.k), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-k", xlab = "f.ev")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$f.ev), col = "blue")
abline(v = f.map.k, lwd = 2)
abline(v = f.hdi025.k, lwd = 1.5 , lty = 2)
abline(v = f.hdi975.k, lwd = 1.5 , lty = 2)

# density plot for the posterior of f
plot(density(post.f.i), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-inflow", xlab = "f.ev")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$f.ev), col = "blue")
abline(v = f.map.i, lwd = 2)
abline(v = f.hdi025.i, lwd = 1.5 , lty = 2)
abline(v = f.hdi975.i, lwd = 1.5 , lty = 2)

# density plot for the posterior of f
plot(density(post.f.p), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-precip", xlab = "f.ev")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$f.ev), col = "blue")
abline(v = f.map.p, lwd = 2)
abline(v = f.hdi025.p, lwd = 1.5 , lty = 2)
abline(v = f.hdi975.p, lwd = 1.5 , lty = 2)

# density plot for the posterior of f
plot(density(post.f.evap), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-lake", xlab = "f.ev")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$f.ev), col = "blue")
abline(v = f.map.evap, lwd = 2)
abline(v = f.hdi025.evap, lwd = 1.5 , lty = 2)
abline(v = f.hdi975.evap, lwd = 1.5 , lty = 2)