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
denplot(as.mcmc(post.lw.evp.senTC))
# x is not sensitive to TC prior

# bivariate density plots
# mcmc_hex(as.mcmc(post.lw.evp.senTC), pars = c("TC", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.senTC), pars = c("k", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.senTC), pars = c("rh", "x"))

# post.d18Ov <- post.lw.evp.senTC$BUGSoutput$sims.list$d18Ov
# 
# post.dDv <- post.lw.evp.senTC$BUGSoutput$sims.list$dDv
# 
# post.d18Oi <- post.lw.evp.senTC$BUGSoutput$sims.list$d18Oi
# 
# post.dDi <- post.lw.evp.senTC$BUGSoutput$sims.list$dDi
# 
# post.d18Op <- post.lw.evp.senTC$BUGSoutput$sims.list$d18Op
# 
# post.dDp <- post.lw.evp.senTC$BUGSoutput$sims.list$dDp
# 
# post.dstar18O <- post.lw.evp.senTC$BUGSoutput$sims.list$dstar18O
# 
# post.dstarD <- post.lw.evp.senTC$BUGSoutput$sims.list$dstarD
# 
# post.intc <- post.lw.evp.senTC$BUGSoutput$sims.list$intc
# 
# post.sl <- post.lw.evp.senTC$BUGSoutput$sims.list$sl

post.x.TC <- post.lw.evp.senTC$BUGSoutput$sims.list$x

# maximum a posteriori estimate
x.map.TC <- map_estimate(post.x.TC)[[1]]

# highest density interval, CI = 0.95
x.hdi025.TC <- hdi(post.x.TC, ci = 0.95)[[2]]
x.hdi975.TC <- hdi(post.x.TC, ci = 0.95)[[3]]

# cloud plot for simulated sources 
# plot(x = lw.d18O, y = lw.dD, 
#      xlim = c(-30,30), ylim = c(-150,200), 
#      col= alpha("cyan4", 0.5), pch = 16)
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))
# 
# points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))
# 
# points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))
# 
# abline(a = intc.map, b = sl.map, lwd = 1.5, col = "orange4")
# 
# abline(a = intc.hdi025, b = sl.hdi975, lwd = 1, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975, b = sl.hdi025, lwd = 1, col = "orange4", lty = 2)

#################################################
######## Test 2: sensitivity to rh prior ########
#################################################
# check the density of parameters
denplot(as.mcmc(post.lw.evp.senrh))
#yes, x is sensitive to rh estimate, although both rh and x center around the posteriors

# bivariate density plots
# mcmc_hex(as.mcmc(post.lw.evp.senrh), pars = c("TC", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.senrh), pars = c("k", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.senrh), pars = c("rh", "x"))

# post.d18Ov <- post.lw.evp.senrh$BUGSoutput$sims.list$d18Ov
# 
# post.dDv <- post.lw.evp.senrh$BUGSoutput$sims.list$dDv
# 
# post.d18Oi <- post.lw.evp.senrh$BUGSoutput$sims.list$d18Oi
# 
# post.dDi <- post.lw.evp.senrh$BUGSoutput$sims.list$dDi
# 
# post.d18Op <- post.lw.evp.senrh$BUGSoutput$sims.list$d18Op
# 
# post.dDp <- post.lw.evp.senrh$BUGSoutput$sims.list$dDp
# 
# post.dstar18O <- post.lw.evp.senrh$BUGSoutput$sims.list$dstar18O
# 
# post.dstarD <- post.lw.evp.senrh$BUGSoutput$sims.list$dstarD
# 
# post.intc <- post.lw.evp.senrh$BUGSoutput$sims.list$intc
# 
# post.sl <- post.lw.evp.senrh$BUGSoutput$sims.list$sl

post.x.rh <- post.lw.evp.senrh$BUGSoutput$sims.list$x

# maximum a posteriori estimate
x.map.rh <- map_estimate(post.x.rh)[[1]]

# highest density interval, CI = 0.95
x.hdi025.rh <- hdi(post.x.rh, ci = 0.95)[[2]]
x.hdi975.rh <- hdi(post.x.rh, ci = 0.95)[[3]]

# cloud plot for simulated sources 
# plot(x = lw.d18O, y = lw.dD, 
#      xlim = c(-30,30), ylim = c(-150,200), 
#      col= alpha("cyan4", 0.5), pch = 16)
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))
# 
# points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))
# 
# points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))
# 
# abline(a = intc.map, b = sl.map, lwd = 1.5, col = "orange4")
# 
# abline(a = intc.hdi025, b = sl.hdi975, lwd = 1, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975, b = sl.hdi025, lwd = 1, col = "orange4", lty = 2)

################################################
######## Test 3: sensitivity to k prior ########
################################################
# check the density of parameters
denplot(as.mcmc(post.lw.evp.senk))
# yes, x is sensitive to k being skewed towards low values, but k = 0.5 means highly seasonal situations 

# bivariate density plots
# mcmc_hex(as.mcmc(post.lw.evp.senk), pars = c("TC", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.senk), pars = c("k", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.senk), pars = c("rh", "x"))

# post.d18Ov <- post.lw.evp.senk$BUGSoutput$sims.list$d18Ov
# 
# post.dDv <- post.lw.evp.senk$BUGSoutput$sims.list$dDv
# 
# post.d18Oi <- post.lw.evp.senk$BUGSoutput$sims.list$d18Oi
# 
# post.dDi <- post.lw.evp.senk$BUGSoutput$sims.list$dDi
# 
# post.d18Op <- post.lw.evp.senk$BUGSoutput$sims.list$d18Op
# 
# post.dDp <- post.lw.evp.senk$BUGSoutput$sims.list$dDp
# 
# post.dstar18O <- post.lw.evp.senk$BUGSoutput$sims.list$dstar18O
# 
# post.dstarD <- post.lw.evp.senk$BUGSoutput$sims.list$dstarD
# 
# post.intc <- post.lw.evp.senk$BUGSoutput$sims.list$intc
# 
# post.sl <- post.lw.evp.senk$BUGSoutput$sims.list$sl

post.x.k <- post.lw.evp.senk$BUGSoutput$sims.list$x

# maximum a posteriori estimate
x.map.k <- map_estimate(post.x.k)[[1]]

# highest density interval, CI = 0.95
x.hdi025.k <- hdi(post.x.k, ci = 0.95)[[2]]
x.hdi975.k <- hdi(post.x.k, ci = 0.95)[[3]]

# cloud plot for simulated sources 
# plot(x = lw.d18O, y = lw.dD, 
#      xlim = c(-30,30), ylim = c(-150,200), 
#      col= alpha("cyan4", 0.5), pch = 16)
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))
# 
# points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))
# 
# points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))
# 
# abline(a = intc.map, b = sl.map, lwd = 1.5, col = "orange4")
# 
# abline(a = intc.hdi025, b = sl.hdi975, lwd = 1, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975, b = sl.hdi025, lwd = 1, col = "orange4", lty = 2)

##########################################################
######## Test 4: sensitivity to d18O inflow prior ########
##########################################################

# check the density of parameters
denplot(as.mcmc(post.lw.evp.Sen18Oi))
# yes, x is sensitive to inflow water isotope values, but this is expected, and inflow prior is well constrained

# bivariate density plots
# mcmc_hex(as.mcmc(post.lw.evp.Sen18Oi), pars = c("TC", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.Sen18Oi), pars = c("k", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.Sen18Oi), pars = c("rh", "x"))

# post.d18Ov <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18Ov
# 
# post.dDv <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDv
# 
# post.d18Oi <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18Oi
# 
# post.dDi <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDi
# 
# post.d18Op <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$d18Op
# 
# post.dDp <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dDp
# 
# post.dstar18O <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dstar18O
# 
# post.dstarD <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$dstarD
# 
# post.intc <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$intc
# 
# post.sl <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$sl

post.x.i <- post.lw.evp.Sen18Oi$BUGSoutput$sims.list$x

# maximum a posteriori estimate
x.map.i <- map_estimate(post.x.i)[[1]]

# highest density interval, CI = 0.95
x.hdi025.i <- hdi(post.x.i, ci = 0.95)[[2]]
x.hdi975.i <- hdi(post.x.i, ci = 0.95)[[3]]

# cloud plot for simulated sources 
# plot(x = lw.d18O, y = lw.dD, 
#      xlim = c(-30,30), ylim = c(-150,200), 
#      col= alpha("cyan4", 0.5), pch = 16)
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))
# 
# points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))
# 
# points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))
# 
# abline(a = intc.map, b = sl.map, lwd = 1.5, col = "orange4")
# 
# abline(a = intc.hdi025, b = sl.hdi975, lwd = 1, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975, b = sl.hdi025, lwd = 1, col = "orange4", lty = 2)

##########################################################
######## Test 5: sensitivity to d18O precip prior ########
##########################################################

# check the density of parameters
denplot(as.mcmc(post.lw.evp.Sen18Op))
# x is not sensitive to precipitation priors, due to the compensation from rh

# mcmc_hex(as.mcmc(post.lw.evp.Sen18Op), pars = c("TC", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.Sen18Op), pars = c("k", "x"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.Sen18Op), pars = c("rh", "x"))

# post.d18Ov <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$d18Ov
# 
# post.dDv <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$dDv
# 
# post.d18Oi <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$d18Oi
# 
# post.dDi <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$dDi
# 
# post.d18Op <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$d18Op
# 
# post.dDp <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$dDp
# 
# post.dstar18O <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$dstar18O
# 
# post.dstarD <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$dstarD
# 
# post.intc <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$intc
# 
# post.sl <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$sl

post.x.p <- post.lw.evp.Sen18Op$BUGSoutput$sims.list$x

# maximum a posteriori estimate
x.map.p <- map_estimate(post.x.p)[[1]]

# highest density interval, CI = 0.95
x.hdi025.p <- hdi(post.x.p, ci = 0.95)[[2]]
x.hdi975.p <- hdi(post.x.p, ci = 0.95)[[3]]

# cloud plot for simulated sources 
# plot(x = lw.d18O, y = lw.dD, 
#      xlim = c(-30,30), ylim = c(-150,200), 
#      col= alpha("cyan4", 0.5), pch = 16)
# abline(a = 10, b = 8, lwd = 2)
# 
# points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))
# 
# points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))
# 
# points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))
# 
# abline(a = intc.map, b = sl.map, lwd = 1.5, col = "orange4")
# 
# abline(a = intc.hdi025, b = sl.hdi975, lwd = 1, col = "orange4", lty = 2)
# 
# abline(a = intc.hdi975, b = sl.hdi025, lwd = 1, col = "orange4", lty = 2)

###### summary of posterior distributions of x #######
par(mfrow=c(2,3))
# density plot for the posterior of x
plot(density(post.x.TC), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-LST", xlab = "x")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$x), col = "blue")
abline(v = x.map.TC, lwd = 2)
abline(v = x.hdi025.TC, lwd = 1.5 , lty = 2)
abline(v = x.hdi975.TC, lwd = 1.5 , lty = 2)
legend(0.5,7,c("Model","Sens-param"),lty = c(1,1), col = c("blue", "red"))

# density plot for the posterior of x
plot(density(post.x.rh), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-rh", xlab = "x")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$x), col = "blue")
abline(v = x.map.rh, lwd = 2)
abline(v = x.hdi025.rh, lwd = 1.5 , lty = 2)
abline(v = x.hdi975.rh, lwd = 1.5 , lty = 2)

# density plot for the posterior of x
plot(density(post.x.k), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-k", xlab = "x")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$x), col = "blue")
abline(v = x.map.k, lwd = 2)
abline(v = x.hdi025.k, lwd = 1.5 , lty = 2)
abline(v = x.hdi975.k, lwd = 1.5 , lty = 2)

# density plot for the posterior of x
plot(density(post.x.i), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-inflow", xlab = "x")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$x), col = "blue")
abline(v = x.map.i, lwd = 2)
abline(v = x.hdi025.i, lwd = 1.5 , lty = 2)
abline(v = x.hdi975.i, lwd = 1.5 , lty = 2)

# density plot for the posterior of x
plot(density(post.x.p), xlim = c(0, 1), ylim = c(0,7), col = "red",
     main = "Posterior Sens-precip", xlab = "x")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$x), col = "blue")
abline(v = x.map.p, lwd = 2)
abline(v = x.hdi025.p, lwd = 1.5 , lty = 2)
abline(v = x.hdi975.p, lwd = 1.5 , lty = 2)