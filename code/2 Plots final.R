library(scales)
library(viridisLite)
library(ggplot2)
library(mcmcplots)
library(bayestestR)
library(bayesplot)

# compile posterior distribution for the variables
post.d18Ov <- post.lw.evp.f$BUGSoutput$sims.list$d18Ov

post.dDv <- post.lw.evp.f$BUGSoutput$sims.list$dDv

post.d18Oi <- post.lw.evp.f$BUGSoutput$sims.list$d18Oi

post.dDi <- post.lw.evp.f$BUGSoutput$sims.list$dDi

post.d18Op <- post.lw.evp.f$BUGSoutput$sims.list$d18Op

post.dDp <- post.lw.evp.f$BUGSoutput$sims.list$dDp

post.dstar18O <- post.lw.evp.f$BUGSoutput$sims.list$dstar18O

post.dstarD <- post.lw.evp.f$BUGSoutput$sims.list$dstarD

post.intc <- post.lw.evp.f$BUGSoutput$sims.list$intc

post.sl <- post.lw.evp.f$BUGSoutput$sims.list$sl

post.x <- post.lw.evp.f$BUGSoutput$sims.list$x


# maximum a posteriori estimate
sl.map <- map_estimate(post.sl)[[1]]

# highest density interval, CI = 0.95
sl.hdi025 <- hdi(post.sl, ci = 0.95)[[2]]
sl.hdi975 <- hdi(post.sl, ci = 0.95)[[3]]

# maximum a posteriori estimate
intc.map <- map_estimate(post.intc)[[1]]

# highest density interval, CI = 0.95
intc.hdi025 <- hdi(post.intc, ci = 0.95)[[2]]
intc.hdi975 <- hdi(post.intc, ci = 0.95)[[3]]

# maximum a posteriori estimate
x.map <- map_estimate(post.x)[[1]]

# highest density interval, CI = 0.95
x.hdi025 <- hdi(post.x, ci = 0.95)[[2]]
x.hdi975 <- hdi(post.x, ci = 0.95)[[3]]

# cloud plot for simulated sources 
plot(x = lw.d18O, y = lw.dD, xlab = "d18O", ylab = "dD",
     xlim = c(-20,30), ylim = c(-100,150), 
     col= alpha("cyan4", 0.5), pch = 16,
     main = "Simulated water isotopes, Lake Turkana evaporation model")
abline(a = 10, b = 8, lwd = 2)

points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))

points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))

points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# there are a lot of uncertainties in the values of dstar
# the values seem to depend on three things: 
# the vapor, the input, and x, which can all be variable
points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))

abline(a = intc.map, b = sl.map, lwd = 2, col = "orange4")
# sl.map = 5.13 (95% CI: 4.47, 5.85)
# intc.map = 10.32 (95% CI: 6.44, 13.65)

abline(a = intc.hdi025, b = sl.hdi975, lwd = 1.5, col = "orange4", lty = 2)

abline(a = intc.hdi975, b = sl.hdi025, lwd = 1.5, col = "orange4", lty = 2)

legend(-20, 50, c("Lake water", "Inflow", "Precipitation", "Vapor", "Evp Lake"),
       pch = c(16,1,1,1,1), col = c("cyan4", "green4", "blue", "magenta4", "orange"))

# bivariate density plots

mcmc_hex(as.mcmc(post.lw.evp.f), pars = c("x", "rh"))

mcmc_hex(as.mcmc(post.lw.evp.f), pars = c("x", "k"))

mcmc_hex(as.mcmc(post.lw.evp.f), pars = c("k", "rh"))


# density plot for the posterior of x
plot(density(post.x), xlim = c(0.1, 0.8), xlab = "x", ylab = "Density",
     main = "Posterior distribution of x")
abline(v = x.map, lwd = 2)
abline(v = x.hdi025, lwd = 1.5 , lty = 2)
abline(v = x.hdi975, lwd = 1.5 , lty = 2)
