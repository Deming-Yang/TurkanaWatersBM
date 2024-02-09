library(scales)
library(viridisLite)
library(ggplot2)
library(mcmcplots)
library(bayestestR)
library(bayesplot)

# compile posterior distribution for the variables
post.d18OA1 <- post.lw.evp.f$BUGSoutput$sims.list$d18OA[,1]

post.dDA1 <- post.lw.evp.f$BUGSoutput$sims.list$dDA[,1]

post.d18OA <- post.lw.evp.f$BUGSoutput$sims.list$d18OA.ev

post.dDA <- post.lw.evp.f$BUGSoutput$sims.list$dDA.ev

post.d18Ov <- post.lw.evp.f$BUGSoutput$sims.list$d18Ov.ev

post.dDv <- post.lw.evp.f$BUGSoutput$sims.list$dDv.ev

post.d18Oi <- post.lw.evp.f$BUGSoutput$sims.list$d18Oi

post.dDi <- post.lw.evp.f$BUGSoutput$sims.list$dDi

post.d18Op <- post.lw.evp.f$BUGSoutput$sims.list$d18Op

post.dDp <- post.lw.evp.f$BUGSoutput$sims.list$dDp

post.dstar18O <- post.lw.evp.f$BUGSoutput$sims.list$dstar18O[,t]

post.dstarD <- post.lw.evp.f$BUGSoutput$sims.list$dstarD[,t]

post.intc <- post.lw.evp.f$BUGSoutput$sims.list$intc.ev

post.sl <- post.lw.evp.f$BUGSoutput$sims.list$sl.ev

post.f <- post.lw.evp.f$BUGSoutput$sims.list$f.ev


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
f.map <- map_estimate(post.f)[[1]]

# highest density interval, CI = 0.95
f.hdi025 <- hdi(post.f, ci = 0.95)[[2]]
f.hdi975 <- hdi(post.f, ci = 0.95)[[3]]

# cloud plot for simulated sources 
plot(x = lw.d18O, y = lw.dD, xlab = "d18O", ylab = "dD",
     xlim = c(-10,10), ylim = c(-80,60), 
     col= alpha("cyan4", 0.5), pch = 16,
     main = "Simulated water isotopes, Lake Turkana evaporation model")
abline(a = 10, b = 8, lwd = 2)

points(x = post.d18Oi, y = post.dDi, col= alpha("green2", 0.01))

points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))

# evaporated lake
points(x = post.d18Ov, y = post.dDv, col= alpha("magenta2", 0.01))

points(x = post.d18OA1, y = post.dDA1, col= alpha("red", 0.01))

# after mixing with evaporated lake
points(x = post.d18OA, y = post.dDA, col= alpha("red4", 0.01))

# there are a lot of uncertainties in the values of dstar
# the values seem to depend on three things: 
# the vapor, the input, and x, which can all be variable
points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))

abline(a = intc.map, b = sl.map, lwd = 2, col = "orange4")
# sl.map = 5.13 (95% CI: 4.47, 5.85)
# intc.map = 10.32 (95% CI: 6.44, 13.65)

abline(a = intc.hdi025, b = sl.hdi975, lwd = 1.5, col = "orange4", lty = 2)

abline(a = intc.hdi975, b = sl.hdi025, lwd = 1.5, col = "orange4", lty = 2)

legend(-10, 60, c("Lake water", "Inflow", "Precipitation", 
                  "Evap. lake water", "Limiting delta",
                  "Precip. Eq. air", "Air-Vapor mixture"),
       pch = c(16,16,16,16,16,16,16), 
       col = c("cyan4", "green2", "blue", "magenta2", "orange",
                                        "red", "red4"))

# bivariate density plots

mcmc_hex(as.mcmc(post.lw.evp.mod), pars = c("f.ev", "rh.ev"))

# mcmc_hex(as.mcmc(post.lw.evp.mod), pars = c("f.ev", "k"))
# 
# mcmc_hex(as.mcmc(post.lw.evp.mod), pars = c("k", "rh.ev"))


# density plot for the posterior of f
plot(density(post.f), xlim = c(0, 1), xlab = "f", ylab = "Density",
     main = "Posterior distribution of f.ev")
abline(v = f.map, lwd = 2)
abline(v = f.hdi025, lwd = 1.5 , lty = 2)
abline(v = f.hdi975, lwd = 1.5 , lty = 2)

# plot relative humidity from before mixing to after mixing

plot(density(post.rh.int), xlim = c(0.2, 1), xlab = "rh", ylab = "Density",
     main = "Posterior distribution of rh", col = "blue")
lines(density(post.rhev), col = "red")
