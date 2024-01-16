library(scales)
library(viridisLite)
library(ggplot2)
library(mcmcplots)
library(bayesplot)

# check the density of parameters
denplot(as.mcmc(post.lw.evp), c("intc", "sl", "k", "x"))

# bivariate density plots
mcmc_hex(as.mcmc(post.lw.evp), pars = c("k", "rh"))

mcmc_hex(as.mcmc(post.lw.evp), pars = c("k", "x"))

mcmc_hex(as.mcmc(post.lw.evp), pars = c("rh", "x"))

mcmc_hex(as.mcmc(post.lw.evp), pars = c("d18Ov", "dDv"))

mcmc_hex(as.mcmc(post.lw.evp), pars = c("d18Oi", "dDi"))

mcmc_hex(as.mcmc(post.lw.evp), pars = c("d18Op", "dDp"))

mcmc_hex(as.mcmc(post.lw.evp), pars = c("sl", "intc"))

post.d18Ov <- post.lw.evp$BUGSoutput$sims.list$d18Ov

post.dDv <- post.lw.evp$BUGSoutput$sims.list$dDv

post.d18Oi <- post.lw.evp$BUGSoutput$sims.list$d18Oi

post.dDi <- post.lw.evp$BUGSoutput$sims.list$dDi

post.d18Op <- post.lw.evp$BUGSoutput$sims.list$d18Op

post.dDp <- post.lw.evp$BUGSoutput$sims.list$dDp

post.dstar18O <- post.lw.evp$BUGSoutput$sims.list$dstar18O

post.dstarD <- post.lw.evp$BUGSoutput$sims.list$dstarD

post.intc <- post.lw.evp$BUGSoutput$sims.list$intc

post.sl <- post.lw.evp$BUGSoutput$sims.list$sl

post.x <- post.lw.evp$BUGSoutput$sims.list$x

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

plot(x = lw.d18O, y = lw.dD, 
     xlim = c(-30,20), ylim = c(-150,100), 
     col= alpha("cyan4", 0.5), pch = 16)

# GMWL
abline(a = 10, b = 8, lwd = 2)

# modeled input water
points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))

# modeled precipitation
points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))

# modeled vapor (gas phase end member)
points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

# modeled fully evaporated lake water (liquid phase end member)
points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))

# MAP estimate of evaporation line and 95% HDI
abline(a = intc.map, b = sl.map, lwd = 2, col = "orange4")

abline(a = intc.hdi025, b = sl.hdi975, lwd = 2, col = "orange4", lty = 2)

abline(a = intc.hdi975, b = sl.hdi025, lwd = 2, col = "orange4", lty = 2)

plot(density(post.x))
abline(v = x.map, lwd = 2)
abline(v = x.hdi025, lwd = 1.5 , lty = 2)
abline(v = x.hdi975, lwd = 1.5 , lty = 2)
