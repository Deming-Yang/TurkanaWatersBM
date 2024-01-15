library(scales)
library(viridisLite)
library(ggplot2)
library(mcmcplots)


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

plot(x = lw.d18O, y = lw.dD, 
     xlim = c(-40,30), ylim = c(-200,150), 
     col= alpha("cyan4", 0.5), pch = 16)
abline(a = 10, b = 8, lwd = 2)
index <- sample(15000, 1000, replace = F)
for(i in 1:length(index)){
  abline(a = c(post.intc)[index[i]], b = c(post.sl)[index[i]], col= alpha("black", 0.01))
}

points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))

points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))

points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))

plot(x = lw.d18O, y = lw.dD, 
     xlim = c(-40,30), ylim = c(-200,150), 
     col= alpha("cyan4", 0.5), pch = 16)
abline(a = c(post.intc)[1:100], b = c(post.sl)[1:100], col= alpha("black", 0.001))




# check the density of parameters
denplot(as.mcmc(post.lw.evp.b), c("intc", "sl", "k", "x"))

# bivariate density plots
mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("k", "rh"))

mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("k", "x"))

mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("rh", "x"))

mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("d18Ov", "dDv"))

mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("d18Oi", "dDi"))

mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("d18Op", "dDp"))

mcmc_hex(as.mcmc(post.lw.evp.b), pars = c("sl", "intc"))

post.d18Ov <- post.lw.evp.b$BUGSoutput$sims.list$d18Ov

post.dDv <- post.lw.evp.b$BUGSoutput$sims.list$dDv

post.d18Oi <- post.lw.evp.b$BUGSoutput$sims.list$d18Oi

post.dDi <- post.lw.evp.b$BUGSoutput$sims.list$dDi

post.d18Op <- post.lw.evp.b$BUGSoutput$sims.list$d18Op

post.dDp <- post.lw.evp.b$BUGSoutput$sims.list$dDp

post.dstar18O <- post.lw.evp.b$BUGSoutput$sims.list$dstar18O

post.dstarD <- post.lw.evp.b$BUGSoutput$sims.list$dstarD

post.intc <- post.lw.evp.b$BUGSoutput$sims.list$intc

post.sl <- post.lw.evp.b$BUGSoutput$sims.list$sl

plot(x = lw.d18O, y = lw.dD, 
     xlim = c(-40,30), ylim = c(-200,150), 
     col= alpha("cyan4", 0.5), pch = 16)
abline(a = 10, b = 8, lwd = 2)
index <- sample(15000, 1000, replace = F)
for(i in 1:length(index)){
  abline(a = c(post.intc)[index[i]], b = c(post.sl)[index[i]], col= alpha("black", 0.01))
}

points(x = post.d18Oi, y = post.dDi, col= alpha("green4", 0.01))

points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))

points(x = post.d18Ov, y = post.dDv, col= alpha("magenta4", 0.01))

points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))

plot(x = lw.d18O, y = lw.dD, 
     xlim = c(-40,30), ylim = c(-200,150), 
     col= alpha("cyan4", 0.5), pch = 16)
abline(a = c(post.intc)[1:100], b = c(post.sl)[1:100], col= alpha("black", 0.001))

qnbinom(0.95, size = 1, prob = 1e-3)

qnbinom(0.8, size = 1, prob = 1e-3)
