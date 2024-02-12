library(MASS)
library(viridisLite)

post.rh.int <- post.lw.evp.f$BUGSoutput$sims.list$f.int
post.rhev <- post.lw.evp.f$BUGSoutput$sims.list$f.ev

# parameter Lake Surface temperature or TC
par(mfrow=c(2,4))
x<-seq(from=24,to=32,length.out=1000)
plot(x, dnorm(x, mean.TC, sd.TC), type = "l", col = "blue",
     main="Prior vs Posterior, LST", xlab = "LST", ylab = "Density")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$TC), col = "red")

# parameter relative humidity, rh
x<-seq(from=0.2,to=1,length.out=1000)
plot(x, dbeta(x, 16, 12), type = "l", col = "blue", ylim = c(0, 13),
     main="Prior vs Posterior, rh.int", xlab = "rh", ylab = "Density")
lines(density(post.rh.int), col = "red")
legend(0.2,12,c("Prior","Posterior"),lty = c(1,1), col = c("blue", "red"))

# plot relative humidity from before mixing to after mixing
plot(density(post.rh.int), xlim = c(0.6, 0.95), xlab = "rh", ylab = "Density",
     main = "Posteriors of rh.int and rh.ev", col = "cyan4")
lines(density(post.rhev), col = "blue4")
legend(0.6,12,c("rh.int","rh.ev"),lty = c(1,1), col = c("cyan4", "blue4"))

# parameter evaporative seasonality, k
x<-seq(from=0.7,to=1,length.out=1000)
plot(x, dbeta(x, 20, 1), type = "l", col = "blue", ylim = c(0, 30),
     main="Prior vs Posterior, k", xlab = "k", ylab = "Density")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$k), col = "red")

# parameter inflow d18O, d18Oi
x<-seq(from=-5,to=2,length.out=1000)
plot(x, dnorm(x, mean.d18Oi, sd.d18Oi), type = "l", col = "blue", ylim = c(0, 0.5),
     xlim=c(-5,2),main="Prior vs Posterior, d18Oi", xlab = "d18Oi", ylab = "Density")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$d18Oi), col = "red")

# parameter precipitation d18O, d18Op
x<-seq(from=-2,to=4,length.out=1000)
plot(x, dnorm(x, mean.d18Op, sd.d18Op), type = "l", col = "blue", ylim = c(0, 0.7),
     main="Prior vs Posterior, d18Op", xlab = "d18Op", ylab = "Density")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$d18Op), col = "red")

# parameter precipitation d18O, d18Op
x<-seq(from=-5,to=45,length.out=1000)
plot(x, dnorm(x, mean.dDp, sd.dDp), type = "l", col = "blue", ylim = c(0, 0.12),
     main="Prior vs Posterior, dDp", xlab = "dDp", ylab = "Density")
lines(density(post.lw.evp.f$BUGSoutput$sims.list$dDp), col = "red")
