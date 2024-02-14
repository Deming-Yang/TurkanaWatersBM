library(scales)
library(viridisLite)
library(ggplot2)
library(mcmcplots)
library(bayestestR)
library(bayesplot)

# Values for plots----

# posterior distributions for the variables
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
post.rh.int <- post.lw.evp.f$BUGSoutput$sims.list$rh[,1]
post.rhev <- post.lw.evp.f$BUGSoutput$sims.list$rh.ev
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

# Plot style----
theme_set(theme(
  line = element_line(color = "gray90", linewidth = 0.5),
  rect = element_rect(color = "gray10", fill = "white", linewidth = 0.5),
  text = element_text(family = "Helvetica", size = 8, face = "plain"),
  axis.ticks = element_blank(),
  legend.background = element_rect(color = "gray10", fill = "white"),
  legend.key = element_blank(),
  legend.title = element_text(face = "bold"),
  legend.title.align = 0, 
  panel.background = element_rect(color = "gray10", fill = "white"),
  panel.grid = element_line(color = "gray90")))

color.lake <- "#228833"
color.precip <- "#66CCEE"
color.river <- "#4477AA"
color.evap.lw <- "#44BB99"
color.a1 <- "#BB2255"
color.mix <- "#AA4499"
color.dstar <- "#BBCC33"

color.GMWL <- "gray19"
color.model <- "gray70"

shape.lake <- 16 # circle
shape.delta <-  2 # triangle empty
shape.precip <- 17 # triangle
shape.river <- 15 # square
shape.ground <- 7 # x in box
shape.tap <- 9 # x in diamond
shape.surface <- 0 # square empty
shape.surface.lake <- 1 # circle empty
shape.model <- 3 # cross 
shape.cloud <- "." # single pixel for point cloud

size.sm <- 1
size.md <- 2
size.lg <- 3

label.d18O <- expression(paste(delta^{18}, "O (‰ VSMOW)"))
label.dD <- expression(paste(delta, "D (‰ VSMOW)"))

# Figure 5----

fig.5 <- ggplot() + 
  geom_point(data = NULL, aes(x = post.d18Oi, y = post.dDi), color = color.river, shape = ".", alpha = 0.25) +
  geom_abline(data = NULL, aes(slope = 8, intercept = 10, color = "GMWL")) +
  geom_abline(data = NULL, aes(slope = sl.map, intercept = intc.map, color = "LEL")) +
  geom_point(data = waters |>
               filter(WaterType %in% c("Lake","Delta", "River")),
             aes(x = d18O, y = dD, color = WaterType, shape = WaterType), size = size.lg) +
  scale_shape_manual(
    breaks = c("Lake", "Delta", "River", "GMWL", "LEL"),
    values = c(
      "Lake" = shape.lake,
      "Delta" = shape.delta,
      "River" = shape.river,
      "GMWL" = 1,
      "LEL" = 1),
    guide = "none",
    name = "Legend") +
  scale_color_manual(
    breaks = c("Lake", "Delta", "River", "GMWL", "LEL"),
    values = c(
      "Lake" = color.lake,
      "Delta" = color.lake,
      "River" = color.river,
      "GMWL" = color.GMWL,
      "LEL" = color.model),
    name = "Legend")  +
  guides(
    color = guide_legend(
      override.aes=list(
        linetype = c("blank", "blank", "blank", "solid", "solid"),
        shape = c(shape.lake, shape.delta, shape.river, NA, NA),
        size = size.md))) +
  labs(x = label.d18O, 
       y = label.dD) +
  theme(text = element_text(family = "Helvetica", size = 10, face = "plain"),
        legend.position=c(vjust = 0.9, hjust = 0.15),
        legend.key.height = unit(0, "cm"),
        legend.key.width = unit(0, "cm"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title=element_blank())
# plot(fig.5)
# ggsave(here("Figure5.png"), fig.5, device = png, width = 8, height = 5.6, units = "in")

# Figure 6----

color.prior <- "#2166AC"
color.post <- "#B2182B"
color.rh.int <- "#117733"
color.rh.ev <- color.mix

fig.6 <- ggplot(data = NULL) +
  scale_color_manual(breaks = c("Prior", "Posterior"),
                     values = c(
                       "Prior" = color.prior,
                       "Posterior" = color.post)) +
  theme(plot.title = element_text(vjust = -9, hjust = 0.03)) +
  ylab(NULL)

# LST
x<-seq(from=24,to=32, length.out=1000)
fig.6a <- fig.6 +
  geom_line(aes(x = x, y = dnorm(x, mean.TC, sd.TC), color = "Prior"), show.legend = FALSE) +
  geom_density(aes(x = post.lw.evp.f$BUGSoutput$sims.list$TC, color = "Posterior"), show.legend = FALSE) +
  lims(x = c(24, 32), y = c(0, 0.48)) +
  xlab("Lake Surface Temperature (°C)") +
  ylab("Density") + 
  ggtitle("a) LST") 
plot(fig.6a)

# rh.int
x<-seq(from=0.2,to=1,length.out=1000)
fig.6b <- fig.6 +
  geom_line(aes(x = x, y = dbeta(x, 16, 12), color = "Prior"), show.legend = FALSE) +
  geom_density(aes(x = post.rh.int, color = "Posterior"), show.legend = FALSE) +
  lims(x = c(0.2, 1), y = c(0, 13)) +
  xlab("Relative humidity") +
  ggtitle("b) Initial rh")
plot(fig.6b)

# rh mixing
fig.6c <- ggplot(data = NULL) +
  geom_abline(aes(slope = 0, intercept = 0, color = "Prior"), linewidth = 0, show.legend = TRUE) +
  geom_abline(aes(slope = 0, intercept = 0, color = "Posterior"), linewidth = 0, show.legend = TRUE) +
  geom_density(aes(x = post.rh.int, color = "rh.int"), show.legend = FALSE) +
  geom_density(aes(x = post.rhev, color = "rh.ev"), show.legend = FALSE) +
  scale_color_manual(breaks = c("Prior", "Posterior", "rh.int", "rh.ev"),
                     values = c(
                       "Prior" = color.prior,
                       "Posterior" = color.post,
                       "rh.int" = color.rh.int,
                       "rh.ev" = color.rh.ev)) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c(rep("solid", 4)),
        shape = c(rep(NA, 4)),
        linewidth = 0.5))) +
  theme(    
    plot.title = element_text(vjust = -9, hjust = 0.03),
    legend.position= c(vjust = 0.3, hjust = 0.55),
    legend.title = element_blank()) +
  lims(x = c(0.4, 1), y = c(0, 15)) +
  xlab("Relative humidity") +
  ylab(NULL) +
  ggtitle("c) Initial rh vs. after mixing")
plot(fig.6c)

# k
xk<-seq(from=0.7,to=1,length.out=1000)
fig.6d <- fig.6 +
  geom_line(aes(x = xk, y = dbeta(xk, 20, 1), color = "Prior"), show.legend = FALSE) +
  geom_density(aes(x = post.lw.evp.f$BUGSoutput$sims.list$k, color = "Posterior"), show.legend = FALSE) +
  lims(x = c(0.7, 1), y = c(0, 30)) +
  xlab("k") +
  ylab("Density") +
  ggtitle("d) k")
plot(fig.6d)

# inflow d18O
x<-seq(from=-5,to=2,length.out=1000)
fig.6e <- fig.6 +
  geom_line(aes(x = x, y = dnorm(x, mean.d18Oi, sd.d18Oi), color = "Prior"), show.legend = FALSE) +
  geom_density(aes(x = post.lw.evp.f$BUGSoutput$sims.list$d18Oi, color = "Posterior"), show.legend = FALSE) +
  lims(x = c(-5, 2), y = c(0, 0.5)) +
  xlab(label.d18O) +
  ggtitle(expression(paste("e) Inflow ", delta^{18}, "O")))
plot(fig.6e)
  
# precip d18O
x<-seq(from=-2,to=4,length.out=1000)
fig.6f <- fig.6 +
  geom_line(aes(x = x, y = dnorm(x, mean.d18Op, sd.d18Op), color = "Prior"), show.legend = FALSE) +
  geom_density(aes(x = post.lw.evp.f$BUGSoutput$sims.list$d18Op, color = "Posterior"), show.legend = FALSE) +
  lims(x = c(-2, 5), y = c(0, 0.8)) +
  xlab(label.d18O) +
  ggtitle(expression(paste("f) Precipitation ", delta^{18}, "O")))
plot(fig.6f)

# precip dD
x<-seq(from=-5,to=45,length.out=1000)
fig.6g <- fig.6 +
  geom_line(aes(x = x, y = dnorm(x, mean.dDp, sd.dDp), color = "Prior"), show.legend = FALSE) +
  geom_density(aes(x = post.lw.evp.f$BUGSoutput$sims.list$dDp, color = "Posterior"), show.legend = FALSE) +
  scale_y_continuous(breaks = c(0.0, 0.1), limits = c(0, 0.13)) +
  xlim(-5, 45) +
  xlab(label.dD) +
  ggtitle(expression(paste("g) Precipitation ", delta, "D")))
plot(fig.6g)

fig6.panels <- (fig.6a | fig.6b | fig.6c | plot_spacer()) / (fig.6d | fig.6e | fig.6f | fig.6g) 

# plot(fig6.panels)
# ggsave(here("Figure6.png"), fig6.panels, device = png, width = 6.5, height = 5.6, units = "in")

# Figure S1----

# cloud plot for simulated sources 
# par(mfrow=c(1,1))
# plot(x = lw.d18O, y = lw.dD, xlab = "d18O", ylab = "dD",
#      xlim = c(-10,10), ylim = c(-80,60), 
#      col= alpha("cyan4", 0.5), pch = 16,
#      main = "Simulated water isotopes, Lake Turkana evaporation model")
# abline(a = 10, b = 8, lwd = 2)
# points(x = post.d18Oi, y = post.dDi, col= alpha("green2", 0.01))
# points(x = post.d18Op, y = post.dDp, col= alpha("blue", 0.01))
# # evaporated lake
# points(x = post.d18Ov, y = post.dDv, col= alpha("magenta2", 0.01))
# points(x = post.d18OA1, y = post.dDA1, col= alpha("red", 0.01))
# # after mixing with evaporated lake
# points(x = post.d18OA, y = post.dDA, col= alpha("red4", 0.01))
# # there are a lot of uncertainties in the values of dstar
# # the values seem to depend on three things: 
# # the vapor, the input, and x, which can all be variable
# points(x = post.dstar18O, y = post.dstarD, col= alpha("orange", 0.01))
# abline(a = intc.map, b = sl.map, lwd = 2, col = "orange4")
# abline(a = intc.hdi025, b = sl.hdi975, lwd = 1.5, col = "orange4", lty = 2)
# abline(a = intc.hdi975, b = sl.hdi025, lwd = 1.5, col = "orange4", lty = 2)
# legend(-10, 60, c("Lake water", "Inflow", "Precipitation", 
#                   "Evap. lake water", "Limiting delta",
#                   "Precip. Eq. air", "Air-Vapor mixture"),
#        pch = c(16,16,16,16,16,16,16), 
#        col = c("cyan4", "green2", "blue", "magenta2", "orange",
#                                         "red", "red4"))

fig.S1 <- ggplot(data = NULL) + 
  geom_abline(aes(slope = 8, intercept = 10, color = "GMWL")) +
  geom_point(aes(x = post.d18Op, y = post.dDp, color = "Precipitation"), shape = ".", alpha = 0.25) +
  geom_point(aes(x = post.d18Ov, y = post.dDv, color = "Evaporated lake"), shape = ".", alpha = 0.25) +
  geom_point(aes(x = post.d18Oi, y = post.dDi, color = "River"), shape = ".", alpha = 0.25) +
  geom_point(aes(x = post.d18OA1, y = post.dDA1, color = "Atmospheric vapor"), shape = ".", alpha = 0.25) +
  geom_point(aes(x = post.d18OA, y = post.dDA, color = "Mixed vapor"), shape = ".", alpha = 0.25) +
  geom_point(aes(x = post.dstar18O, y = post.dstarD, color = "Limiting ratio"), shape = ".", alpha = 0.25) +
  geom_point(aes(x = lw.d18O, y = lw.dD, color = "Lake"), shape = shape.lake) +
  geom_abline(aes(slope = sl.map, intercept = intc.map, color = "LEL")) +
  geom_abline(aes(slope = sl.hdi975, intercept = intc.hdi025, color = "LEL"), linetype = "dashed") +
  geom_abline(aes(slope = sl.hdi025, intercept = intc.hdi975, color = "LEL"), linetype = "dashed") +
  scale_color_manual(
    breaks = c("Lake", "River", "Precipitation", "Evaporated lake", "Atmospheric vapor", "Mixed vapor", "Limiting ratio", "GMWL", "LEL"),
    values = c(
      "Lake" = color.lake,
      "River" = color.river,
      "Precipitation" = color.precip,
      "Evaporated lake" = color.evap.lw,
      "Atmospheric vapor" = color.a1,
      "Mixed vapor" = color.mix,
      "Limiting ratio" = color.dstar,
      "GMWL" = color.GMWL,
      "LEL" = color.model),
    name = "Legend")  +
  guides(
    color = guide_legend(
      override.aes=list(
        linetype = c(rep("blank", 7), "solid", "solid"),
        shape = c(rep(shape.lake, 7), NA, NA),
        size = size.md))) +
  lims(x = c(-10,10), y = c(-80,60)) +
  labs(x = label.d18O, 
       y = label.dD) +
  theme(legend.position=c(vjust = 0.8, hjust = 0.15),
        legend.key.height = unit(0, "cm"),
        legend.key.width = unit(0, "cm"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title=element_blank())

# plot(fig.S1)
# ggsave(here("FigureS1.png"), fig.S1, device = png, width = 8, height = 6, units = "in")

# Density plots----

denplot(as.mcmc(post.lw.evp.f), 
        parms = c("TC", "rh.ev", "dDi", "d18Oi", "k", "dDp", "d18Op", "dDA.ev", "d18OA.ev", "dDv.ev", "d18Ov.ev", "dDL.ev", "d18OL.ev","f.ev", "sl.ev", "intc.ev"),
        style = "plain")

# bivariate density plot for f and rh
color_scheme_set(scheme = "brewer-GnBu")
mcmc_hex(as.mcmc(post.lw.evp.f), pars = c("f.ev", "rh.ev")) +
  theme(panel.grid = element_blank())

# density plot for the posterior of f
plot(density(post.f), xlim = c(0, 1), xlab = "f", ylab = "Density",
     main = "Posterior distribution of f.ev")
abline(v = f.map, lwd = 2)
abline(v = f.hdi025, lwd = 1.5 , lty = 2)
abline(v = f.hdi975, lwd = 1.5 , lty = 2)


