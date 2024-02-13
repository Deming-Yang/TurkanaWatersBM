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
  text = element_text(family = "Helvetica", size = 12, face = "plain"),
  plot.title = element_text(size = 14, face = "bold", hjust = 0),
  axis.ticks = element_blank(),
  legend.background = element_rect(color = "gray10", fill = "white"),
  legend.key = element_blank(),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10, face = "bold"),
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
       y = label.dD,
       title = "Simulated water isotopes") +
  theme(legend.position=c(vjust = 0.8, hjust = 0.15),
        legend.key.height = unit(0, "cm"),
        legend.key.width = unit(0, "cm"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title=element_blank())

plot(fig.S1)
ggsave(here("FigureS1.png"), fig.S1, device = png, width = 8, height = 6, units = "in")

# Density plots----
# density plot for the posterior of f
plot(density(post.f), xlim = c(0, 1), xlab = "f", ylab = "Density",
     main = "Posterior distribution of f.ev")
abline(v = f.map, lwd = 2)
abline(v = f.hdi025, lwd = 1.5 , lty = 2)
abline(v = f.hdi975, lwd = 1.5 , lty = 2)

# bivariate density plot
# between f and rh
mcmc_hex(as.mcmc(post.lw.evp.f), pars = c("f.ev", "rh.ev"))
