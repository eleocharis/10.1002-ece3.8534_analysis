# only the distributions part from 2_struct connectivity

# list of available distributions:
# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/Distributions.html

rm(list = ls())
library(tidyverse)
library(MASS)
library(fitdistrplus)
library(logspline)

setwd("/home/jan/confobi/Data")

x <- read.csv(sort(list.files(path = "/home/jan/confobi/Data", pattern = glob2rx("all_plot*")), decreasing = T)[1], stringsAsFactors = T)
y <- read.csv(sort(list.files(path = "/home/jan/confobi/Data", pattern = glob2rx("all_6x55*")), decreasing = T)[1], stringsAsFactors = T)


######################  Find distribution for SR_understory_herb1  ######################
plotdist(x$SR_understory, histo = TRUE, demp = TRUE, discrete = TRUE)
descdist(x$SR_understory, discrete = TRUE, boot=500)

fit.poiss <- fitdist(x$SR_understory, "pois")
fit.negbinom <- fitdist(x$SR_understory, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1726.104
fit.negbinom$aic #[1] 1147.697


# Distribution SR_understory:  Negative binomial with lamda = 6 -->  negative.binomial(6)


stddw.dist <- data.frame(x$vol_standDW)
stddw.dist <- stddw.dist %>% dplyr::filter(x.vol_standDW != "NA")
stddw.dist <- stddw.dist %>% dplyr::filter(x.vol_standDW != "NaN")
stddw.dist <- stddw.dist %>% dplyr::filter(x.vol_standDW <= 300)

plotdist(stddw.dist$x.vol_standDW, histo = TRUE, demp = TRUE, discrete = F)
descdist(x$vol_standDW, discrete = F, boot = 1000)

fit.weibull <- fitdist(stddw.dist$x.vol_standDW, "weibull")
fit.norm <- fitdist(stddw.dist$x.vol_standDW, "norm")
fit.gamma <- fitdist(stddw.dist$x.vol_standDW, "gamma")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "normal", "gamma")
denscomp(list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
cdfcomp (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
qqcomp  (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
ppcomp  (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
gofstat(list(fit.weibull, fit.norm, fit.gamma), fitnames = c("Weibull", "normal", "gamma"))
fit.weibull$aic #[1] 907.807
fit.norm$aic    #[1] 1158.258
fit.gamma$aic   #[1] 915.724

##########################################################################################################
# for vol_standDW  weibull(1st) or gamma (2end)
##########################################################################################################


######################  Find distribution for SR_tree  ######################
par(mfrow = c(1, 2))
plotdist(x$SR_tree, histo = TRUE, demp = TRUE, discrete = TRUE)
par(mfrow = c(1, 1))
descdist(x$SR_tree, discrete = TRUE, boot=500)

fit.poiss <- fitdist(x$SR_tree, "pois")
fit.negbinom <- fitdist(x$SR_tree, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1733.737
fit.negbinom$aic #[1] 1145.773

######################  Find distribution for SSCI  ######################

SSCI2 <- x  %>% filter(SSCI != "NA") %>% dplyr::select(SSCI)

plotdist(SSCI2$SSCI, histo = T, demp = F, discrete = F)
par(mfrow = c(1, 1))
descdist(SSCI2$SSCI, discrete = F, boot = 1000)

fit.weibull <- fitdist(SSCI2$SSCI, "weibull")
fit.norm <- fitdist(SSCI2$SSCI, "norm")
fit.gamma <- fitdist(SSCI2$SSCI, "gamma")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "normal", "gamma")
denscomp(list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
cdfcomp (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
qqcomp  (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
ppcomp  (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
gofstat(list(fit.weibull, fit.norm, fit.gamma), fitnames = c("Weibull", "normal", "gamma"))
# gamma distribution


######################  Find distribution for SR_herb  ######################
plotdist(y$SR_herb, histo = TRUE, demp = F, discrete = T)
par(mfrow = c(1, 1))
descdist(y$SR_herb, discrete = TRUE, boot=500)

fit.poiss <- fitdist(x$SR_understory, "pois")
fit.negbinom <- fitdist(x$SR_understory, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1733.737
fit.negbinom$aic #[1] 1145.773

# Distribution SR_understory:  Negative binomial 

######################  Find distribution for SR_herb_agg  ######################
x <- x %>% dplyr::filter(SR_herb_agg != "NA")
plotdist(x$SR_herb_agg, histo = TRUE, demp = F, discrete = T)
par(mfrow = c(1, 1))
descdist(x$SR_herb_agg, discrete = TRUE, boot=500)

fit.poiss <- fitdist(x$SR_herb_agg, "pois")
fit.negbinom <- fitdist(x$SR_herb_agg, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 7148.390
fit.negbinom$aic #[1] 5132.997

# Distribution SR_understory:  Negative binomial 



#####################  vol_standDW  ###############################  
stddw.dist <- data.frame(x$vol_standDW)
stddw.dist <- stddw.dist %>% dplyr::filter(x.vol_standDW != "NA")
stddw.dist <- stddw.dist %>% dplyr::filter(x.vol_standDW != "NaN")
stddw.dist <- stddw.dist %>% dplyr::filter(x.vol_standDW <= 300)

plotdist(stddw.dist$x.vol_standDW, histo = TRUE, demp = TRUE, discrete = F)
descdist(x$vol_standDW, discrete = F, boot = 1000)

fit.weibull <- fitdist(stddw.dist$x.vol_standDW, "weibull")
fit.norm <- fitdist(stddw.dist$x.vol_standDW, "norm")
fit.gamma <- fitdist(stddw.dist$x.vol_standDW, "gamma")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "normal", "gamma")
denscomp(list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
cdfcomp (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
qqcomp  (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
ppcomp  (list(fit.weibull, fit.norm, fit.gamma), legendtext = plot.legend)
gofstat(list(fit.weibull, fit.norm, fit.gamma), fitnames = c("Weibull", "normal", "gamma"))
fit.weibull$aic #[1] 907.806
fit.norm$aic    #[1] 1158.258
fit.gamma$aic   #[1] 915.724

##########################################################################################################
# for vol_standDW  weibull or gamma
##########################################################################################################


#####################  plot hCover  ###############################  
stddw.dist <- data.frame(x$hCover)
stddw.dist <- stddw.dist %>% dplyr::filter(x.hCover != "NA")
stddw.dist <- stddw.dist %>% dplyr::filter(x.hCover != "NaN")
stddw.dist <- stddw.dist %>% dplyr::filter(x.hCover <= 300)

plotdist(stddw.dist$x.hCover, histo = TRUE, demp = TRUE)
descdist(stddw.dist$x.hCover, discrete = F, boot = 1000)

fit.norm <- fitdist(stddw.dist$x.hCover, "norm")
fit.gamma <- fitdist(stddw.dist$x.hCover, "gamma")
fit.uni <- fitdist(stddw.dist$x.hCover, "unif")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "uniform")
denscomp(list(fit.norm, fit.gamma, fit.uni), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.uni), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.uni), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.uni), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.uni), fitnames = c("normal", "gamma", "uniform"))
fit.norm$aic    #[1] 1158.258
fit.gamma$aic   #[1] 915.724
fit.uni$aic # NA
par(mfrow = c(1, 1))



##################### subplot hCover  ###############################  


plotdist(y$hCover, histo = TRUE, demp = TRUE)
descdist(y$hCover, discrete = T, boot = 1000)

fit.norm <- fitdist(y$hCover, "norm")
fit.poiss <- fitdist(y$hCover, "pois")
fit.negbinom <- fitdist(y$hCover, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "poisson", "nbinom")
denscomp(list(fit.norm, fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.negbinom), fitnames = c("normal", "gamma", "neg. binomial"))
fit.norm$aic    #[1] 1158.258
fit.gamma$aic   #[1] 915.724
fit.uni$aic # NA
par(mfrow = c(1, 1))
#normal seam to bi fine
summary(y$hCover)
#####################  shanon index of herbs aggregated  ###############################  
x <- x %>% dplyr::filter(shan_herb_agg != "NA")
x <- x %>% dplyr::filter(shan_herb_agg > 0)
plotdist(x$shan_herb_agg, histo = TRUE, demp = TRUE, discrete = F)
descdist(x$shan_herb_agg, discrete = F, boot = 1000)

#fit.weibull <- fitdist(x$shan_herb_agg, "weibull")
fit.norm <- fitdist(x$shan_herb_agg, "norm")
fit.gamma <- fitdist(as.vector(x$shan_herb_agg), "gamma")
fit.lognorm <- fitdist(x$shan_herb_agg, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal")
denscomp(list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm), fitnames = c("normal", "gamma", "log-normal"))
fit.norm$aic    #[1] 251.2
fit.gamma$aic   #[1] 270.7
fit.lognorm$aic #[1] 284.5
#the latter two distributions have tailing, my data have fronting. --> 

# Due to the incontinous cover data the distribution is not really indiscrete. 
# Thus trying poisson and neg-binom.


plotdist(x$shan_herb_agg, histo = T, demp = T, discrete = T)
par(mfrow = c(1, 1))
descdist(x$shan_herb_agg, discrete = TRUE, boot=500)

fit.poiss <- fitdist(x$SR_understory, "pois")
fit.negbinom <- fitdist(x$SR_understory, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1683.4
fit.negbinom$aic #[1] 1131.4



#####################  DLI_sd   ###############################  
x1 <- x %>% dplyr::select(plotID, DLI, DLI_sd) %>% dplyr::filter(DLI_sd != "NA")
#x1 <- x %>% dplyr::filter(shan_herb_agg > 0)
plotdist(x1$DLI_sd, histo = TRUE, demp = TRUE, discrete = F)
par(mfrow = c(1, 1))
descdist(x1$DLI_sd, discrete = F, boot = 1000)


fit.norm <- fitdist(x1$DLI_sd, "norm")
fit.gamma <- fitdist(x1$DLI_sd, "gamma")
fit.lognorm <- fitdist(x1$DLI_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal")
denscomp(list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm), fitnames = c("normal", "gamma", "log-normal"))

## Gamma is best!
#####################  DLI_cv   ###############################  
x1 <- x %>% dplyr::select(plotID, DLI, DLI_cv) %>% dplyr::filter(DLI_cv != "NA")
#x1 <- x %>% dplyr::filter(shan_herb_agg > 0)
plotdist(x1$DLI_cv, histo = TRUE, demp = TRUE, discrete = F)
par(mfrow = c(1, 1))
descdist(x1$DLI_cv, discrete = F, boot = 1000)


fit.norm <- fitdist(x1$DLI_cv, "norm")
fit.gamma <- fitdist(x1$DLI_cv, "gamma")
fit.lognorm <- fitdist(x1$DLI_cv, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal")
denscomp(list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm), fitnames = c("normal", "gamma", "log-normal"))

#####################  DLI_var   ###############################

x1 <- x %>% dplyr::select(plotID, DLI, DLI_sd, DLI_var) %>% dplyr::filter(DLI_var != "NA")
#x1 <- x %>% dplyr::filter(shan_herb_agg > 0)
par(mfrow = c(1, 1))
plotdist(x1$DLI_var, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$DLI_var, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$DLI_var, "weibull")
fit.norm <- fitdist(x1$DLI_var, "norm")
fit.gamma <- fitdist(x1$DLI_var, "gamma")
fit.lognorm <- fitdist(x1$DLI_var, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal"))

## Gamma is best!


#####################  N_sd   ###############################

x1 <- x %>% dplyr::select(plotID, N_sd) %>% dplyr::filter(N_sd != "NA")
par(mfrow = c(1, 1))
plotdist(x1$N_sd, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$N_sd, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$N_sd, "weibull")
fit.norm <- fitdist(x1$N_sd, "norm")
fit.gamma <- fitdist(x1$N_sd, "gamma")
fit.lognorm <- fitdist(x1$N_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!

#####################  C_sd   ###############################

x1 <- x %>% dplyr::select(plotID, C_sd) %>% dplyr::filter(C_sd != "NA")
par(mfrow = c(1, 1))
plotdist(x1$C_sd, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$C_sd, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$C_sd, "weibull")
fit.norm <- fitdist(x1$C_sd, "norm")
fit.gamma <- fitdist(x1$C_sd, "gamma")
fit.lognorm <- fitdist(x1$C_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!

#####################  C_cv   ###############################  
x1 <- x %>% dplyr::select(plotID, C, C_cv) %>% dplyr::filter(C_cv != "NA")
#x1 <- x %>% dplyr::filter(shan_herb_agg > 0)
plotdist(x1$C_cv, histo = TRUE, demp = TRUE, discrete = F)
par(mfrow = c(1, 1))
descdist(x1$C_cv, discrete = F, boot = 1000)


fit.norm <- fitdist(x1$C_cv, "norm")
fit.gamma <- fitdist(x1$C_cv, "gamma")
fit.lognorm <- fitdist(x1$C_cv, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal")
denscomp(list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm), fitnames = c("normal", "gamma", "log-normal"))

#####################  ammo.f.all_sd   ###############################

x1 <- x %>% dplyr::select(plotID, ammo.f.all_sd) %>% dplyr::filter(ammo.f.all_sd != "NA")
par(mfrow = c(1, 1))
plotdist(x1$ammo.f.all_sd, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$ammo.f.all_sd, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$ammo.f.all_sd, "weibull")
fit.norm <- fitdist(x1$ammo.f.all_sd, "norm")
fit.gamma <- fitdist(x1$ammo.f.all_sd, "gamma")
fit.lognorm <- fitdist(x1$ammo.f.all_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!


#####################  pH_sd   ###############################

x1 <- x %>% dplyr::select(plotID, pH_sd) %>% dplyr::filter(pH_sd != "NA")
par(mfrow = c(1, 1))
plotdist(x1$pH_sd, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$pH_sd, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$pH_sd, "weibull")
fit.norm <- fitdist(x1$pH_sd, "norm")
fit.gamma <- fitdist(x1$pH_sd, "gamma")
fit.lognorm <- fitdist(x1$pH_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!

#####################  pH_cv   ###############################

x1 <- x %>% dplyr::select(plotID, pH_cv) %>% dplyr::filter(pH_cv != "NA")
par(mfrow = c(1, 1))
plotdist(x1$pH_cv, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$pH_cv, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$pH_cv, "weibull")
fit.norm <- fitdist(x1$pH_cv, "norm")
fit.gamma <- fitdist(x1$pH_cv, "gamma")
fit.lognorm <- fitdist(x1$pH_cv, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!

#####################  CNratio_cv   ###############################

x1 <- x %>% dplyr::select(plotID, CNratio_cv) %>% dplyr::filter(CNratio_cv != "NA")
par(mfrow = c(1, 1))
plotdist(x1$CNratio_cv, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$CNratio_cv, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$CNratio_cv, "weibull")
fit.norm <- fitdist(x1$CNratio_cv, "norm")
fit.gamma <- fitdist(x1$CNratio_cv, "gamma")
fit.lognorm <- fitdist(x1$CNratio_cv, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!

#####################  N_cv   ###############################

x1 <- x %>% dplyr::select(plotID, N_cv) %>% dplyr::filter(N_cv != "NA")
par(mfrow = c(1, 1))
plotdist(x1$N_cv, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$N_cv, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$N_cv, "weibull")
fit.norm <- fitdist(x1$N_cv, "norm")
fit.gamma <- fitdist(x1$N_cv, "gamma")
fit.lognorm <- fitdist(x1$N_cv, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!

#####################  P_sd   ###############################

x1 <- x %>% dplyr::select(plotID, P_sd) %>% dplyr::filter(P_sd != "NA")
par(mfrow = c(1, 1))
plotdist(x1$P_sd, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$P_sd, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$P_sd, "weibull")
fit.norm <- fitdist(x1$P_sd, "norm")
fit.gamma <- fitdist(x1$P_sd, "gamma")
fit.lognorm <- fitdist(x1$P_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!



#####################  K_sd   ###############################

x1 <- x %>% dplyr::select(plotID, K_sd) %>% dplyr::filter(K_sd != "NA")
par(mfrow = c(1, 1))
plotdist(x1$K_sd, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$K_sd, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$K_sd, "weibull")
fit.norm <- fitdist(x1$K_sd, "norm")
fit.gamma <- fitdist(x1$K_sd, "gamma")
fit.lognorm <- fitdist(x1$K_sd, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## log-normal is best!



#####################  DLI   ###############################

x1 <- x %>% dplyr::select(plotID, DLI) %>% dplyr::filter(DLI != "NA")
par(mfrow = c(1, 1))
plotdist(x1$DLI, histo = TRUE, demp = TRUE, discrete = F)
descdist(x1$DLI, discrete = F, boot = 1000)

fit.weibull <- fitdist(x1$DLI, "weibull")
fit.norm <- fitdist(x1$DLI, "norm")
fit.gamma <- fitdist(x1$DLI, "gamma")
fit.lognorm <- fitdist(x1$DLI, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("normal", "gamma", "log-normal", "weibull")
denscomp(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
cdfcomp (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
qqcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
ppcomp  (list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), legendtext = plot.legend)
gofstat(list(fit.norm, fit.gamma, fit.lognorm, fit.weibull), fitnames = c("normal", "gamma", "log-normal", "weibull"))

## normal is best!



#####################  n_standDW   ###############################

x_DW <- x %>% dplyr::select(plotID, n_standDW) %>% dplyr::filter(n_standDW != "NA")
par(mfrow = c(1, 1))
boxplot(x_DW$n_standDW)
summary(x_DW$n_standDW)

x_DW <- x %>% dplyr::select(plotID, n_standDW) %>% dplyr::filter(n_standDW != "NA" & n_standDW <= 34)
plotdist(x_DW$n_standDW, histo = TRUE, demp = TRUE, discrete = TRUE)
descdist(x_DW$n_standDW, discrete = TRUE, boot=500)

fit.poiss <- fitdist(x_DW$n_standDW, "pois")
fit.negbinom <- fitdist(x_DW$n_standDW, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 6221.845
fit.negbinom$aic #[1] 1210.338



######################  Find distribution for SR_herb_com1  ######################
y_herb1 <- y %>% dplyr::select(plotID, SR_herb, phyt_clust4) %>% dplyr::filter(phyt_clust4 == "Gal+Luz-Fagetum")

plotdist(y_herb1$SR_herb, histo = TRUE, demp = F, discrete = T)
par(mfrow = c(1, 1))
descdist(y_herb1$SR_herb, discrete = TRUE, boot=500)

fit.poiss <- fitdist(y_herb1$SR_herb, "pois")
fit.negbinom <- fitdist(y_herb1$SR_herb, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1733.737
fit.negbinom$aic #[1] 1145.773

######################  Find distribution for SR_herb_com2  ######################
y_herb2 <- y %>% dplyr::select(plotID, SR_herb, phyt_clust4) %>% dplyr::filter(phyt_clust4 == "Galio-Abietetum")

plotdist(y_herb2$SR_herb, histo = TRUE, demp = F, discrete = T)
par(mfrow = c(1, 1))
descdist(y_herb2$SR_herb, discrete = TRUE, boot=500)

fit.poiss <- fitdist(y_herb2$SR_herb, "pois")
fit.negbinom <- fitdist(y_herb2$SR_herb, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1874.624
fit.negbinom$aic #[1] 1708.242

######################  Find distribution for SR_herb_com3  ######################
y_herb3 <- y %>% dplyr::select(plotID, SR_herb, phyt_clust4) %>% dplyr::filter(phyt_clust4 == "Pyrolo-Abietetum")

plotdist(y_herb3$SR_herb, histo = TRUE, demp = F, discrete = T)
par(mfrow = c(1, 1))
descdist(y_herb3$SR_herb, discrete = TRUE, boot=500)

fit.poiss <- fitdist(y_herb3$SR_herb, "pois")
fit.negbinom <- fitdist(y_herb3$SR_herb, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1099.151
fit.negbinom$aic #[1] 972.5204


######################  Find distribution for SR_herb_com4  ######################
y_herb4 <- y %>% dplyr::select(plotID, SR_herb, phyt_clust4) %>% dplyr::filter(phyt_clust4 == "Vac+Luz-Abietet")

plotdist(y_herb4$SR_herb, histo = TRUE, demp = F, discrete = T)
par(mfrow = c(1, 1))
descdist(y_herb4$SR_herb, discrete = TRUE, boot=500)

fit.poiss <- fitdist(y_herb4$SR_herb, "pois")
fit.negbinom <- fitdist(y_herb4$SR_herb, "nbinom")
par(mfrow = c(2, 2))
plot.legend <- c("Poisson", "Negative binomial")
denscomp(list(fit.poiss, fit.negbinom), legendtext = plot.legend)
cdfcomp (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
qqcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
ppcomp  (list(fit.poiss, fit.negbinom), legendtext = plot.legend)
gofstat(list(fit.poiss, fit.negbinom), fitnames = c("Poisson", "Negative binomial"))
fit.poiss$aic    #[1] 1233.135
fit.negbinom$aic #[1] 1181.541

