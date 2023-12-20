##################### Figures for the publication in Ecography #########################
#                                                                                      #
#      In this script I make the plots for th figures of the manuscript.               #
#                                                                                      #
# Figures:                                                                             #
#  1. Schematic scetch of the research                                                 #
#  2. Study sites in black forest                                                      #
#  3. DCA of forest plant communities                                                  #
#  4. Driver of Species Richness in Forests                                            #
#  5. Heterogeneity in relation to structural complexity                               #
#  6. Effects of environmental heterogeneity on plant species richness                 #
#  7. Canopy complexity measures in comparison to Species Richness                     #
#                                                                                      #
# the first three are not prepared with this script.                                   #
######################################### . ############################################

rm(list = ls())
############################ Load required R-packages ##################################
library(tidyverse)
#library(gridExtra) # grit.arrange() for saving multiple plots. cowplot is better
library(ggeffects)
library(MASS)
library(cowplot) 

###############################   Load models    ########################################
source("/media/jan/speicher/Uni/confobi/Analysis/1_struct_hetero_div_V2/load_sig_models.R", echo = F)


########################### Load colour and theme styles ################################
source("/media/jan/speicher/Uni/confobi/Analysis/1_struct_hetero_div_V2/plot_style.R", echo = F)
# Save graphics in the folder:
setwd("/media/jan/speicher/Uni/confobi/Drafts/1_structure_heterogeneity/5th_ecoevo/Figures")
rm(SR_Het1, SR_Het2, SR_Het4, SR_Het5)
#stop()


###############################   H1   ########################################
###########################   . H1 light   ####################################

DLIcv_BA$pred <- ggpredict(DLIcv_BA$bm, terms = c("basal_A [all]"))
H1.1 <- DLIcv_BA$pred %>% ggplot(aes(x, predicted)) +
  geom_point(data = DLIcv_BA$orig_data, mapping = aes(x = basal_A, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") +
  geom_smooth(method = "lm", se = T, col = regline) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = regline, linetype = 0, alpha = 0.15) +
  labs(x = expression("Basal area (m"^2*" / ha)"), y = "CV of DLI (%)"); print(H1.1)

DLIcv_DBHcv$pred <- ggpredict(DLIcv_DBHcv$bm, terms = c("DBHcv [all]"))
H1.2 <- DLIcv_DBHcv$pred %>% ggplot(aes(x, predicted)) +
  geom_point(data = DLIcv_DBHcv$orig_data, mapping = aes(x = DBHcv, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") +
  geom_smooth(method = "lm", se = T, col = regline) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = regline, linetype = 0, alpha = 0.15) +
  labs(x = "CV of DBH (%)", y = ""); print(H1.2)

DLIcv_SSCI$pred <- ggpredict(DLIcv_SSCI$bm, terms = c("SSCI [all]"))
H1.3 <- DLIcv_SSCI$pred %>% ggplot(aes(x, predicted)) +
  geom_point(data = DLIcv_SSCI$orig_data, mapping = aes(x = SSCI, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") +
  geom_smooth(method = "lm", se = T, col = regline) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = regline, linetype = 0, alpha = 0.15) +
  labs(x = "SSCI", y = "CV of DLI (%)"); print(H1.3)

DLIcv_TRI$pred <- ggpredict(DLIcv_TRI$bm, terms = c("TRI [all]"))
H1.4 <- DLIcv_TRI$pred %>% ggplot(aes(x, predicted)) +
  geom_point(data = DLIcv_TRI$orig_data, mapping = aes(x = TRI, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") +
  geom_smooth(method = "lm", se = T, col = regline) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = regline, linetype = 0, alpha = 0.15) +
  labs(x = "TRI", y = ""); print(H1.4)

ggsave(filename = "Fig_3.jpg",
       plot = plot_grid(H1.1, H1.2, H1.3, H1.4,
                        labels = c("a", "b", "c", "d"), ncol = 2),
       width = 6, height = 6,
       dpi = 800)


############################################# . H1 soil #####################################################

pHcv_DBHcv$pred <- ggemmeans(pHcv_DBHcv$bm, terms = c("DBHcv [all]", "DLI"))
pHcv_DBHcv$pred$x <- pHcv_DBHcv$pred$x * sd(pHcv_DBHcv$orig_data$DBHcv) + mean(pHcv_DBHcv$orig_data$DBHcv)
pHcv_DBHcv$pred$group <- round(as.numeric(as.character(pHcv_DBHcv$pred$group)) * sd(pHcv_DBHcv$orig_data$DLI) + mean(pHcv_DBHcv$orig_data$DLI), digits = 0) %>% as.factor()

H1.5 <- pHcv_DBHcv$pred %>% ggplot(aes(x, predicted, col = group)) + col.theme.g + col.fill.g +
  geom_point(data = pHcv_DBHcv$orig_data, mapping = aes(x = DBHcv, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") + 
  geom_smooth(se = F, show.legend = F) +
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high, fill = group), linetype = 0, alpha = 0.15, show.legend = F) +
  coord_cartesian(ylim = c(NA, 21)) +
  labs(x = "CV of DBH (%)", y = "CV of pH (%)"); print(H1.5)

pHcv_TRI$pred <- ggemmeans(pHcv_TRI$bm, terms = c("TRI [all]", "DLI"))
pHcv_TRI$pred$x <- pHcv_TRI$pred$x * sd(pHcv_TRI$orig_data$TRI) + mean(pHcv_TRI$orig_data$TRI)
pHcv_TRI$pred$group <- round(as.numeric(as.character(pHcv_TRI$pred$group)) * sd(pHcv_TRI$orig_data$DLI) + mean(pHcv_TRI$orig_data$DLI), digits = 0) %>% as.factor()

H1.6 <- pHcv_TRI$pred %>% ggplot(aes(x, predicted, col = group)) + col.theme.g + col.fill.g +
  geom_point(data = pHcv_TRI$orig_data, mapping = aes(x = TRI, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") + 
  geom_smooth(se = F) +
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high, fill = group), linetype = 0, alpha = 0.15) +
  coord_cartesian(ylim = c(NA, 21)) +
  labs(x = "TRI", y = NULL, col = "DLI", fill = "DLI"); print(H1.6)


CNcv_SSCI$pred <- ggemmeans(CNcv_SSCI$bm, terms = c("SSCI [all]", "DLI_cv"))
CNcv_SSCI$pred$x <- CNcv_SSCI$pred$x * sd(CNcv_SSCI$orig_data$SSCI) + mean(CNcv_SSCI$orig_data$SSCI)
CNcv_SSCI$pred$group <- round(as.numeric(as.character(CNcv_SSCI$pred$group)) * sd(CNcv_SSCI$orig_data$DLI_cv) + mean(CNcv_SSCI$orig_data$DLI_cv), digits = 0) %>% as.factor()

H1.7 <- CNcv_SSCI$pred %>% ggplot(aes(x, predicted, col = group)) + col.theme.g + col.fill.g +
  geom_point(data = CNcv_SSCI$orig_data, mapping = aes(x = SSCI, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") + 
  geom_smooth(se = F) +
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high, fill = group), linetype = 0, alpha = 0.15) +
  labs(x = "SSCI", y = "CV of C:N-ratio (%)", col = "CV of DLI (%)", fill = "CV of DLI (%)"); print(H1.7)


CNcv_TRI$pred <- ggemmeans(CNcv_TRI$bm, terms = c("TRI [all]", "DLI_cv", "CNratio [-1.4, -0.2, 1]"))
CNcv_TRI$pred$x <- CNcv_TRI$pred$x * sd(CNcv_TRI$orig_data$TRI) + mean(CNcv_TRI$orig_data$TRI)
CNcv_TRI$pred$group <- round(as.numeric(as.character(CNcv_TRI$pred$group)) * sd(CNcv_TRI$orig_data$DLI_cv) + mean(CNcv_TRI$orig_data$DLI_cv), digits = 0) %>% as.factor()
CNcv_TRI$pred$facet <- round(as.numeric(as.character(CNcv_TRI$pred$facet)) * sd(CNcv_TRI$orig_data$CNratio) + mean(CNcv_TRI$orig_data$CNratio), digits = 0) %>% as.factor()
CNcv_TRI$pred$facet <- factor(CNcv_TRI$pred$facet, levels = c("14", "18", "22"), 
                  labels = c("C:N-ratio of 14", "C:N-ratio of 18", "C:N-ratio of 22"))
H1.8 <- CNcv_TRI$pred %>% ggplot(aes(x, predicted, col = group)) + col.theme.g + col.fill.g +
  geom_point(data = CNcv_TRI$orig_data, mapping = aes(x = TRI, y = fitted_y), shape = 21, size = 2, fill = dots, col = "#FFFFFF") + 
  geom_smooth(se = F) +
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high, fill = group), linetype = 0, alpha = 0.15) +
  facet_grid(~facet) +
  labs(x = "TRI", y = "CV of C:N-ratio (%)", col = "CV of DLI (%)", fill = "CV of DLI (%)"); print(H1.8)


#grid.draw(cbind(ggplotGrob(H1.5), ggplotGrob(H1.6), size="last"))
ph_plots <- plot_grid(H1.5, H1.6, labels = c("a", "b"), rel_widths = c(.84, 1)); print(ph_plots)
mid_row <- plot_grid(H1.7, labels = "c", rel_widths = c(1, 0.7), ncol = 2)
ggsave(filename = "Fig_4.jpg", 
       plot = plot_grid(ph_plots, mid_row, H1.8,
                        labels = c("a", "c", "d"), 
                        ncol = 1,
                        greedy = T),
       width = 6, height = 9,
       dpi = 800)



####### H2 ########

# predict the values
SR_Het3$pred <- ggemmeans(SR_Het3$bm, terms = c("DLI_cv [all]", "CNratio_cv [-1.5, 0.75, 2]"), type = "fe")
# back scaling
SR_Het3$pred$x <- SR_Het3$pred$x * sd(SR_Het3$orig_data$DLI_cv) + mean(SR_Het3$orig_data$DLI_cv)
SR_Het3$pred$group <- round(as.numeric(as.character(SR_Het3$pred$group)) * sd(SR_Het3$orig_data$CNratio_cv) + mean(SR_Het3$orig_data$CNratio_cv), digits = 0) %>% as.factor()

H2 <- SR_Het3$pred %>% ggplot(aes(x, predicted, col = group)) + col.theme.g + col.fill.g + 
  geom_point(data = SR_Het3$orig_data, mapping = aes(x = DLI_cv, y = fitted_y), shape = 21, size = 2.5, fill = dots, col = "#FFFFFF") + 
  geom_smooth(se = F) +
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high, fill = group), linetype = 0, alpha = 0.1) + 
  coord_cartesian(ylim = c(NA, 62)) +
  labs(x = "CV of DLI (%)", y = "Species richness", col = "CV of C:N-ratio (%)", fill = "CV of C:N-ratio (%)"); print(H2)


SR_Het3$pred2 <- ggpredict(SR_Het3$bm, terms = c("DLI"), type = "fe")
SR_Het3$pred2$x <- SR_Het3$pred2$x * sd(SR_Het3$orig_data$DLI) + mean(SR_Het3$orig_data$DLI)
#SR_Het3$pred2$group <- round(as.numeric(as.character(SR_Het3$pred2$group)) * sd(SR_Het3$orig_data$DLI_cv) + mean(SR_Het3$orig_data$DLI_cv), digits = 0) %>% as.factor()

H2.2 <- SR_Het3$pred2 %>% ggplot(aes(x, predicted)) + 
  geom_point(data = SR_Het3$orig_data, mapping = aes(x = DLI, y = fitted_y), shape = 21, size = 2.5, fill = dots, col = "#FFFFFF") + 
  geom_smooth(method = "loess", se = F, col = regline) +
  geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high), fill = regline, linetype = 0, alpha = 0.1) + 
  coord_cartesian(ylim = c(NA, 62)) +
  labs(x = "DLI (%)", y = "Species richness"); print(H2.2)


ggsave(filename = "Fig_5.jpg",
       plot = plot_grid(H2, H2.2, labels=c("a", "b"), ncol = 2, rel_widths = c(3,2)),
       width = 8, height = 3.5,
       dpi = 1000)




####### H3 ########
# 
# # predict the values
# SR_TRI$pred <- ggpredict(SR_TRI$bm, terms = "TRI", type = "fe")
# # back scaling
# SR_TRI$pred$x <- SR_TRI$pred$x * sd(SR_TRI$orig_data$TRI) + mean(SR_TRI$orig_data$TRI)
# 
# H3 <- SR_TRI$pred %>% ggplot(aes(x, predicted)) + 
#   geom_point(data = SR_TRI$orig_data, mapping = aes(x = TRI, y = SR_herb_agg), shape = 21, size = 2.6, fill = dots, col = "#FFFFFF") + 
#   geom_smooth(method = "loess", se = F, col = regline) +
#   geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high), fill = regline, linetype = 0, alpha = 0.1) + 
#   #coord_cartesian(ylim = c(NA, 62)) +
#   labs(x = "TRI", y = "Species richness"); print(H3)
# 
# ggsave(filename = "H3.pdf",
#        plot = H3,
#        #plot = H2,
#        width = 10, height = 4,
#        dpi = 600)
# 
# summary(SR_TRI$bm)