############################## Summary statistics for the Manuscript ############################
#                                                                                               #
# In this script summary statistics for the paper is calculated.                                #
# November/ December 2019                                                                       #
# Jan Helbach                                                                                   #
#                                                                                               #
#################################################################################################


####################################### Required packages #######################################
#rm(list = ls())
#required libraries:
library(tidyverse)
library(MASS)
library(cowplot)
#library(lme4)
#library(multcomp) # für p-werte  #cftest(model)
#library(MuMIn)
#library(ggeffects)
#library(effects)
library(xtable)
#########################################    Data    #######################################################

setwd("/home/jan/confobi/Data")
x <- read.csv(sort(list.files(path = "/home/jan/confobi/Data", pattern = glob2rx("all_plot*")), decreasing = T)[1], stringsAsFactors = T)



############################# load colour and theme styles ###############################
source("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plot_style.R", echo = F)


################################## phytosociological clusters ####################################
stat_table <- x %>% dplyr::select(phyt_clust4, SR_herb, DLI, DLI_cv, pH, pH_cv, CNratio, CNratio_cv, P, P_cv, altitude, aspect, basal_A, DBHcv, SSCI, TRI) %>% 
  group_by(phyt_clust4) %>% summarise_all(list(mean = mean, sd = sd), na.rm = T)

# put table into right shape:
a <- as.vector(stat_table$phyt_clust4)
stat_table <- stat_table %>% transmute_if(is.numeric, round, digits = 2)
stat_table <- data.frame(t(stat_table))
colnames(stat_table) <- a

#stat_table <- stat_table[-1,]
stat_table_mean <- stat_table[1:(nrow(stat_table)/2), 1:4]
stat_table_sd <- stat_table[(nrow(stat_table)/2+1):nrow(stat_table), 1:4]
stat_table <- rownames_to_column(stat_table, var = "Measure")
stat_table <- bind_cols(dplyr::select(stat_table[1:(nrow(stat_table)/2), ], Measure), stat_table_mean, stat_table_sd)
names(stat_table) <- c("Measure", "Gal+Luz-Fagetum", "Galio-Abietetum", "Pyrolo-Abietetum", "Vac+Luz-Abietetum", "Gal+Luz-Fagetum1", "Galio-Abietetum1", "Pyrolo-Abietetum1", "Vac+Luz-Abietetum1")
stat_table <- stat_table %>% map_dfc(as.character)
stat_table$`Galio-Abietetum` <- paste(stat_table$`Galio-Abietetum`, stat_table$`Galio-Abietetum1`, sep = " \u00B1 ")
stat_table$`Gal+Luz-Fagetum` <- paste(stat_table$`Gal+Luz-Fagetum`, stat_table$`Gal+Luz-Fagetum1`, sep = " \u00B1 ")
stat_table$`Pyrolo-Abietetum` <- paste(stat_table$`Pyrolo-Abietetum`, stat_table$`Pyrolo-Abietetum1`, sep = " \u00B1 ")
stat_table$`Vac+Luz-Abietetum` <- paste(stat_table$`Vac+Luz-Abietetum`, stat_table$`Vac+Luz-Abietetum1`, sep = " \u00B1 ")
stat_table <- stat_table[,1:5]
stat_table$Measure <- stat_table$Measure %>% str_replace("_mean", "")



print(xtable(stat_table), include.rownames = F, hline = c(0,0))
#write.csv(stat_table, "/home/jan/confobi/Drafts/1_structure_heterogeneity/community_properties20210201.csv", row.names = F) #outcommented because there are manual changes


# Simple anovas for the different responsevarioables for the ohyto_clusters
# tukey test:
summary(phytosoc_SR <- lm(SR_herb ~ phyt_clust4, data = x))    # ***
summary(phytosoc_DLI <- lm(DLI ~ phyt_clust4, data = x))             # -
summary(phytosoc_DLI_cv <- lm(DLI_cv ~ phyt_clust4, data = x))       # -
summary(phytosoc_pH <- lm(pH ~ phyt_clust4, data = x))               # ***
summary(phytosoc_pH_cv <- lm(pH_cv ~ phyt_clust4, data = x))         # ***
summary(phytosoc_CNratio <- lm(CNratio ~ phyt_clust4, data = x))     # ***
summary(phytosoc_CNratio_cv <- lm(CNratio_cv ~ phyt_clust4, data = x))
#summary(phytosoc_P <- lm(P ~ phyt_clust4, data = x))                 # *
#summary(phytosoc_P_cv <- lm(P_cv ~ phyt_clust4, data = x))                 # *

summary(phytosoc_alt <- lm(altitude ~ phyt_clust4, data = x))        # ***
#summary(phytosoc_asp <- lm(aspect ~ phyt_clust4, data = x))          # -

#summary(phytosoc_ammo <- lm(ammo.f.all ~ phyt_clust4, data = x))     # -
#summary(phytosoc_nitr <- lm(nitr.f.all ~ phyt_clust4, data = x))     # **

summary(phytosoc_BA <- lm(basal_A ~ phyt_clust4, data = x))          # -
summary(phytosoc_DBHcv <- lm(DBHcv ~ phyt_clust4, data = x))         # -
summary(phytosoc_TRI <- lm(TRI ~ phyt_clust4, data = x))             # *
summary(phytosoc_SSCI <- lm(SSCI ~ phyt_clust4, data = x))           # -



########### Tucky-Test for the summary table ##################
library(emmeans)
models <- ls(pattern = "phytosoc")
for (i in models) {
  a <- get(i)
  a_means <- emmeans(a, "phyt_clust4")
  print( paste("################", i, "###############"))
  print(pairs(a_means))
  
}


############################### Boxplots ##################################

# Boxplot for species richness
community_SR <- x %>% dplyr::filter(!is.na(SR_herb)) %>% ggplot(aes(x = phyt_clust4, y = SR_herb, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T) + col.fill + labs(x = "Forest communities", y = "Species richness", fill = "Forest communities") +
  annotate("text", x = c(1,2,3,4), y=68, label = c("CD","CD","ABD","ABC")) +
  theme(axis.text.x = element_blank()); print(community_SR)
# outsource the legend
legend <- get_legend(community_SR)
# Remove the legend from the box plot
community_SR <- community_SR + theme(legend.position="none")

# Boxplots for environmental parameters
community_pH <- x %>% dplyr::filter(!is.na(pH)) %>% ggplot(aes(x = phyt_clust4, y = pH, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "pH", fill = "Forest communities") +
  theme(axis.text.x = element_blank()); print(community_pH)

community_pH_cv <- x %>% dplyr::filter(!is.na(pH_cv)) %>% ggplot(aes(x = phyt_clust4, y = pH_cv, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "CV of pH (%)", fill = "Forest communities") +
  annotate("text", x = c(1,2.2,3,4), y=30, label = c("BCD","A","A","A")) +
  theme(axis.text.x = element_blank()); print(community_pH_cv)


community_CNratio <- x %>% dplyr::filter(!is.na(CNratio)) %>% ggplot(aes(x = phyt_clust4, y = CNratio, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "C:N-ratio", fill = "Forest communities") +
  theme(axis.text.x = element_blank()); print(community_CNratio)

community_CNratio_cv <- x %>% dplyr::filter(!is.na(CNratio_cv)) %>% ggplot(aes(x = phyt_clust4, y = CNratio_cv, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "CV of CNratio (%)", fill = "Forest communities") +
  annotate("text", x = 3.2, y=30, label = "BD") +
  theme(axis.text.x = element_blank()); print(community_CNratio_cv)


community_alt <- x %>% dplyr::filter(!is.na(altitude)) %>% ggplot(aes(x = phyt_clust4, y = altitude, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "Altitude (m)", fill = "Forest communities") +
  theme(axis.text.x = element_blank()); print(community_alt)


# Boxplots for forest structural parameters:
community_BA <- x %>% dplyr::filter(!is.na(basal_A)) %>% ggplot(aes(x = phyt_clust4, y = basal_A, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = 'Basal area (m"^2*" / ha)', fill = "Forest communities") +
  theme(axis.text.x = element_blank()); print(community_BA)

community_DBHcv <- x %>% dplyr::filter(!is.na(DBHcv)) %>% ggplot(aes(x = phyt_clust4, y = DBHcv, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "CV of DBH (%)", fill = "Forest communities") +
  theme(axis.text.x = element_blank()); print(community_DBHcv)

community_SSCI <- x %>% dplyr::filter(!is.na(SSCI)) %>% ggplot(aes(x = phyt_clust4, y = SSCI, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "SSCI", fill = "Forest communities") +
  annotate("text", x = 3, y=10, label = "B") + annotate("text", x = 2, y=10, label = "C") +
  theme(axis.text.x = element_blank()); print(community_SSCI)

community_TRI <- x %>% dplyr::filter(!is.na(TRI)) %>% ggplot(aes(x = phyt_clust4, y = TRI, fill = phyt_clust4)) + 
  geom_boxplot(na.rm = T, show.legend = F) + col.fill + labs(x = "Forest communities", y = "TRI", fill = "Forest communities") + 
  annotate("text", x = 1, y=0.95, label = "B") + annotate("text", x = 2.14, y=0.95, label = "A") +
  theme(axis.text.x = element_blank()); print(community_TRI)



setwd("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plots/manuscript_final")

# save the most relevant graphics

ggsave(filename = "Forest_community_plots.pdf",
       #plot = grid.arrange(community_SR, community_pH, community_pH_cv, community_alt, community_TRI, legend, ncol = 2),
       plot = plot_grid(community_pH_cv, community_CNratio_cv, community_SSCI, community_TRI, community_SR, legend,  
                        labels=c("a", "b", "c", "d", "e"), ncol = 2),
       width = 7, height = 10,
       dpi = 600)
