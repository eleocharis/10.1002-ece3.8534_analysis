########################################  4. Ordinations  ###############################################

rm(list = ls())
############################ Load required R-packages ##################################
library(tidyverse)
library(ggeffects)
#library(MASS)
library(gridExtra)
library(cowplot)
library(vegan)
#library(viridis)   #scale_color_viridis(option = "A", discrete = T)
#library(RColorBrewer)
#library(sjlabelled)

###################################   Load and prepare Data    ######################################
setwd("/home/jan/confobi/Data")
x <- read.csv(sort(list.files(path = "/home/jan/confobi/Data", pattern = glob2rx("all_plot*")), decreasing = T)[1], stringsAsFactors = T)
#y <- read.csv(sort(list.files(path = "/home/jan/confobi/Data", pattern = glob2rx("all_6x55*")), decreasing = T)[1], stringsAsFactors = T)

x_phyt_clust <- x %>% dplyr::select(plotID, phyt_clust4)
x <- x %>% dplyr::select(-phyt_clust4, -Bedrock, -nitr, -ammo, -X, -Y, -mCover_SD, -stemarea)
#y <- y %>% dplyr::select(-phyt_clust4, -Bedrock)

x <- column_to_rownames(x, var = "plotID")
#y <- column_to_rownames(y, var = "subplot")

herb_p <- read.csv("/home/jan/confobi/Data/vegdata/6x25/agg_understory20200731.csv", stringsAsFactors = FALSE)
herb_p <- herb_p[order(herb_p$plotID), ]
herb_p <- remove_rownames(herb_p)
herb_p <- column_to_rownames(herb_p, var = "plotID")
# herb <- read.csv("/home/jan/confobi/Data/vegdata/6x25/6x55_herb20190510.csv", stringsAsFactors = FALSE)
# herb <- herb[order(herb$subplot), ]
# herb <- remove_rownames(herb)
# herb <- column_to_rownames(herb, var = "subplot")

########################### Load colour and theme styles ################################
source("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plot_style.R", echo = T)
# Save graphics in the folder:
setwd("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plots")


######################################## . DCA  ###############################################

# Making an DCA and plot it in ggplot with grouping:
dca <- decorana(herb_p)
# creating dataframes:
## sites with environmental parameters:
dca_env <- as.data.frame(scores(dca, display = "sites"))
dca_env <- rownames_to_column(dca_env, var = "plotID")
dca_env <- left_join(dca_env, (x_phyt_clust %>% dplyr::select(plotID, phyt_clust4)))
## species tables 
dca_sp <- as.data.frame(scores(dca, display = "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
dca_sp <- rownames_to_column(dca_sp, var = "species") # create a column of species, from the rownames of dca_sp

# Another way to look at these is to plot a hull around each of the groups. To accomplish this, 
# you can utilize the chull function. 
phyt_clust4.1 <- dca_env[dca_env$phyt_clust4 == "Galio-Abietetum", ][chull(dca_env[dca_env$phyt_clust4 == 
                                                                                     "Galio-Abietetum", c("DCA1", "DCA2")]), ]  # hull values for phyt_clust4 1
phyt_clust4.2 <- dca_env[dca_env$phyt_clust4 == "Vac+Luz-Abietetum", ][chull(dca_env[dca_env$phyt_clust4 == 
                                                                                     "Vac+Luz-Abietetum", c("DCA1", "DCA2")]), ]  # hull values for phyt_clust4 2
phyt_clust4.3 <- dca_env[dca_env$phyt_clust4 == "Gal+Luz-Fagetum", ][chull(dca_env[dca_env$phyt_clust4 == 
                                                                                     "Gal+Luz-Fagetum", c("DCA1", "DCA2")]), ]  # hull values for phyt_clust4 3
phyt_clust4.4 <- dca_env[dca_env$phyt_clust4 == "Pyrolo-Abietetum", ][chull(dca_env[dca_env$phyt_clust4 == 
                                                                                     "Pyrolo-Abietetum", c("DCA1", "DCA2")]), ]  # hull values for phyt_clust4 4
hull.dca <- rbind(phyt_clust4.1, phyt_clust4.2, phyt_clust4.3, phyt_clust4.4)  #combine phyt_clust4.a and phyt_clust4.b
hull.dca$phyt_clust4 <- as.factor(hull.dca$phyt_clust4)
hull.dca <- hull.dca %>% dplyr::filter(!is.na(phyt_clust4))

# add environmental vectors:
dca_arrows <- envfit(dca, x, na.rm = T, permu = 999)
dca_arrows.df <- as.data.frame(dca_arrows$vectors$arrows*sqrt(dca_arrows$vectors$r))
dca_arrows.df <- rownames_to_column(dca_arrows.df, var = "env.factors")
dca_arrows.pvals <- as.data.frame(dca_arrows$vectors$pvals); names(dca_arrows.pvals)[1] <- "p-values"
dca_arrows.pvals <- rownames_to_column(dca_arrows.pvals, var = "env.factors")
dca_arrows.df <- left_join(dca_arrows.df, dca_arrows.pvals, by = "env.factors")
# filter arrows by p-value < 0.05
dca_arrows.df <- dca_arrows.df %>% dplyr::filter(`p-values` < 0.1)


dca_plot <- ggplot() + col.theme +
  geom_segment(data = dca_arrows.df, aes(x = 0, xend = DCA1*1.2, y = 0, yend = DCA2*1.2),
               arrow = arrow(length = unit(0.3, "cm")), colour = regline) +  # shows environmental vectors
  #geom_text(data = dca_sp, aes(x = DCA1, y = DCA2, label = species), alpha = 0.5, size = 2) +  # add the species labels
  #geom_point(data = dca_env, aes(x = DCA1, y = DCA2, colour = phyt_clust4), size = 1) + # add the point markers
  geom_text(data = dca_env, aes(x = DCA1, y = DCA2, col = phyt_clust4, label = plotID),size = 3, vjust = 0) +  # add the plotID labels
  geom_polygon(data = hull.dca, aes(x = DCA1, y = DCA2, colour = phyt_clust4), alpha = 0.00) + # add the convex hulls
  geom_text(data = dca_arrows.df, aes(x = DCA1*1.6, y = DCA2*1.6, label = env.factors), size = 3) + # shows names of environmental vectors
  #coord_fixed() + coord_equal() +
  labs(col = "Forest Communities", x = "DCA1", y = "DCA2") +
  theme(legend.justification=c(1,1), legend.position=c(1,1),)

print(dca_plot)

ggsave(filename = "DCAt.pdf",
       dca_plot,
       width = 12, height = 10,
       dpi = 600)
