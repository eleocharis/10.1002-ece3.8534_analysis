# Correlationmatrix and PCA to compare the variables

################################ Load R-packages #######################################

library(ggbiplot)   #library(devtools) install_github("vqv/ggbiplot")
library(vegan)
library(gridExtra)
library(cowplot)
library(corrplot)
library(tidyverse)
################################## Load data sets ########################################
rm(list = ls())
path = "/home/jan/confobi/Data"
setwd(path)

x <- read.csv(sort(list.files(path = path, pattern = glob2rx("all_plot*")), decreasing = T)[1], stringsAsFactors = T)

########################### Load colour and theme styles ################################
source("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plot_style.R", echo = F)
ggbiplot2 <- source("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/ggbiplot_funct.R")


# Save graphics in the folder:
setwd("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plots/")



########### correlation matrix  ################

make_corr <- function(vars, fname_corr){
  vars_cor  <- vars[ , -1]
  cormatrix <- cor(vars_cor, use = "complete.obs")
  
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(cormatrix)
  # head(p.mat[, 1:5])
  
  
  pdf(file = fname_corr)
  corrplot(cormatrix,
            method="color",
            #type = "upper", 
            order = "hclust", hclust.method = "mcquitty",
            p.mat = p.mat, sig.level = 0.1, insig = "blank", 
            addCoef.col = "black", addCoefasPercent = T, number.cex = 0.8,
            tl.col="black", tl.srt=45, tl.cex = 0.9 #Text label color rotation and size
            )
  dev.off()

}



########## PCA ###############

# for the pca

# to join the Forest community again after pca (no categorical vars accepted)
# fct = forest community table

make_pca <- function(vars, legend_position, fname_pca){
  fct <- x %>% dplyr::select(plotID, `Forest community` = phyt_clust4)
  
  vars <- na.omit(vars)
  fct <- vars %>% left_join(fct) %>% dplyr::select(plotID, `Forest community`)
  vars$plotID <- NULL
  pca <- prcomp(vars, center = TRUE, scale. = TRUE)
  
  # plot(pca, type = "l") #Shows ow much of the variance is explained by which PCA-axis. 
  # biplot(pca) shows an old fashion biplot
  
  pca$plotID <- fct$plotID
  pca_env <- as.data.frame(scores(pca, display = "sites"))
  pca_env$plotID <- fct$plotID
  pca_env <- left_join(pca_env, fct)
  
  # a nice script to make beautiful PCA
  
  
  PCAgraph <- ggbiplot(pca, choices = c(1, 2), obs.scale = 1, var.scale = 1, col = regline) + col.theme + 
                  geom_point(data = pca_env, aes(x = PC1, y = PC2, colour = `Forest community`), size = 1) +
                  #geom_text(data = pca_env, aes(x = PC1, y = PC2, label = plotID, colour = `Forest community`), size = 3) +
                  legend_postion; print(PCAgraph)
  
  ggsave(filename = fname_pca,
         plot = PCAgraph,
         width = 5, height = 5,
         dpi = 800)
}

stop()
############## Variable groups ########################
#all <- x %>% dplyr::select(plotID, SR_herb, DLI, DLI_cv, pH, pH_cv, Ca, Mg, P, P_cv,  K, Fe, C, N, ammo.f.all, nitr.f.all, Na, CNratio, CNratio_cv, altitude, aspect, avg_slope, acer_p, beech_p, fir_p, pine_p, spruce_p, BA = basal_A, DBHcv, SSCI, TRI)
all_vars <- x %>% dplyr::select(-phyt_clust4)
#light <- x %>% dplyr::select(plotID, DLI, DLI_cv, BLI, BLI_cv, tB, SR_herb)

################ selections: #########################
# vars is always the selection of variables miced threw a corr plot an a PCA


# fname_corr = "all_vars_corr.pdf"; fname_pca = "all_vars_pca.pdf"
# legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.66, 0.66))
# make_pca(all_vars, legend_position, fname_pca)
# make_corr(all_vars, fname_corr)

fname_corr = "Fig_A1.jpg"; fname_pca = "Fig_A1.jpg"
vars <- x %>% dplyr::select(nitr.f.all, pH, Ca, Mg, DLI, Fe, P, CNratio, K, Na, ammo.f.all)
legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.64, 0.68)); make_pca(vars, legend_position, fname_pca)
make_corr(vars, fname_corr)

fname_corr = "H2_corr.pdf"; fname_pca = "H2_pca.pdf"
vars <- x %>% dplyr::select(plotID, SR_herb, DLI, DLI_cv, pH, pH_cv, P, P_cv, CNratio, CNratio_cv, altitude, basal_A, DBHcv, SSCI, TRI)
legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.64, 0.68)); make_pca(vars, legend_position, fname_pca)
make_corr(vars, fname_corr)
# 
# 
# vars <- x %>% dplyr::select(plotID, SR_herb_agg, DLI, DLI_cv, pH, pH_cv, CNratio, CNratio_cv, altitude)
# fname_corr = "H3_corr.pdf"; fname_pca = "H3_pca.pdf"
# legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.02, 0.02)); make_pca(vars, legend_position, fname_pca)
# make_corr(vars, fname_corr)

# vars <- trees; fname_corr = "tree_corr.pdf"; fname_pca = "tree_pca.pdf"
# legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.02, 0.02)); make_pca(vars, legend_position, fname_pca)
# make_corr(vars, fname_corr)
# 
# 
# vars <- htgnty; fname_corr = "heterogeneity_variables_corr.pdf"; fname_pca = "heterogeneity_variables_pca.pdf"
# legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.02, 0.02)); make_pca(vars, legend_position, fname_pca)
# make_corr(vars, fname_corr)
# 
# 
# vars <- responses; fname_corr = "responses_corr.pdf"; fname_pca = "responses_pca.pdf"
# legend_postion <- theme(legend.justification = c(0, 0), legend.position = c(0.02, 0.02)); make_pca(vars, legend_position, fname_pca)
# make_corr(vars, fname_corr)











# quickly plot the pca axis
#pca_env<- bind_cols(pca_env, dplyr::select(vars, SR_herb_agg))
#pca_env %>% ggplot(aes(PC3, SR_herb_agg)) + geom_point(col=dots) + geom_smooth(method = "loess", col=regline,alpha=0.15)
