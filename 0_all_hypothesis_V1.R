########  Canopy complexity measures in comparison to resource heterogeneity  #########
#                                                                                      #
# script written by Jan Helbach                                                        #
# rewritten and condensed in November 2021                                             #
#                                                                                      #
# Analysis plan:                                                                       #
# Here I analysed the effects of three sturctural complexity varables                  #
# in reation to resource heterogeneity                                                 #
#                                                                                      #
# Structural comlexity variables are:                                                  #
#  - Standard Deviation of DBH (DBH_cv)                                                #
#  - Stand Structural Complexity index (SSCI)                                          #
#  - Terrainian ruggedness index (TRI)                                                 #
#  - Basal Area (Stand density)                                                        #
#                                                                                      #
# Resource heterogeneity variables are:                                                #
#  - DLI_cv                                                                            #
#  - pH_cv                                                                             #
#  - CNratio_cv                                                                        #
#  - P_cv                                                                              #
#                                                                                      #
# Covariables in the models are:                                                       #
#  - DLI, pH, phyt_clust4, P, CNratio, altitude, acer_p                                #
#                                                                                      #
#   Number of Models: 3 x 4 = 12                                                       #
#                                                                                      #
###################################### . ###############################################

#rm(list = ls())
setwd("/home/jan/confobi/Analysis/1_struct_hetero_div_V2")
# load the auto model function
#source("/home/jan/prgwerk/automodel/ms.R")
source("automodel.R")

# load file
x <- read.csv(paste0("../../Data/", sort(list.files(path = "../../Data", pattern = "all_plot"), decreasing = T)[1]), stringsAsFactors = T)
setwd("models")

#x %>% ggplot(aes(X,Y)) + geom_point(aes(size = CNratio_cv, col = SR_herb))
#x$CNratio_cv
stop()
################ H1 Effect of structural complexity on Resource Heterogeneity ###################

######### . DLI_cv #################

DLIcv_BA <- modeler(df = x, 
                    fm_term = "DLI_cv ~ basal_A * DLI", 
                    model_type = "lm",
                    predNo = 2,
                    save = T,
                    filename = "DLIcv_BA")

DLIcv_DBHcv <- modeler(df = x, 
                       fm_term = "DLI_cv ~ DBHcv * DLI", 
                       model_type = "lm",
                       predNo = 2, 
                       save = T,
                       filename = "DLIcv_DBHcv")

DLIcv_SSCI <- modeler(df = x, 
                      fm_term = "DLI_cv ~ SSCI * DLI", 
                      model_type = "lm", 
                      predNo = 2,
                      save = T,
                      filename = "DLIcv_SSCI")

DLIcv_TRI <- modeler(df = x, 
                     fm_term = "DLI_cv ~ TRI * DLI", 
                     model_type = "lm", 
                     predNo = 2,
                     filename = "DLIcv_TRI")


######### . pH_cv #################

pHcv_BA <- modeler(df = x, 
                   fm_term = "pH_cv ~ basal_A * pH * DLI * DLI_cv + (1|phyt_clust4)", 
                   model_type = "glmer_logn", 
                   predNo = 2,
                   filename = "pHcv_BA")

pHcv_DBHcv <- modeler(df = x, 
                      fm_term = "pH_cv ~ DBHcv * pH * DLI * DLI_cv + (1|phyt_clust4)", 
                      model_type = "glmer_logn", 
                      predNo = 2,
                      filename = "pHcv_DBHcv")

pHcv_SSCI <- modeler(df = x, 
                     fm_term = "pH_cv ~ SSCI * pH * DLI * DLI_cv + (1|phyt_clust4)", 
                     model_type = "glmer_logn", 
                     predNo = 2,
                     filename = "pHcv_SSCI")

pHcv_TRI <- modeler(df = x, 
                    fm_term = "pH_cv ~ TRI * pH * DLI * DLI_cv + (1|phyt_clust4)", 
                    model_type = "glmer_logn", 
                    predNo = 2,
                    filename = "pHcv_TRI")


######### . C:Nratio_cv #################

CNcv_BA <- modeler(df = x,
                        fm_term = "CNratio_cv ~ basal_A * DLI * DLI_cv",
                        model_type = "glm_logn",
                        predNo = 2,
                        save = F,
                        filename = "CNcv_BA")

CNcv_DBHcv <- modeler(df = x,
                           fm_term = "CNratio_cv ~ DBHcv * CNratio * DLI * DLI_cv",
                           model_type = "glm_logn",
                           predNo = 2,
                            save = F,
                           filename = "CNcv_DBHcv")

CNcv_SSCI <- modeler(df = x,
                          fm_term = "CNratio_cv ~ SSCI * CNratio * DLI * DLI_cv",
                          model_type = "glm_logn",
                          predNo = 2,
                          save = F,
                          filename = "CNcv_SSCI")

CNcv_TRI <- modeler(df = x, 
                         fm_term = "CNratio_cv ~ TRI * CNratio * DLI * DLI_cv",
                         model_type = "glm_logn",
                         predNo = 2,
                          save = F,
                         filename = "CNcv_TRI")


######### . P_cv #################

Pcv_BA <- modeler(df = x, 
                  fm_term = "P_cv ~ basal_A * P * DLI * DLI_cv", 
                  model_type = "glm_logn", 
                  predNo = 2,
                  filename = "Pcv_BA")

Pcv_DBHcv <- modeler(df = x, 
                     fm_term = "P_cv ~ DBHcv * P * DLI * DLI_cv", 
                     model_type = "glm_logn", 
                     predNo = 2,
                     filename = "Pcv_DBHcv")

Pcv_SSCI <- modeler(df = x, 
                    fm_term = "P_cv ~ SSCI * P * DLI * DLI_cv", 
                    model_type = "glm_logn", 
                    predNo = 2,
                    filename = "Pcv_SSCI")

Pcv_TRI <- modeler(df = x, 
                   fm_term = "P_cv ~ TRI * P * DLI * DLI_cv", 
                   model_type = "glm_logn", 
                   predNo = 2,
                   filename = "Pcv_TRI")




################ H2 Influences of Resource Heterogeneity on species richness ####################

SR_Het1 <- modeler(df = x, 
                   fm_term = "SR_herb_agg ~ DLI_cv * DLI * pH_cv * pH + (1|phyt_clust4)", 
                   model_type = "glmer.nb", 
                   filename = "SR_Het1")

SR_Het2 <- modeler(df = x, 
                   fm_term = "SR_herb_agg ~ DLI_cv * DLI * CNratio_cv * CNratio + (1|phyt_clust4)", 
                   model_type = "glmer.nb", 
                   filename = "SR_Het2")

SR_Het3 <- modeler(df = x,
                   fm_term = "SR_herb_agg ~ DLI_cv + DLI + CNratio_cv + DLI_cv:CNratio_cv + (1|phyt_clust4)",
                   model_type = "glmer.nb",
                   save = T,
                   filename = "SR_Het3")





################ H3 Influences of Structural Complexity on species richness (kicked out of the manuscript) ####################


SR_BA <- modeler(df = x,
                 fm_term = "SR_herb_agg ~ basal_A + (1|phyt_clust4)",
                 model_type = "glmer.nb", 
                 filename = "SR_BA")

SR_DBHcv <- modeler(df = x,
                    fm_term = "SR_herb_agg ~ DBHcv + (1|phyt_clust4)", 
                    model_type = "glmer.nb", 
                    filename = "SR_DBHcv")

SR_SSCI <- modeler(df = x,
                   fm_term = "SR_herb_agg ~ SSCI  + (1|phyt_clust4)", 
                   model_type = "glmer.nb", 
                   filename = "SR_SSCI")

SR_TRI <- modeler(df = x,
                  fm_term = "SR_herb_agg ~ TRI + (1|phyt_clust4)", 
                  model_type = "glmer.nb", 
                  filename = "SR_TRI")




############# Side analysis ##################

####### . Relationship herb richness and herb cover #######
x1<- x %>% dplyr::select(hCover, SR_herb_agg)
x1 <- x1[complete.cases(x1), ]
m3<-glmer.nb(SR_herb_agg ~ hCover + (1|phyt_clust4),x)
m4<-lm(SR_herb_agg ~ poly(hCover, 3),x1)
summary(m3)
AIC(m3, m4)
source("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/plot_style.R", echo = F)

x %>% ggplot(aes(hCover, SR_herb_agg))+ geom_point(size = 7, col = "#f0c203", alpha = 0.7) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x,3)", col = "#008080", fill = "#008080", size = 2, alpha = 0.15)

x %>% ggplot(aes(hCover, SR_herb_agg))+ geom_point(size = 7, col = "#f0c203", alpha = 0.7) + 
  geom_smooth(method = "lm", formula = "y ~ x", col = "#008080", fill = "#008080", size = 2, alpha = 0.15)


m11 <- lm(hCover ~ log(DLI), x)
summary(m11)
m12 <- lm(hCover ~ CNratio, x)
summary(m12)
m13 <- lm(hCover ~ pH, x)
summary(m13)
###### . Relationship of dead wood and CN heterogeneity ########
library(readxl)
y<- read_excel("/home/jan/confobi/Data/Plot_overview_dataset.xlsx")
x <- x %>% left_join(dplyr::select(y, plotID = Plot_no, lying_dw_volume))



m5<-lm(CNratio ~ n_standDW, x)
m6<-lm(CNratio ~ vol_standDW, x)
m7<-lm(DLI_cv ~ n_standDW, x)

m8 <- glm(CNratio ~ lying_dw_volume, family = "gaussian"(link='log'), x)
summary(m8)

x %>% ggplot(aes(DLI, hCover))+ 
  geom_point(size = 5, col = "#f0c253", alpha = 0.7) + 
  geom_smooth(method = "glm", formula = "y ~ log(x)", col = "#008080", fill = "#008080", size = 2, alpha = 0.15)


#method.args = list(family = "gaussian"(link='log')),


# library(car)
# vif(fm)
# library(effects)
# m <- glmer.nb(SR_herb_agg ~ DLI_cv + DLI + CNratio * CNratio_cv + (1|phyt_clust4), data = df_sc)
# summary(m)
# plot(bm)
# plot(allEffects(m))



###### . Relationship of TRI/DBHcv with tree-biomass ########


biom_struc <- modeler(x,
                      fm_term = "basal_A ~ DBHcv",
                      model_type = "lm")

biom_struc <- modeler(x,
                      fm_term = "basal_A ~ DBHcv",
                      model_type = "lm")