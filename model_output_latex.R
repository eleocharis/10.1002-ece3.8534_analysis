######################### Print Models for table output #########################################
#                                                                                      #
#     In this script I load all significant models saved for this manuscript.          #
#                                                                                      #
######################################### . ############################################

rm(list = ls())
library(tidyverse)
library(rsq)
library(xtable)
#library(report) # library(remotes); remotes::install_github("easystats/report") 
modelpath <- "/home/jan/confobi/Analysis/1_struct_hetero_div_V2/models/"

###############################   Load models    ########################################
source("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/load_sig_models.R", echo = F)
rm(modelpath)
setwd("/home/jan/confobi/Analysis/1_struct_hetero_div_V2/tables")

stop()
options("digits")[[4]]
# plot the lm and glm models mixed effect models are not supported.
models <- ls()
#i=models[length(models)-6]
for(i in models) {
  a <- summary(get(i)$bm)
  write_file(x = print(xtable(x = a$coefficients, digits = 4), 
                       include.rownames = T,
                       hline.after = c(-1,-1,0, nrow(a$coefficients), nrow(a$coefficients))),
             file = paste0(i, ".tex"))
}

textabs <- list.files()
for (i in textabs) {
  a <- read_lines(i)
  a <- a[5:(length(a)-1)]
  write_lines(a, file = i, append = F)
}

stop()
attributes(a$coefficients)
