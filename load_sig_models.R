######################### Load Models to play #########################################
#                                                                                      #
#     In this script I load all significant models saved for this manuscript.          #
#                                                                                      #
######################################### . ############################################

library(stringr)

modelpath <- "/home/jan/confobi/Analysis/1_struct_hetero_div_V2/models/"
fname <- list.files(path = modelpath, pattern = glob2rx("*.rds"))
for (i in fname) {
  a <- readRDS(file = paste0(modelpath, i))
  assign(str_sub(i, start = 1L, end = -5L), a)
}
rm(a, i, fname)

