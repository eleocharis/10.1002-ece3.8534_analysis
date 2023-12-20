##########################################  Plot Style  #######################################
#                                                                                             #
# Script to set the plot style globally for all plots made for the paper:                     #
# "Structural complexity influences Light heterogeneity and Plant Diversity"                  #
#                                                                                             #
###############################################################################################




########################################## Set graph theme ####################################

theme_set(
  theme_bw() +
  theme(
    panel.background = element_rect(fill = NA, colour = "black"),
    #strip.background = element_rect(fill = "white", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
    panel.grid.major = element_blank(),  # remove major-grid
    panel.grid.minor = element_blank(),  # remove minor-grid
    axis.ticks = element_line(color = "black", size = 0.75),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.background = element_rect(fill = NA),
    strip.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF") # size=1.5, linetype="solid", face = "bold.italic" for facet background
    )
  )

########################################## Colour Palettes #####################################

#colour.palette = c("#00008b", "#9cc600", "#ffa500", "#c0003a", "#00aedb", "#a200ff") #the old palette
colour.palette = c("#00008b", "#009393", "#ffa500", "#c0003a", "#65fc1e", "#a200ff")
col.theme <- scale_colour_manual(values = colour.palette)
col.fill <- scale_fill_manual(values = colour.palette)

#col.gradient <- c("#c3a803", "#7d9404", "#496901")
#col.gradient <- c("#607a00", "#9cc600", "#b0e000")
col.gradient <- c("#3469d3", "#9cc600", "#fa9632")
col.gradient.f <- scale_fill_gradientn(colors = col.gradient)
col.gradient.c <- scale_colour_gradientn(colors = col.gradient)
col.theme.g <- scale_colour_manual(values = col.gradient)
col.fill.g <- scale_fill_manual(values = col.gradient)

dots <- "#00008b"
regline <- "#9cc600"

# "#d60a48" red 
#607a00