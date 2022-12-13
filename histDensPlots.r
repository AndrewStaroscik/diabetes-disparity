# First plot showing histogram and density plots for diabetes by race (white vs black)

# clear variables and data frames 
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off()


library(tidyverse) # needed for left join
library(cowplot) # for plot_grid


source('./loadData.r')
source('./colList.r')

# get dataframe
df <- getDataFrame()

# get list of custom colors
my_colors <- colList()

h02xlim = 69
h02denslim = 0.091
# diabetes by race
h02ad <- ggplot(df) +
  theme_classic() +
  theme(
    axis.line=element_line(color=my_colors$dkGr),
    axis.title=element_text(color=my_colors$dkGr),
    axis.ticks=element_line(color=my_colors$dkGr),
    axis.text=element_text(color=my_colors$dkGr)) +
  geom_histogram(aes(x=v_diab_white), binwidth=1, fill = my_colors$bl, alpha = 0.4, na.rm=TRUE) +
  geom_histogram(aes(x=v_diab_black), binwidth=1, fill = my_colors$or, alpha = 0.4, na.rm=TRUE) +
  xlab("Diabetes Prevalence (% of population)") +
  ylab("Count") +
  ylim(0, 310) +
  xlim(0, h02xlim) +
  annotate("rect", xmin = 50, xmax = 58, ymin = 255, ymax = 275,
    fill = my_colors$or, alpha = 0.4) +
  annotate("rect", xmin = 50, xmax = 58, ymin = 230, ymax = 250,
    fill = my_colors$bl, alpha = 0.4) +
  annotate("text", x=60, y=240,
    color = my_colors$dkBl,
    label = 'White', hjust = 0) +
  annotate("text", x=60, y=265,
    color = my_colors$dkOr,
    label = 'Black', hjust = 0)

h02bd <- ggplot(df) +
  theme_classic() +
  theme(
    axis.line=element_line(color=my_colors$dkGr),
    axis.title=element_text(color=my_colors$dkGr),
    axis.ticks=element_line(color=my_colors$dkGr),
    axis.text=element_text(color=my_colors$dkGr)) +
  geom_density(aes(x=v_diab_white), color=my_colors$dkBl, fill=my_colors$bl, na.rm=TRUE, alpha = 0.4) +
  geom_density(aes(x=v_diab_black), color=my_colors$dkOr, fill=my_colors$or, na.rm=TRUE, alpha = 0.4) +
  xlab("Diabetes Prevalence (% of population)") +
  ylim(0, h02denslim) +
  xlim(0, h02xlim) +
  ylab("Density") 

plot_grid(h02ad, h02bd, align='v')