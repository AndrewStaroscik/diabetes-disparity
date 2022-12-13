# boxploty by urban/rural only

# clear variables and data frames 
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off()


library(tidyverse) # needed for ggplot
library(cowplot) # for half_open theme


source('./loadData.r')
source('./colList.r')

# get dataframe
df <- getDataFrame()

# get list of custom colors
my_colors <- colList()

plot <- ggplot(df, aes(x = urban, y=v_diab_allR)) +
  theme_half_open() +
  geom_boxplot()

plot
