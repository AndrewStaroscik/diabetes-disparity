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

# make some extra columns to organize the data by
# add another urban rural label to avoid conflict
df <- df %>% 
  mutate(u = case_when(
    urban == "Urban" ~ 'u',
    urban == 'Rural' ~ 'r'
  ))

# need new population size labels to properly sort boxplots
df <- df %>%
  mutate(dB_diab_allR = case_when(
    d_diab_allR == '11-499' ~ 1,
    d_diab_allR == '500-999' ~ 2,
    d_diab_allR == '1,000-4,999' ~ 3,
    d_diab_allR == '5,000-9,999' ~ 4,
    d_diab_allR == '10,000+' ~ 5
  ))

  df <- df %>%
  mutate(dB_diab_black = case_when(
    d_diab_black == '11-499' ~ 1,
    d_diab_black == '500-999' ~ 2,
    d_diab_black == '1,000-4,999' ~ 3,
    d_diab_black == '5,000-9,999' ~ 4,
    d_diab_black == '10,000+' ~ 5
  ))

  df <- df %>%
  mutate(dB_diab_white = case_when(
    d_diab_white == '11-499' ~ 1,
    d_diab_white == '500-999' ~ 2,
    d_diab_white == '1,000-4,999' ~ 3,
    d_diab_white == '5,000-9,999' ~ 4,
    d_diab_white == '10,000+' ~ 5
  ))
