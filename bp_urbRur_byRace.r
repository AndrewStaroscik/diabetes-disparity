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

df_diab_race <- melt(df, id.vars= c('fips', 'county_info', 'urban'),
  measure.vars = c('v_diab_black', 'v_diab_white'))

plot <- ggplot(df_diab_race, aes(x = variable, y = value, fill = urban)) +
  theme_half_open() +
  geom_boxplot()

plot

