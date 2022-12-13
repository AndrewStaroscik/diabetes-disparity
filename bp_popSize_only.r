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
# df <- df %>% 
#   mutate(u = case_when(
#     urban == "Urban" ~ 'u',
#     urban == 'Rural' ~ 'r'
#   ))

# need a new column for denominator to sort plot properly
df <- df %>%
  mutate(dB_diab_allR = case_when(
    d_diab_allR == '11-499' ~ 'a',
    d_diab_allR == '500-999' ~ 'b',
    d_diab_allR == '1,000-4,999' ~ 'c',
    d_diab_allR == '5,000-9,999' ~ 'd',
    d_diab_allR == '10,000+' ~ 'e'
  ))




# df_diab_race <- melt(df, id.vars= c('fips', 'county_info', 'urban'),
#   measure.vars = c('v_diab_black', 'v_diab_white'))

plot <- ggplot(df, aes(x = dB_diab_allR, y=v_diab_allR)) +
  theme_half_open() +
  geom_boxplot()

plot
