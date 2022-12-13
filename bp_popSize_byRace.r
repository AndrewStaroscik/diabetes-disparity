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

 df <- df %>%
  mutate(dB_diab_black = case_when(
    d_diab_black == '11-499' ~ 'a',
    d_diab_black == '500-999' ~ 'b',
    d_diab_black == '1,000-4,999' ~ 'c',
    d_diab_black == '5,000-9,999' ~ 'd',
    d_diab_black == '10,000+' ~ 'e'
  ))

  df <- df %>%
  mutate(dB_diab_white = case_when(
    d_diab_white == '11-499' ~ 'a',
    d_diab_white == '500-999' ~ 'b',
    d_diab_white == '1,000-4,999' ~ 'c',
    d_diab_white == '5,000-9,999' ~ 'd',
    d_diab_white == '10,000+' ~ 'e'
  ))  


df_diab_race <- melt(df, id.vars= c(
  'fips', 
  'county_info', 
  'urban', 
  'd_diab_white', 
  'd_diab_black',
  'dB_diab_white', 
  'dB_diab_black'
  ),
  measure.vars = c('v_diab_black', 'v_diab_white'))

  # add another column to explicitly declare the right population size:
df_diab_race <- df_diab_race %>%
  mutate(pop_size = case_when(
    variable == 'v_diab_black' ~ df_diab_race$dB_diab_black,
    variable == 'v_diab_white' ~ df_diab_race$dB_diab_white
  ))

# add another column to get the outlier colors correct:
df_diab_race <- df_diab_race %>%
  mutate(labcol = case_when(
    variable == 'v_diab_black' ~ my_colors$dkOr,
    variable == 'v_diab_white' ~ my_colors$dkBl
  )) 

# work on labeling
count_by_pop_by_race <-df_diab_race %>%
  group_by(variable,pop_size) %>%
  summarize(
    count= sum(!is.na(prev))) 

count_by_pop_by_race  

# manual annotations for the count labels
count_annotation <- data.frame(
  x = c(0.875, 1.25, 1.875, 2.25, 2.875, 3.25, 3.875, 4.25, 4.875, 5.25),
  y = c(43.8, 32.3, 43.3, 33.3, 42.3, 32.8, 39.8, 31.3, 40.8, 29.3),
  label = c(
    count_by_pop_by_race$count[1], 
    count_by_pop_by_race$count[7], 
    count_by_pop_by_race$count[2], 
    count_by_pop_by_race$count[8], 
    count_by_pop_by_race$count[3], 
    count_by_pop_by_race$count[9], 
    count_by_pop_by_race$count[4], 
    count_by_pop_by_race$count[10], 
    count_by_pop_by_race$count[5],
    count_by_pop_by_race$count[11] 
  ),
  col = c(my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl)
  )

plot <- ggplot(
  na.omit(df_diab_race), 
  aes(
    x = pop_size,
    y= value,
    fill = variable,
    color = variable 
    # fill = ifelse(variable == 'd_diab_black', dB_diab_black, dB_diab_white)
  ),
  alpha = 0.4) +
  theme_half_open() +
  theme(
    axis.line = element_line(color = my_colors$dkGr),
    axis.title = element_text(color = my_colors$dkGr),
    axis.ticks = element_line(color = my_colors$dkGr),
    axis.text = element_text(color = my_colors$dkGr),
    legend.position = c(0.6, 0.8),
    legend.title = element_blank(),
    legend.key.size = unit(2, 'cm')

  ) +
  geom_boxplot(
    
  ) +
  xlab("Size of Medicare Population") +
  ylab("Diabetes Prevalence (%)") +
  scale_x_discrete(labels=c('11-499', '500-999', '1,000-4,999','5,000-9,999', '10,000+')) +
  scale_color_manual(values = c(
    my_colors$dkOr, 
    my_colors$dkBl
  ),
  labels = c('', '')) + 
  scale_fill_manual(values = c(
    my_colors$or, 
    my_colors$bl
  ),
  labels = c('', '')) +
  annotate("text",
    x = count_annotation$x,
    y = count_annotation$y,
    label = paste('n =',count_annotation$label,sep=' '),
    angle=90,
    color = count_annotation$col
  ) +
  annotate('text', 
    x = 4.35, 
    y = 64.3,
    label = "Black",
    size = 5.5, 
    color = my_colors$dkOr
    ) +
  annotate('text', 
    x = 4.35, 
    y = 56.1,
    label = "White",
    size = 5.5, 
    color = my_colors$dkBl
    )

# the annotations here are fragile: based on scaling in RStudio
plot
