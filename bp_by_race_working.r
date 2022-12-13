# a boxplot showing the difference in summary and spread between black and white populations

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

# add sortable popSize
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

df_white_filtered <- df
df_black_filtered <- df

# #drop NAs
df_white_filtered <- df_white_filtered[!is.na(df$v_diab_white),]
df_black_filtered <- df_black_filtered[!is.na(df$v_diab_black),]

# add a race column for indexing
df_white_filtered$race <- 'w'
df_black_filtered$race <- 'b'

# make the column names match

df_white_sm <- df_white_filtered[,c('race', 'd_diab_white', 'dB_diab_white','v_diab_white')]
df_black_sm <- df_black_filtered[,c('race', 'd_diab_black', 'dB_diab_black', 'v_diab_black')]

colnames(df_white_sm)[2] ="popSize"
colnames(df_black_sm)[2] ="popSize"

colnames(df_white_sm)[3] ="sortVar"
colnames(df_black_sm)[3] ="sortVar"

colnames(df_white_sm)[4] ="prev"
colnames(df_black_sm)[4] ="prev"


df_diab_race <- rbind(df_white_sm, df_black_sm)

# work on labeling
count_by_pop_by_race <-df_diab_race %>%
  group_by(race,popSize) %>%
  summarize(
    count= sum(!is.na(prev)))    

count_by_pop_by_race  


# manual annotations for the count labels
count_annotation <- data.frame(
  x = c(0.9, 1.28, 1.9, 2.28, 2.9, 3.28, 3.9, 4.28, 4.9, 5.28),
  y = c(40.5, 27.5, 40.5, 28.5, 39.5, 29.5, 37.5, 28.5, 38.5, 26.5),
  label = c(
    count_by_pop_by_race$count[3], 
    count_by_pop_by_race$count[8], 
    count_by_pop_by_race$count[5], 
    count_by_pop_by_race$count[10], 
    count_by_pop_by_race$count[1], 
    count_by_pop_by_race$count[6], 
    count_by_pop_by_race$count[4], 
    count_by_pop_by_race$count[9], 
    count_by_pop_by_race$count[2],
    count_by_pop_by_race$count[7] 
  ),
  col = c(my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl,my_colors$dkOr,my_colors$dkBl)
  )

  # grouped boxplot
plot <- ggplot(df_diab_race, 
  aes(
    x = sortVar, 
    y = prev, 
    fill = race,
    color = race),
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
    geom_boxplot() +
  xlab("Size of Medicare Population") +
  ylab("Diabetes Prevalence (%)") +
  scale_x_discrete(labels=c('11-499', '500-999', '1,000-4,999','5,000-9,999', '10,000+')) +
  scale_color_manual(values = c(
    alpha(my_colors$dkOr, 0.5), 
    alpha(my_colors$dkBl, 0.5)
  ),
  labels = c('', '')) + 
  scale_fill_manual(values = c(
    alpha(my_colors$or, 0.4), 
    alpha(my_colors$bl,0.4)
  ),
  labels = c('', '')) +
  annotate("text",
    x = count_annotation$x,
    y = count_annotation$y,
    label = paste('n =',count_annotation$label,sep=' '),
    angle=90,
    hjust = 0,
    color = count_annotation$col
  ) +
  annotate('text', 
    x = 4.35, 
    y = 66,
    label = "Black",
    size = 5.5, 
    color = my_colors$dkOr
    ) +
  annotate('text', 
    x = 4.35, 
    y = 53.5,
    label = "White",
    size = 5.5, 
    color = my_colors$dkBl
    )

plot    