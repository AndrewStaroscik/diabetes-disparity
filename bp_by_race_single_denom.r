# a boxplot showing the difference in summary and spread between black and white populations
# with a filter that only shows one population_denominator

# clear variables and data frames 
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off()


library(tidyverse) # needed for ggplot
library(cowplot) # for half_open theme
library(ggrepel)


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


# get rid of the work county in the county column
df$county_short <- gsub(" County", "", df$county)

#make a new column containing a useful lable that has both county and state names
df$county_info = paste(df$county_short,', ',df$code, sep="")

df_white_filtered <- df
df_black_filtered <- df

# #drop NAs
df_white_filtered <- df_white_filtered[!is.na(df$v_diab_white),]
df_black_filtered <- df_black_filtered[!is.na(df$v_diab_black),]

# add a race column for indexing
df_white_filtered$race <- 'w'
df_black_filtered$race <- 'b'

# make the column names match

df_white_sm <- df_white_filtered[,c('race', 'd_diab_white', 'dB_diab_white','v_diab_white', 'county_info')]
df_black_sm <- df_black_filtered[,c('race', 'd_diab_black', 'dB_diab_black', 'v_diab_black', 'county_info')]

colnames(df_white_sm)[2] ="popSize"
colnames(df_black_sm)[2] ="popSize"

colnames(df_white_sm)[3] ="sortVar"
colnames(df_black_sm)[3] ="sortVar"

colnames(df_white_sm)[4] ="prev"
colnames(df_black_sm)[4] ="prev"


df_diab_race <- rbind(df_white_sm, df_black_sm)

filterVal <- '10,000+'

#filter for one popSize
df_diab_race <- filter(df_diab_race, popSize == filterVal)

# count by group for labelson the boxplot
diab_count_by_race <-df_diab_race %>%
  group_by(race) %>%
  summarize(sumVals = sum(!is.na(prev)))

paste("Total count of counties reporting diabetes prevelance for blacks and whites")
diab_count_by_race


# anotations for the boxplot
diab_by_race_box_annot <- data.frame(
  x = c(1.2, 2.2),
  y = c(42,30),
  label = c(diab_count_by_race$sumVals[1], diab_count_by_race$sumVals[2])
)

find_outlier <- function(x) {
  return(x > quantile(x, .75, na.rm=TRUE) + 1.5 * IQR(x, na.rm=TRUE) | x < quantile(x, .25, na.rm=TRUE) - 1.5 * IQR(x, na.rm=TRUE) )
}

#add new column to data frame that indicates if each observation is an outlier
df_diab_race <- df_diab_race %>%
  group_by(race) %>%
  mutate(
    outlier = ifelse(find_outlier(prev), paste(county_info), NA))

# add another column to get the outlier colors correct:
df_diab_race <- df_diab_race %>%
  mutate(labcol = case_when(
    race == 'b' ~ my_colors$dkOr,
    race == 'w' ~ my_colors$dkBl
  ))




bp1 <- ggplot(df_diab_race, aes(x=race, y=prev)) +
  theme_half_open() +
  theme(
    axis.line = element_line(color = my_colors$dkGr),
    axis.title = element_text(color = my_colors$dkGr),
    axis.ticks = element_line(color = my_colors$dkGr),
    axis.text = element_text(color = my_colors$dkGr),
    legend.position = 'none'
  ) +
  geom_boxplot(color = c(my_colors$dkOr, my_colors$dkBl), fill = c(my_colors$or, my_colors$bl), alpha=0.4) +
  geom_text(data=diab_by_race_box_annot, 
    aes(x=x, y=y, label=paste('n = ',label)),
    color = c(my_colors$dkOr, my_colors$dkBl),
    size=5) +
  xlab("Race") +
  scale_x_discrete(labels=c('Black', "White")) +
  ylab("Diabetes Prevalence (%)") +
  geom_label_repel(aes(label=outlier), 
    na.rm=TRUE,
    size=3,
    label.size = NA,
    fill = "white",
    hjust=0.5,
    color = df_diab_race$labcol,
    nudge_x = 0.25) + 
  annotate("text",
    x = 0.5,
    y = 15,
    label = paste('population size:',filterVal,sep=' '),
    size = 6,
    angle=0,
    hjust = 0,
    color = my_colors$dkGr
  )

bp1

