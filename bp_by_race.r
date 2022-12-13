# a boxplot showing the difference in summary and spread between black and white populations

# clear variables and data frames 
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off()


library(tidyverse) # needed for ggplot
library(cowplot) # for half_open theme
library(reshape2)
library(ggrepel)


source('./loadData.r')
source('./colList.r')

# get dataframe
df <- getDataFrame()

# get list of custom colors
my_colors <- colList()

# # get rid of zeros
# df <- filter(df, 
#     v_diab_white > 0, 
#     v_diab_black > 0
# )

# #drop NAs
# df[!is.na(df$v_diab_white),]
# df[!is.na(df$v_diab_black),]

df_diab_race <- melt(df, id.vars= c('fips', 'county_info', 'd_diab_white', 'd_diab_black'),
  measure.vars = c('v_diab_black', 'v_diab_white'))

# count by group for labelson the boxplot
diab_count_by_race <-df_diab_race %>%
  group_by(variable) %>%
  summarize(sumVals = sum(!is.na(value)))

paste("Total count of counties reporting diabetes prevelance for blacks and whites")
diab_count_by_race

# anotations for the boxplot
diab_by_race_box_annot <- data.frame(
  x = c(1.2, 2.2),
  y = c(42,30),
  label = c(diab_count_by_race$sumVals[1], diab_count_by_race$sumVals[2])
)

find_outlier <- function(x) {
  return(x > quantile(x, .75, na.rm=TRUE) + 3*IQR(x, na.rm=TRUE))
}

#add new column to data frame that indicates if each observation is an outlier
df_diab_race <- df_diab_race %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(find_outlier(value), paste(county_info, '\n', ifelse(variable == 'v_diab_black', d_diab_black, d_diab_white)), NA))

# add another column to get the outlier colors correct:
df_diab_race <- df_diab_race %>%
  mutate(labcol = case_when(
    variable == 'v_diab_black' ~ my_colors$dkOr,
    variable == 'v_diab_white' ~ my_colors$dkBl
  ))

bp1 <- ggplot(df_diab_race, aes(x=variable, y=value)) +
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
    size=3.5,
    label.size = NA,
    fill = "white",
    hjust=0.5,
    color = df_diab_race$labcol,
    nudge_x = 0.25)

bp1

