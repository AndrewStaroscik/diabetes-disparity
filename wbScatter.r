# x y scatter showing spread is higher for blacks than whites

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

# get rid of zeros
dfScatter <- filter(df, 
    v_diab_white > 0, 
    v_diab_black > 0
)

#drop NAs
dfScatter[!is.na(dfScatter$v_diab_white),]
dfScatter[!is.na(dfScatter$v_diab_black),]

#linear regressions
regression_white_vs_black<-lm(formula = v_diab_black ~ v_diab_white,
              data=dfScatter)

print(summary(regression_white_vs_black))

coeff_wvb <- coefficients(regression_white_vs_black)

regression_rsq <- summary(regression_white_vs_black)$r.squared
regression_rsq

regression_intercept <- coeff_wvb[1]
regression_intercept

regression_slope <- coeff_wvb[2]
regression_slope

min_x = min(dfScatter$v_diab_white)

spxx <- ggplot(dfScatter, aes(x = v_diab_white, y = v_diab_black)) + 
  geom_point(color = my_colors$dkBl, alpha = 0.25) + 
  #geom_abline(intercept = regression_intercept, slope = regression_slope, color=my_colors$dkBl, 
  #            linetype="dashed", size=0.5, na.rm=TRUE) +
  stat_smooth(method = "lm", se = FALSE, color = my_colors$dkB) +
  geom_segment(aes(
    x = 0, 
    xend = min_x, 
    y = regression_intercept + regression_slope * 0, 
    yend = regression_intercept + regression_slope * min_x
    ),
    linetype = 'dashed',
    size = 0.5, 
    color = my_colors$bl
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75)) +
  theme_half_open() +
  theme(
    axis.line = element_line(color = my_colors$dkGr),
    axis.title = element_text(color = my_colors$dkGr),
    axis.ticks = element_line(color = my_colors$dkGr),
    axis.text = element_text(color = my_colors$dkGr)
  ) +
  xlab('Diabetes Prevalence in White Populaiton (%)') +
  ylab('Diabetes Prevalence in Black Populaiton (%)') +
  annotate(
    geom = 'rect', 
    xmax = max(dfScatter$v_diab_white), 
    xmin = min(dfScatter$v_diab_white), 
    ymax = max(dfScatter$v_diab_black), 
    ymin = min(dfScatter$v_diab_black),
    alpha = .1,
    linetype = 'dotted', 
    size = .5, 
    color = my_colors$bl_faded,   
    fill = NA 
  )  +
  annotate("label", 
    x = 45,
    y = 55,
    size = 5.5,
    label = paste('y = ',round(regression_slope, 2),'x + ',round(regression_intercept, 2), sep=''),
    color=my_colors$dkBl,
    label.size = NA, 
    fill = 'white'
    ) +
  annotate("label", 
    x = 45,
    y = 52,
    size = 5.5,
    label = paste('R^2 == ',round(regression_rsq, 2), sep=''),
    parse=TRUE,
    color=my_colors$dkBl,
    label.size = NA,  
    fill = 'white'
    )



spxx

# in case the values are useful for the text. 
paste('maximum black value:', max(dfScatter$v_diab_black))
paste('minimum black value:', min(dfScatter$v_diab_black))
paste('maximum white value:', max(dfScatter$v_diab_white))
paste('minimum white value:', min(min_x))

