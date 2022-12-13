# Bayesian mean comparision by race


# clear variables and data frames 
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off()


library(tidyverse) # needed for ggplot
library(cowplot) # for half_open theme
library(rethinking) # for statistical analysis


source('./loadData.r')
source('./colList.r')

# get dataframe
df <- getDataFrame()

# get list of custom colors
my_colors <- colList()

# get rid of zeros
df <- filter(df, 
    v_diab_white > 0, 
    v_diab_black > 0
)

#drop NAs
df[!is.na(df$v_diab_white),]
df[!is.na(df$v_diab_black),]

set.seed(4585) # for reproducibility

# for priors use the values from the total population

mu_base <- mean(df$v_diab_allR)
sigma_base <- sd(df$v_diab_allR)

##  !!!!!!!!!
##  !!!!!!!!!
##  !!!!!!!!!
##  !!!!!!!!!
# Modify these lines to change the output - can estimate the difference
# in any of the populaiton size categories. 


df_white_filtered <- df
df_black_filtered <- df


filterVal <- '11-499'
df_white_filtered <- filter(df, d_diab_white == filterVal)
df_black_filtered <- filter(df, d_diab_black == filterVal)

##  !!!!!!!!!
##  !!!!!!!!!
##  !!!!!!!!!
##  !!!!!!!!!


# add a race column for indexing
df_white_filtered$race <- 1
df_black_filtered$race <- 2

# make the column names match
df_white_sm <- df_white_filtered[,c('race', 'd_diab_white', 'v_diab_white')]
df_black_sm <- df_black_filtered[,c('race', 'd_diab_black', 'v_diab_black')]

colnames(df_white_sm)[2] ="popSize"
colnames(df_black_sm)[2] ="popSize"

colnames(df_white_sm)[3] ="prev"
colnames(df_black_sm)[3] ="prev"

# make final merged dataframe
dfMerged <- rbind(df_white_sm, df_black_sm)

# rethinking's quap function to assess data with the prior (mu and sigma_base)
mean_by_race <- quap(
  alist(
    prev ~ dnorm(mu,sigma),
    mu <- a[race],
    a[race] ~ dnorm(27,4), # from mu_base and sigma_base
    sigma ~ dunif(0,10)
  ), data=dfMerged 
)

#extract samples from the model
post <- extract.samples(mean_by_race)

dens(post$a[,2],xlim=c(25,38), ylim=c(0,5),lwd=3, col=my_colors$dkOr)
dens(post$a[,1],lwd=3, col=my_colors$dkBl, add=TRUE)

# the above density plots are the uncertainty of the actual mean value
# to get a better idea of the actual variance in the county data, simulate 
# a group of samples from the posterior

dW <- rnorm(1000, post$a[,1], post$sigma)
dB <- rnorm(1000, post$a[,2], post$sigma)

dens(dW, col=my_colors$dkBl)
dens(dB, col=my_colors$dkOr, add=TRUE)

# to calculate the difference, difference the post values
muContrast_post <- post$a[,1] - post$a[,2] 

dens(muContrast_post, xlab='Posterior mean dabetes prevalence difference (%)')
mean(muContrast_post)
HPDI(muContrast_post, 0.89)
ci95 <- HPDI(muContrast_post, 0.95)
names(ci95) <- NULL



muContrast <- data.frame(muContrast_post)

muCp <- ggplot(muContrast, aes(x=muContrast_post)) + geom_density()
muCp

muCp_g <- ggplot_build(muCp)

muCp_df <- data.frame(x=muCp_g$data[[1]]$x, y=muCp_g$data[[1]]$y)



muCp_df_ci95 <- filter(muCp_df, x >= ci95[1] & x <= ci95[2])

muCp_plot <- ggplot(data=muCp_df, aes(x,y) ) + 
 theme_half_open() +
  theme(
    axis.line = element_line(color = my_colors$dkGr),
    axis.title = element_text(color = my_colors$dkGr),
    axis.ticks = element_line(color = my_colors$dkGr),
    axis.text = element_text(color = my_colors$dkGr),
  ) + 
  #scale_x_continuous(limits = c(-11, -9.85)) + 
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  xlab('Posterior Mean Diabetes Prevalence Difference by Race (%)') +
  ylab('Density') +
  geom_area(data=muCp_df_ci95, aes(x,y),fill = my_colors$gr, alpha = 0.5) +
  geom_line(data=muCp_df, aes(x,y), color = my_colors$dkGr, size = 0.75) +
  annotate('text', 
    label = paste(round(ci95[1], 2),'%', sep=''),
    x = ci95[1] - 0.065,
    y = 0.425,
    size = 4.5,
    color = my_colors$dkGr
  ) +
  annotate('text', 
    label = paste(round(ci95[2], 2),'%', sep=''),
    x = ci95[2] + 0.065,
    y = 0.425,
    size = 4.5,
    color = my_colors$dkGr
  )


muCp_plot

# this shows that on average for a given population on average prevalence of diabetes
# will be on average about about 10.4% higher in blacks than whites with the bulk of the uncertainty ranging
# from 10.06 to as high as 10.74


# another view of this is to look at the full distribution by sampling not the mean
# but the mean plus variation by doing the contrast on Dw and Db calculated above

diab_contrast <- dB - dW
dens(diab_contrast,xlab='Posterior prevalence difference (%)')
sum(diab_contrast > 0) / 1000
sum(diab_contrast < 0) / 1000

dfContrast <- data.frame(diab_contrast)

p<-ggplot(dfContrast,aes(x=diab_contrast)) +
  geom_density() 

pg <- ggplot_build(p)

pg_data<-data.frame(pg$data[[1]],stringsAsFactors = F)
pg_data_lt0 <- filter(pg_data, x <= 0.1)
pg_data_gt0 <- filter(pg_data, x >= -0.1)

plot <- ggplot(data=pg_data, aes(x,y) ) + 
 theme_half_open() +
  theme(
    axis.line = element_line(color = my_colors$dkGr),
    axis.title = element_text(color = my_colors$dkGr),
    axis.ticks = element_line(color = my_colors$dkGr),
    axis.text = element_text(color = my_colors$dkGr),
    legend.position = c(0.6, 0.8),
    legend.title = element_blank()
  ) + 
  geom_area(data=pg_data_lt0,aes(x,y),fill = my_colors$or, alpha = 0.5) +
  geom_area(data=pg_data_gt0,aes(x,y),fill = my_colors$bl, alpha = 0.5) +
  geom_line(data=pg_data_gt0,aes(x,y), color = my_colors$dkBl, size = 0.75) +
  geom_line(data=pg_data_lt0,aes(x,y), color = my_colors$dkOr, size = 0.75) 

plot

muCp_plot