

# clear variables and data frames 
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off()

source('./loadData.r')

library(rethinking) # for clean precis output

# get dataframe
df <- getDataFrame()

dfForSummary <- data.frame(df[c('v_diab_allR', 'v_diab_white', 'v_diab_black')])

precis(dfForSummary)

# get count of non-NA and non-zero values
colSums(!is.na(dfForSummary) & dfForSummary != 0)


# get count of zero values
colSums(!is.na(dfForSummary) & dfForSummary == 0)