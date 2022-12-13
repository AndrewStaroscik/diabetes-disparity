# get the data and return what is needed for this analysis
getDataFrame = function() {

  library(tidyverse)

  # Load data and name df
  diabObesity2021 <- read.csv("./data/diab_by_race2021_v02a.csv")
  df <- diabObesity2021 #The whole data set

  # bring in a list of state abreviations
  stabr <- read.csv("./data/stateAbr.csv")

  stabr$state <- toupper(stabr$state) # needed so that everything matches

  # add the two letter state abbr
  df <- left_join(df,stabr, by="state")

  # get rid of the word county in the county column
  df$county_short <- gsub(" County", "", df$county)

  #make a new column containing a useful label that has both county and state names
  df$county_info = paste(df$county_short,', ',df$code, sep="")

  return (df)

}
