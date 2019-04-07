setwd("~/Cal/Spring2019/Stat222")

#####
#### Author: Andrea Bonilla
#### 

library(tidyverse)
library(tidycensus)
variables <- load_variables(2016, "acs5", cache = TRUE)

# Neighborhood size, median income, employment rate, age/gender/race composition

# income check
# gender and age check 
# race check
# neighborhood size
# education and gender

               
population_vars <- paste("B01001_0", c(paste("0", 1:9 ,sep=""), 10:49) ,sep="")
population_names <- gsub("( |!!)", "_", gsub("(Estimate!!Total!!)","",
                    variables$label[variables$name %in% population_vars]))
income_vars <- paste("B06010_0", c(paste("0", 3:9, sep=""), 10:11), sep="")
income_names <- gsub("( |!!)", "_", 
                     gsub("(Estimate!!Total!!|\\$|,)","",
                          variables$label[variables$name %in% income_vars]))

race_vars <- paste("B03002_0", c(paste("0",2:9 ,sep=""), 10:21), sep="")
race_names <- gsub("( |!!)", "_", 
                   gsub("(Estimate!!Total!!|,)","",
                        variables$label[variables$name %in% race_vars]))

housing_vars <- c("B25008_002","B25008_003")
housing_names <- gsub("( |!!)", "_", 
                      gsub("(Estimate!!Total!!|,)","",
                           variables$label[variables$name %in% housing_vars]))

mobility_vars <- paste("B07003_0",c(paste("0", 4:9, sep=""), 10:18), sep="")
mobility_names <- gsub("( |!!)", "_", 
                       gsub("(Estimate!!Total!!|,)","",
                            variables$label[variables$name %in% mobility_vars]))

education_vars <- paste("B15002_0", c(paste("0", 2:9, sep=""), 10:18, 
                                      20:35), sep="")
education_names <- gsub("( |!!)", "_", 
                        gsub("(Estimate!!Total!!|,)","",
                             variables$label[variables$name %in% education_vars]))

employment_vars <- paste("B17005_0", c(paste("0", 1:9, sep=""), 10:22), sep="")
employment_names <- gsub("( |!!)", "_", 
                         gsub("(Estimate!!Total!!|,)","",
                              variables$label[variables$name %in% employment_vars]))

of_interest <- c(population_vars, income_vars, race_vars,
                 housing_vars, mobility_vars, education_vars, employment_vars)
of_interest_names <- c(population_names, income_names, race_names,
                       housing_names, mobility_names, education_names, employment_names)

translation <- data.frame(variable=of_interest, 
                          census_var=of_interest_names, stringsAsFactors = FALSE)

create_census_data <- function(years, geography){
  data <- data.frame()
  for(y in years){
    print(y)
    data0 <- get_acs(geography = geography, 
                    variables = of_interest , state='CA',
                    county="San Francisco", year = y)
    data0$year <- y
    data <- rbind(data, data0)
  }
  data <- left_join(data, translation, by="variable")
  return(data)
}

census_data_tract <- create_census_data(c(2009:2017), 'tract') # tract
#census_data_zip <- create_census_data(c(2009:2010), 'zcta') #zip NOPE Cant filter for dif parts in the state
census_data_block <- create_census_data(2013:2017, 'block group') # only after 2013, smaller than tracts

library(reshape2)
census_data_tract2 <- melt(census_data_tract, id.vars=c('GEOID', 'NAME', 'year', 'census_var'), 
                           measure.vars=c('estimate'))

tract_wide <- dcast(census_data_tract2, GEOID+NAME+year~census_var, mean)

census_data_block2 <- melt(census_data_block, id.vars=c('GEOID', 'NAME', 'year', 'census_var'), 
                           measure.vars=c('estimate'))

block_wide <- dcast(census_data_block2, GEOID+NAME+year~census_var, mean)

write_csv(tract_wide, "census_tract_2009_2017.csv")
write_csv(block_wide, "census_block_2013_2017.csv")
write_csv(census_data_tract, "census_tract_2009_2017_long.csv")
write_csv(census_data_block, "census_block_2013_2017_long.csv")
# translation <- data.frame(name=c(population_vars, income_vars, race_vars,
#                                      housing_vars, mobility_vars, education_vars), 
#                           label=c(population_names, income_names, race_names,
#                                        housing_names, mobility_names, education_names))
# 
# ## quick check that 2009-2017 same 
# for(y in 2010:2010){
#   vars <- load_variables(y, "acs5", cache = TRUE)
#   vars <- subset(vars, name %in% translation$name)
#   if(nrow(vars) ==nrow(translation)){
#     vars$label = gsub("( |!!)", "_", 
#                       gsub("(Estimate!!Total!!|\\$|,)","",
#                            vars$label))
#     check <- full_join(vars, translation, by=c("label","name"))
#     if(nrow(check)!=nrow(translation)){
#       cat('error in year', y, nrow(check))
#     }
#   }else{
#     cat('error in year ', y)
#   }
# }
# 
