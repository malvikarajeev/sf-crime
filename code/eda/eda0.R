setwd("~/Cal/Spring2019/Stat222")

#####
#### Author: Andrea Bonilla
#### 

library(tidyverse)
fn <- list.files("data/", full.names = TRUE)
data0 <- read_csv(fn[2], col_types = cols(
  IncidntNum = col_integer(),
  Category = col_character(),
  Descript = col_character(),
  DayOfWeek = col_character(),
  Date = col_date(format="%m/%d/%Y"),
  Time = col_time(format = "%H:%M"),
  PdDistrict = col_character(),
  Resolution = col_character(),
  Address = col_character(),
  X = col_double(),
  Y = col_double(),
  Location = col_character(),
  PdId = col_double()
))

names(data0)[c(10:11)] <- c("lng","lat")
data0 <- data0[!is.na(data0$PdDistrict),]
library(lubridate)
data0$year <- year(data0$Date)

data0 <- filter(data0, year < 2018) # keep earlier data...data dif now

data1 <- read_csv(fn[1],
                  col_names = c("IncidntDatetime","Date",
                                "Time", "year",
                                "DayOfWeek",
                                "ReportDatetime", "RowID",
                                "IncidntID","IncidntNum",
                                "CADNum","ReportType",
                                "ReportTypeDescr",
                                "FiledOnline","IncidntCode",
                                "Category",
                                "Subcategory",
                                "Descript",
                                "Resolution", "Address",
                                "CNN","PdDistrict","AnalysisNeighborhood",
                                "SupervisorDistrict",
                                "lat","lng","Location"),
                  skip = 1,
                  col_types = cols(
                    .default = col_character(),
                    IncidntDatetime = col_datetime("%Y/%m/%d %H:%M:%S %p"),
                    Date = col_date(format = ""),
                    Time = col_time(format = ""),
                    year = col_integer(),
                    ReportDatetime = col_datetime("%Y/%m/%d %H:%M:%S %p"),
                    RowID = col_double(),
                    IncidntID = col_integer(),
                    IncidntNum = col_integer(),
                    CADNum = col_integer(),
                    CNN = col_double(),
                    lat = col_double(),
                    lng = col_double()
                  ))

data1 <- filter(data1, PdDistrict!="Out of SF")

data1$Category <- toupper(data1$Category)
data1$PdDistrict <- toupper(data1$PdDistrict)
data1$Descript <- toupper(data1$Descript)

## COMBINE 
data <- full_join(data0, data1)

nrow(data0) + nrow(data1) == nrow(data)

## data cleaning
#data$year <- year(data$Date)
data$month <- month(data$Date)
data$day <- day(data$Date)

#according to https://en.wikipedia.org/wiki/White-collar_crime#Blue-collar_crime

white_crime=c("FRAUD", "FORGERY/COUNTERFEITING", "BAD CHECKS" , "EXTORTION", 
              "EMBEZZLEMENT", "SUSPICIOUS OCC","BRIBERY",
              "FORGERY AND COUNTERFEITING")

blue_crime=c("VANDALISM", "LARCENY/THEFT", "STOLEN PROPERTY", "ROBBERY", 
             "DRIVING UNDER THE INFLUENCE",  "DISORDERLY CONDUCT", "LIQUOR LAWS", 
             "VEHICLE THEFT", "ASSAULT", "KIDNAPPING", "TRESPASS", 
             "ARSON", "RECOVERED VEHICLE", "BURGLARY", "LARCENY THEFT",
             "TRAFFIC VIOLATION ARREST", "MOTOR VEHICLE THEFT", "MOTOR VEHICLE THEFT?",
             "VEHICLE IMPOUNDED","VEHICLE MISPLACED")

sex_crime=c("SEX OFFENSES NON FORCIBLE", "SEX OFFENSES FORCIBLE", 
            "SEX OFFENSES, NON FORCIBLE", "SEX OFFENSES, FORCIBLE",
            "PROSTITUTION", "RAPE", "HUMAN TRAFFICKING, COMMERCIAL SEX ACTS",
            "HUMAN TRAFFICKING (A), COMMERCIAL SEX ACTS",
            "SEX OFFENSE")

drug_crime <- c("DRUG OFFENSE","DRUG/NARCOTIC",
                "DRUG VIOLATION")

other_crime=c("MISSING PERSON", "RUNAWAY", "FAMILY OFFENSES", 
              "PORNOGRAPHY/OBSCENE MAT", "WEAPON LAWS", "DRUNKENNESS", "SUICIDE", 
              "TREA",  "LOITERING",
              "OTHER OFFENSES", "NON-CRIMINAL", "GAMBLING", "WARRANTS",
              "SECONDARY CODES", "OTHER MISCELLANEOUS",
              "MALICIOUS MISCHIEF", "MISCELLANEOUS INVESTIGATION",
              "LOST PROPERTY", "VEHICLE IMPOUNDED", "OTHER", "JUVENILE OFFENSES",
              "OFFENCES AGAINST THE FAMILY AND CHILDREN", "WEAPONS CARRYING ETC",
              "WARRANT", "HOMICIDE",
              "WEAPONS OFFENCE", "COURTESY REPORT", "FIRE REPORT",
              "TRAFFIC COLLISION", "FAMILY OFFENSE", "WEAPONS OFFENSE",
              "CASE CLOSURE", "CIVIL SIDEWALKS", "SUSPICIOUS")

data$CatDesc <- ifelse(data$Category %in% white_crime, "WhiteCollar",
                       ifelse(data$Category %in% blue_crime, "BlueCollar", 
                              ifelse(data$Category %in% drug_crime, "Drug", 
                                     ifelse(regexpr("DRUG", data$Descript) != -1 &
                                              regexpr("SEXUAL ASSAULT", data$Descript) == -1 , "Drug", # without word sexual assualt
                              ifelse(data$Category %in% other_crime, "Other", 
                                     ifelse(data$Category %in% sex_crime, "SexCrime",
                                            NA))))))

data$marijuana <- ifelse(regexpr("MARIJUANA", data$Descript) != -1,
                         1, 0)
data$meth <-ifelse(regexpr("METHAMPHETAMINE|AMPHETAMINE|METH-AMPHETAMINE", 
                           data$Descript) != -1, 1, 0)
data$opium <- ifelse(regexpr("OPIUM", data$Descript) != -1, 1, 0)


sum(is.na(data$CatDesc))

## for now just drop 2019
## EDA 
#####################################
data <- filter(data, year < 2019)

create_graph <- function(data, column){
  ggplot(data=data) + geom_bar(aes_string(column)) + theme_bw()
}

create_counts <- function(data, column){
  return(data %>% group_by(!!column) %>% count(sort=TRUE))
}

create_counts(data, quo(Descript))
create_counts(data, quo(Category))
create_counts(data, quo(PdDistrict))
create_counts(data, quo(Resolution))

library(scales)

relations <- data %>% group_by(Category, Descript) %>% count(sort=TRUE)
over_time <- data %>% group_by(year, CatDesc) %>% count(sort=TRUE)

resolution_cat <- data %>% group_by(Category, Resolution) %>% count(sort=T)

ggplot(data=over_time) + geom_bar(aes(x=CatDesc, 
                                      y=n, fill=CatDesc), stat="identity") +
  facet_wrap(.~year) + theme_bw() + theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank(),
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank())


## ranking of top categories of crime
categories <- data %>% group_by(year, Category) %>% count() %>% 
  group_by(year) %>% arrange(-n) %>%
  mutate(rank=row_number()) %>% arrange(year)

categories$top10 <- ifelse(categories$rank < 11, 1, 0)
top10 <- unique(categories$Category[categories$top10==1])
categories$top10bool <- ifelse(categories$Category %in% top10,  1, 0)

ggplot(data=filter(categories, top10bool==1)) + geom_bar(aes(x=Category, y=n,
                                                             fill=Category), 
                                                         stat="identity") +
  facet_wrap(.~year) + theme_bw()  + theme(axis.title.x = element_blank(),
                                            axis.text.x = element_blank(),
                                            panel.grid.major.x = element_blank(),
                                            panel.grid.minor.x = element_blank())
# larceny/theft increasing over the years

year_count <- create_counts(data, quo(year))
year_count <- left_join(year_count, sf_pop, by="year")
year_count <- year_count %>% mutate(crime_rate=n/pop*100000)

ggplot(data=year_count) + geom_line(aes(x=year, y=crime_rate)) + 
  theme_bw() + ylab("Count") #+ geom_vline(xintercept=2016) +  
  #geom_vline(xintercept=2018)

### SF POP DATA

sf_pop <- data.frame(year=2003:2018, 
                     pop=c(781870,780699, 779655, 782928, 791334,798673,
                           801799,809753, 820194,833120, 844066,
                            854421,864783,872463, 880418,880418 # assuming same
                           ))

year_count_pd <- data %>% group_by(year, PdDistrict) %>% count()
year_count_pd <- left_join(year_count_pd, sf_pop, by="year")
year_count_pd <- year_count_pd %>% mutate(crime_rate=n/pop*100000)

ggplot() + geom_line(data=year_count_pd, aes(x=year, y=crime_rate,
                                             color=factor(PdDistrict))) +
  #geom_line(data=year_count, aes(x=year, y=n)) + 
  theme_bw() + ylab("Rate per 100,000 Residents") + scale_y_continuous(label=comma) + 
  scale_color_discrete(name="Police District") + ggtitle("Crime over Time") 

year_count_pd2018 <- data %>% filter(year==2018) %>% 
  group_by(PdDistrict, CatDesc) %>% count()

year_count_pd2018_2 <- data %>% filter(year==2018) %>% 
  group_by(PdDistrict, Category) %>% count(sort=T)

date_count <- create_counts(data, quo(Date))
ggplot(data=date_count) + geom_line(aes(x=Date, y=n)) + theme_bw() +
  ylab("Count") + geom_smooth(aes(x=Date, y=n)) + 
  geom_vline(xintercept=as.Date("2016-01-01"),color="red") +  
  geom_vline(xintercept=as.Date("2018-01-01"), color="red")

ggplot(data=date_count) + geom_density(aes(x=n), alpha=0.2) +
  geom_bar(aes(x=n),alpha=0.8) + 
  theme_bw() + ylab("Frequency") + xlab("Number of Crimes in a Given Day")

month_count <- data %>% group_by(year, month) %>% count()
month_count <- left_join(month_count, sf_pop, by="year")
month_count <- month_count %>% mutate(crime_rate=n/pop*100000)

#month_count$month_year <- interaction(month_count$year, month_count$month, lex.order=T)
ggplot(data=month_count) + geom_line(aes(x=interaction(year, month, lex.order = T),
                                         y = crime_rate, group = 1)) +
  scale_y_continuous(label=comma) + #, limits=c(9500, 14050))  + 
  ggtitle("Monthly Crime Counts per 100,000 Resdients over Time") + ylab("Count") + 
  #annotate(geom = "text", x = seq_len(nrow(month_count)), y = 0, label = month_count$month, size = 1) +
  annotate(geom = "text", x = 5 +   12 * (0:15), y = 1100, 
           label = unique(month_count$year), size = 2) +
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     panel.grid.major.x = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.minor.x = element_blank())

library(leaflet)
library(tigris)
library(rgdal)

# find correct shape files

# effective July 19th, 2015,
# https://data.sfgov.org/Public-Safety/Current-Police-Districts/wkhw-cjsf
# effective May 2003 - July 2015 
# https://data.sfgov.org/Public-Safety/Historical-Police-Districts/embj-38bg
# download shape file option

##### MAPS
##### 

postJuly2015 <-  readOGR(
  list.files("Current Police Districts", "*.shp", full.names = TRUE))

preMay2003 <- readOGR(
  list.files("Historical Police Districts", "*.shp", full.names = TRUE))

merge_shape_files <- function(data, year){
  if(year==2015){
    print("Redistricted police department. Using July 2015 data for entire year")
  }
  sub_year <- filter(data, year==year)
  if(year<2015){
    map_data <- preMay2003
  }
  else{
    map_data <- postJuly2015
  }
  
  sub_year2 <- sub_year %>% group_by(PdDistrict) %>% count()
  
  map_data2 <- geo_join(map_data, sub_year2, by_sp="district", by_df="PdDistrict",
                        how='left')
  
  return(map_data2)
}

test <- merge_shape_files(data, year=2016)
test <- merge_shape_files(data, year=2018)

leaflet(test) %>% addTiles() %>% 
  fitBounds(lng1=-122.47, lng2=-122.37, lat1=37.70, lat2=37.82) %>%
  addPolygons(
    color="blue", weight=1,
    opacity = 1.0, fillOpacity = 0.5,
    popup=paste(test@data$district, 
                paste("Total Crime",format(test@data$n, big.mark=","), sep=": ")),
    fillColor = ~colorQuantile("YlOrRd", n)(n), # yellow, orange,r ed crime
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE))

test2 <- data %>% group_by(year, lng, lat) %>% count() %>% filter(year==2006)

leaflet(test2) %>% addTiles() %>% 
  addCircleMarkers(
    radius=1, weight=1,
    lat=~lat, lng=~lng,
    fillColor = ~colorQuantile("YlOrRd", sort(unique(n)))(n),
    fillOpacity=0.8, stroke=FALSE)



library(gganimate)
library(gifski)
animated_test <- data %>% group_by(year, PdDistrict) %>% count()

ggplot(animated_test, 
       aes(x=PdDistrict,y=n, fill=PdDistrict)) +
  geom_bar(alpha = 0.7, show.legend = TRUE, stat="identity") +
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank()) + 
  # Here comes the gganimate specific bits
  labs(title = 'Year: {closest_state}', x = 'Police District', y = 'Count') +
  transition_states(year, wrap=FALSE) 


## census data
library(tidycensus)
grab_dates <- function(min_year, max_year){
  estimates <- c()
  for(i in seq(min_year, max_year)){
    val <- get_acs(geography = 'county', variables = 'B01001_001', state='CA',
    county="San Francisco", year = i)$estimate
    estimates <- c(estimates, val)
  }
  df <- data.frame(year=seq(min_year, max_year), estimate=estimates)
  return(df)
}
sf <- grab_dates(2009, 2017)

variables <- load_variables(2016, "acs5", cache = TRUE)

grab_income <- function(min_year, max_year){
  
}

# income
x3 <- get_acs(geography='county', variables = 'B19127_001', state='CA',
              county='San Francisco', year = 2016)

### get points and map to census sub division
library(tigris)
sf_map <- tracts(state = "CA", county='San Francisco', cb =TRUE)
test <- data %>% filter(year==2018) %>% drop_na(lat, lng)


library(sp)

mapLookup <-  function(data, map){
  data0 <- data %>% drop_na(lat, lng)
  data1 <- data0[,c("lng","lat")]
  
  spatial_data  <- SpatialPointsDataFrame(coordinates(data1), data)
  
  proj4string(spatial_data) <- proj4string(map)
  
  over_lay_points <- over(spatial_data, map, returnList=FALSE)
  results <-  cbind(data0, over_lay_points)
  return(results)
}

testing  <- mapLookup(test, sf_map)



# http://www.sfgenealogy.org/sf/history/hgpop.htm
# https://www.census.gov/data/developers/data-sets.html
