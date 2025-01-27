###################################################
#
# Title: Data_download_20250109.R
# Purpose: Extract daily counts of species occurrence records (only those that are georeferenced and tagged as human observations) from the monthly snapshot of the
#          Global Biodiversity Information Facility (GBIF) dated January 1st, 2025. Merge them with information on human mobility and stringency index of lockdown 
#          regimes in different countries.
#
# Author: Stephanie Roilo, TUD Dresden University of Technology, Germany & University of Bonn, Germany
# Date: last edited on January 27st, 2025
#
###################################################

library(arrow)
library(rgbif)  
library(dplyr)   
library(lubridate)
library(data.table)  # for fast reading of large datasets
library(ISOcodes)  # to access the ISO 3166 list of country codes
library(DHARMa)
library(nlme)     # for GLS
library(ggplot2)
library(rnaturalearth)
library(sf)
library(tmap)
library(gridExtra)
library(effects)  # for plotting conditional effects of the models
library(npreg)  # for non-parametric smoothing splines


### Access the January 2025 monthly GBIF snapshot ------------------------
# see instructions on how to work with Parquet format data: https://data-blog.gbif.org/post/apache-arrow-and-parquet/ 
# set the path to where the GBIF snapshot was downloaded 
local_df <- open_dataset("G:/Bonn_backup/January2025_snapshot/occurrence.parquet")

# check names of all columns in the dataset
schema(local_df)
# names of GBIF variables are explained here: https://techdocs.gbif.org/en/data-use/download-formats 

# count number of observations per day per country
snapshottab = local_df %>% 
  filter(
    is.na(decimallatitude) == FALSE,
    is.na(decimallongitude) == FALSE,
    is.na(countrycode) == FALSE,
    basisofrecord == "HUMAN_OBSERVATION",
    year >= 2010
  )  %>%
  group_by(year, month, day, countrycode) %>%
  count() %>% 
  collect() %>% 
  arrange(countrycode, year, month, day) 
# remove NAs (e.g. rows with missing days and/or months) | Note that countrycode = "NA" (Namibia) is different than NA (not available)
snapshottab = na.omit(snapshottab)   # snapshottab contains data for 251 countries and dependent territories
# create and eventDate column
snapshottab$eventDate = as.Date(paste(snapshottab$year, snapshottab$month, snapshottab$day, sep="-"))
# add rows for missing days on which no record was collected
snapt2 = merge(snapshottab, expand.grid(countrycode = unique(snapshottab$countrycode), eventDate = unique(snapshottab$eventDate)), all = TRUE)
# set the number of records to zero for those dates
snapt2$n[is.na(snapt2$n)] = 0
snapt2$year = year(snapt2$eventDate)
snapt2$month = month(snapt2$eventDate)
snapt2$day = day(snapt2$eventDate)

### extract eBird record counts per day ----------------------
ebirdc = local_df %>% 
  filter(
    is.na(decimallatitude) == FALSE,
    is.na(decimallongitude) == FALSE,
    institutioncode == "CLO",
    is.na(countrycode) == FALSE,
    basisofrecord == "HUMAN_OBSERVATION",
    year >= 2010
  ) %>%
  group_by(year, month, day, countrycode) %>%
  count() %>% 
  collect()  %>% 
  arrange(countrycode, year, month, day) 
# remove NAs (e.g. rows with missing days and/or months)
ebirdc = na.omit(ebirdc)
# create and eventDate column
ebirdc$eventDate = as.Date(paste(ebirdc$year, ebirdc$month, ebirdc$day, sep="-"))
# merge to the daily counts of human observations
names(ebirdc)[5] = "n_eBird"
ebirdc2 = merge(snapt2, ebirdc[,c("countrycode", "eventDate", "n_eBird")], by = c("countrycode", "eventDate"), all = TRUE)
# fill in missing values
ebirdc2$n_eBird[is.na(ebirdc2$n_eBird)] = 0

### Stringency index and mobility reports -------------------
# download Covid OWID (Our World In Data) data and the Google Covid-19 community mobility report data
covid = fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
mob = fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
# set start and end dates of the time period of interest
start_date = "2019-01-01"  # to be entered in the format yyyy-mm-dd
end_date = "2022-10-15"
# filter the data to the time period of interest
allcn = ebirdc2 %>% filter(
  eventDate >= start_date, 
  eventDate <= end_date)
# add the 3-lettered isocode and the country name to the occurrence record dataset
allcn$iso_code = ISO_3166_1$Alpha_3[match(allcn$countrycode, ISO_3166_1$Alpha_2)]
allcn$Country = ISO_3166_1$Name[match(allcn$countrycode, ISO_3166_1$Alpha_2)]
# check which countries are missing in the ISO 3166-1 list
unique(allcn$countrycode[is.na(allcn$iso_code)])   # XK = Kosovo, and ZZ = "High Seas" -> observations in international waters
# fix this
allcn[allcn$countrycode=="XK", "iso_code"] = "XKX"
allcn[allcn$countrycode=="XK", "Country"] = "Kosovo"
allcn = allcn[-which(allcn$countrycode=="ZZ"),]

# filter the covid data to be merged with the occurrence record dataset
covid = covid %>% 
  select(iso_code, location, date, stringency_index, population) %>%
  filter(
    date >= start_date, 
    date <= end_date)
# fix isocodes for Kosovo, and remove the multi-country entries (e.g. "World", continents, etc. which are preceded by OWID_)
covid$iso_code[covid$location=="Kosovo"] = "XKX"
covid = covid[!grepl("OWID", covid$iso_code),]
# merge the covid data with the occurrence record dataset
allcn = merge(allcn, covid, by.x = c("iso_code", "eventDate"), by.y = c("iso_code", "date"), all.x = TRUE, all.y =FALSE)

# merge the google mobility reports by country and bind them to the dataframe
# filter out all rows which have an iso_3166_2_code, as these mark subregions within a country
mob = mob %>% filter(
  iso_3166_2_code == "",
  sub_region_1 == "",
  sub_region_2 == "",
  metro_area == "")
allcn = merge(allcn, mob[,c("country_region_code", "date", "parks_percent_change_from_baseline", "residential_percent_change_from_baseline")], by.x = c("countrycode", "eventDate"), by.y = c("country_region_code", "date"),  all.x = TRUE, all.y =FALSE)

# add info on day of week, so that we can identify the weekends
allcn$weekday = weekdays(allcn$eventDate)
#allcn$weekday = strftime(allcn$eventDate, "%u") # get weekday in number format (1 is Monday)
allcn$weeknr = strftime(allcn$eventDate, format = "%V") # get number of the week
# remove the column "location", containing country name from the OWID covid dataset
allcn = allcn[,c(1:9, 11:16)]

# save to file
write.table(allcn, file = "Data_250_countries_Jan2025snapshot_20250109.csv", sep = ",", dec = ".", row.names = FALSE)

