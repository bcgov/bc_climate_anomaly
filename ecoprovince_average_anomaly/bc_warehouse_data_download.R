library(bcdata)
library(tidyverse)
library(magrittr)
library(sf)

# Explore data ------------------------------------------------------------


bcdc_browse() #BC Data catalogue browese manualay online
# bcdc_search( #Search data in the BC data catalgue
# ...,
# license_id = NULL,
# download_audience = "Public",
# res_format = NULL,
# sector = NULL,
# organization = NULL,
# n = 100
# )

# list all data and search by key words -----------------------------------

bcdc_list() %>%  #List of all the data on BC Data catalogue
  str_detect('ecoprovinces') %>%
  keep(bcdc_list(), .)#subset to water ('water as key word)

# Sample eco-province data
req_dt_fl <- "ecoprovinces-ecoregion-ecosystem-classification-of-british-columbia" # from the list of subseeted bc data catalogue
bcdc_get_record(req_dt_fl) # get record information about the data . Using this info download the data

bcdc_tidy_resources('51832f47-efdf-4956-837a-45fc2c9032dd') # information on records contained in teh data
bcdc_preview("51832f47-efdf-4956-837a-45fc2c9032dd") #Preview of the data

# Get the data to temporary folder

bcgw_data <- bcdc_get_data(record = '51832f47-efdf-4956-837a-45fc2c9032dd') # the record is from previous step
names(bcgw_data)
plot(st_geometry(bcgw_data))
# bcgw_data$id <- seq(1:nrow(bcgw_data))

# Write data in the folder
# Select only required columns and write the data
bcgw_data%<>%
  select(code=ECOPROVINCE_CODE,name=ECOPROVINCE_NAME,area=FEATURE_AREA_SQM,length=FEATURE_LENGTH_M,geometry)
bcgw_data
plot(bcgw_data)
unique(bcgw_data$code)
st_write(bcgw_data,"ecoprovinces_bc.shp",append=FALSE)

