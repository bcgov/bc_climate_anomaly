# Required -------------------------------
rqr_pkgs <-
  c(
    'terra',
    'sf',
    "tidyverse",
    'lubridate',
    'zoo',
    'magrittr',
    "Kendall",
    "zyp",
    "tidyterra",
    "colorspace",
    "cptcity",
    "quarto",
    'patchwork',
    'foreach',
    'doParallel'
  )
# Install packages if not
installed_rqr_pkgs <- rqr_pkgs %in% rownames(installed.packages())
if (any(installed_rqr_pkgs == FALSE)) {
  install.packages(rqr_pkgs[!installed_rqr_pkgs])
}
# Load:
lapply(rqr_pkgs , require, character.only = TRUE)

# Paths ------------------------
# setwd(getwd())
shp_fls_pth <- '../shapefiles/'
ano_dt_pth <-  '../ano_clm_data/'


## Months, parameters ----
months_nam <-
  c(
    "annual",
    "winter",
    "spring",
    "summer",
    "fall",
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
months_nam

parameters <- c("tmean", "tmax", "tmin", "prcp","vpd","rh","soil_moisture")
parameters

min_year <- 1951
max_year <- 2024

update_month <- "January"
update_year <- "2024"
years <- seq(min_year, max_year, 1)
years
length(years)
curr_mon_yr <- as.Date(paste0(update_year,update_month,"15"),format = "%Y%B%d" )
curr_mon_yr
prvs12_mon_yr <- curr_mon_yr-365
prvs12_mon_yr
prvs12_mon_yr_1 <- prvs12_mon_yr-30

prvs_12_mns <- seq.Date((curr_mon_yr-365),curr_mon_yr, by = "month")[2:13]
prvs_12_mns

cur_mon_nam <- format(as.Date(curr_mon_yr), "%B %Y")
cur_yr_nam <- format(as.Date(curr_mon_yr), "%Y")
cur_mon_only <- format(as.Date(curr_mon_yr), "%B")
prv_mon_nam <- format(as.Date(prvs12_mon_yr_1), "%B %Y")


## Anomalies Data files -----
list.files(path = ano_dt_pth,
           pattern = ".*_ano_.*\\.nc",
           full.names = T) -> ano_dt_fls
ano_dt_fls

ano_dt_fl <- tibble(dt_pth = ano_dt_fls)
ano_dt_fl %<>%
  mutate(mon = str_extract(ano_dt_fls,
                           paste(months_nam, collapse = "|")),
         par = str_extract(ano_dt_fls,
                           paste(parameters, collapse = "|")))

## Climatology Data files -----
list.files(path = ano_dt_pth,
           pattern = ".*_clm_.*\\.nc",
           full.names = T) -> clm_dt_fls
head(clm_dt_fls)

clm_dt_fl <- tibble(dt_pth = clm_dt_fls)
clm_dt_fl %<>%
  mutate(par = str_extract(clm_dt_fls,
                           paste(parameters, collapse = "|")))

# Spatial average analysis for BC
sel_area_shpfl <- bc_shp
region <- "BC"

current_month <- curr_mon_yr

# Mean temperature summary
parr <- 'tmean'
ano_dt_fl%>%
  filter(mon==format(as.Date(current_month), "%b")) -> ano_dt_fl_mn
ano_dt_fl_mn
monn <- unique(ano_dt_fl_mn$mon)

ano_dt_fl_mn_par <- ano_dt_fl_mn%>%
  filter(par==parr)

#Read data for given month and parameter
ano_dt_rast <- rast(ano_dt_fl_mn_par$dt_pth)
names(ano_dt_rast)

# Read data and create a monthly, 12 months and long term anomaly plot ----
# Current month anomaly -----
# Climatology
clm_dt_fl %>%
  filter(par == parr) -> clm_dt_fl_i

clm_dt_rast <- rast(clm_dt_fl_i$dt_pth)
clm_dt_rast
names(clm_dt_rast) <- months_nam
clm_dt_rast_mon <-
  subset(clm_dt_rast, which(names(clm_dt_rast) %in% monn))
clm_dt_rast <- clm_dt_rast_mon
rm(clm_dt_rast_mon)
# plot(clm_dt_rast)
# plot(ano_dt_rast)

if (parr == 'prcp' | parr == 'soil_moisture') {
  ano_dt_rast_per1 <- (ano_dt_rast / clm_dt_rast) * 100
  #If prcp anomalies are very high ( > 200 %) then convert and limit to 200.
  ano_dt_rast_per2 <-
    ifel(ano_dt_rast_per1 > 201, 200, ano_dt_rast_per1)
  ano_dt_rast_per3 <-
    ifel(ano_dt_rast_per2 < -201, -200, ano_dt_rast_per2)
  ano_dt_rast <- ano_dt_rast_per3
} else{
  ano_dt_rast <- ano_dt_rast
}
# plot(ano_dt_rast,40:44)
ano_dt_rast
# Monthly spatial anomaly for current month
# Subset for current month and create the plot
ano_dt_rast_mn <-
  subset(ano_dt_rast, which(names(ano_dt_rast) %in% curr_mon_yr))
ano_dt_rast_mn
# plot(ano_dt_rast_mn)

ano_rng_lmt <- terra::minmax(ano_dt_rast_mn, compute = T)
mean_ano_val <- global(
  ano_dt_rast_mn, fun = "mean", na.rm = T
)



