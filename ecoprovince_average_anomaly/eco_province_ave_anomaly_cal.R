## This R Script calculate spatially average anomalies of eco-regions of BC.
# For details please refer this Github page : https://github.com/bcgov/bc_climate_anomaly
## author: Aseem Raj Sharma aseem.sharma@gov.bc.ca
# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Required -------------------
library('sf')
library('terra')
library('tidyterra')
library('tidyverse')
library('magrittr')
library('lubridate')
library('zoo')

# Load and process input data -------
## Paths --
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shp_fls_pth <- '../shapefiles/'
ano_dt_pth <-  '../ano_clm_data/'

## Eco-region and BC Shape files --------------
# List of shape files
list.files(path = shp_fls_pth,
           pattern = ".shp",
           full.names = T) -> shp_fls_lst
shp_fls_lst

# BC
bc_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_shapefile") == T])
# plot(st_geometry(bc_shp))

# BC eco-regions
bc_ecoprv_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoprovince") == T])
# plot(st_geometry(bc_ecoprv_shp))

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

parameters <- c("tmean", "tmax", "tmin", "prcp")
parameters

min_year <- 1951
max_year <- 2023

update_month <- c("Oct","Nov")
update_year <- "2023"

## Anomalies Data files -----
list.files(path = ano_dt_pth,
           pattern = ".*_ano_.*\\.nc",
           full.names = T) -> ano_dt_fls
head(ano_dt_fls)

ano_dt_fl <- tibble(dt_pth = ano_dt_fls)
ano_dt_fl %<>%
  mutate(mon = str_extract(ano_dt_fls,
                           paste(months_nam, collapse = "|")),
         par = str_extract(ano_dt_fls,
                           paste(parameters, collapse = "|")))%<>%
  drop_na()

## Climatology Data files -----
list.files(path = ano_dt_pth,
           pattern = ".*_clm_.*\\.nc",
           full.names = T) -> clm_dt_fls
head(clm_dt_fls)

clm_dt_fl <- tibble(dt_pth = clm_dt_fls)
clm_dt_fl %<>%
  mutate(par = str_extract(clm_dt_fls,
                           paste(parameters, collapse = "|")))

# Anomalies and percentage calculation  --------------------------------------------------------

#Function to calculate spatially averaged anomalies and percentage of  anomalies for BC and ecoprovince region of BC

# i<-1
ano_subregion_clip_fun <-
  function(ano_dt_fl,
           clm_dt_fl,
           eco_rgn_shp,
           eco_region_name) {

    # eco_region_name <- "eco_province"
    # eco_rgn_shp <- bc_ecoprv_shp
    # clm_dt_fl <- clm_dt_fl
    # ano_dt_fl_i <- ano_dt_fl[i,]
    # ano_dt_fl_i

    ano_dt_fl_i <- ano_dt_fl

    # eco_rgn_shp$centroid <- st_centroid(eco_rgn_shp)

    ano_par_dt <- rast(ano_dt_fl_i$dt_pth)

    yr_df <- tibble(paryr = names(ano_par_dt))
    yr_df %<>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    names(ano_par_dt) <- yr_df$yr

    parr <- unique(ano_dt_fl_i$par)
    mon <- unique(ano_dt_fl_i$mon)

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "(°C)"
    } else if (parr == "prcp") {
      unt <- "(mm)"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "(%)"
    } else {
      unt <- ""
    }
    unt

    # Anomaly plot by decades
    minyr  <- min(yr_df$yr)
    maxyr  <- max(yr_df$yr)
    monn <- unique(mon)

    #Average anomaly and percentage for BC
    # clip by part of shapefile
    bc_ano_dt_shp_i <-
      terra::crop(ano_par_dt, bc_shp, mask = T)
    # plot(bc_ano_dt_shp_i)

    # Shape file spatial average anomalies by year
    bc_ano_shp_av_dt_i <-
      tibble(rownames_to_column(global(
        bc_ano_dt_shp_i, fun = "mean", na.rm = T
      ), "yrmn")) %>%
      dplyr::select(yrmn, ano = mean)

    #Calculate climatology
    clm_dt_fl %>%
      filter(par == parr) -> clm_dt_fl_par
    clm_dt_par <- rast(clm_dt_fl_par$dt_pth)
    names(clm_dt_par) <- months_nam
    # plot(clm_dt_par)

    clm_dt_par_mn_i <-
      subset(clm_dt_par, which(names(clm_dt_par) %in% monn))

    bc_clm_dt_shp_i <-
      terra::crop(clm_dt_par_mn_i, bc_shp, mask = T)
    # plot(bc_clm_dt_shp_i)

    bc_clm_shp_av_dt_i <-
      tibble(rownames_to_column(global(
        bc_clm_dt_shp_i, fun = "mean", na.rm = T
      ), "yrmn")) %>%
      dplyr::select(yrmn, clm_av = mean)

    # For percentage calculation
    bc_ano_shp_av_dt_i %<>%
      mutate(av_clm_val = bc_clm_shp_av_dt_i$clm_av) %>%
      mutate(
        per_ano = (ano / av_clm_val) * 100,
        yr = as.numeric(str_extract(yrmn, "[0-9]+")),
        par = parr,
        mon = mon,
        subregion = "BC",
        region = "BC"
      ) %>%
      select(yr, par, mon, ano, av_clm_val, per_ano, subregion, region)

    # Extract data for eco-regions
    shp_fl_mean_ano_list <- list()
    for (j in 1:nrow(eco_rgn_shp)) {
      # j<-3
      shp_fl_i <- eco_rgn_shp[j,]
      shp_fl_i
      # plot(st_geometry(shp_fl_i))

      # clip by part of shapefile
      ano_dt_shp_i <-
        terra::crop(ano_par_dt, shp_fl_i, mask = T)
      # plot(ano_dt_shp_i)

      # Shape file spatial average anomalies by year
      ano_shp_av_dt_i <-
        tibble(rownames_to_column(global(
          ano_dt_shp_i, fun = "mean", na.rm = T
        ), "yrmn")) %>%
        dplyr::select(yrmn, ano = mean)
      ano_shp_av_dt_i

      #Calcualte climatology
      clm_dt_fl %>%
        filter(par == parr) -> clm_dt_fl_par
      clm_dt_par <- rast(clm_dt_fl_par$dt_pth)
      names(clm_dt_par) <- months_nam

      clm_dt_par_mn_i <-
        subset(clm_dt_par, which(names(clm_dt_par) %in% monn))
      clm_dt_par_mn_i

      clm_dt_shp_i <-
        terra::crop(clm_dt_par_mn_i, shp_fl_i, mask = T)
      # plot(clm_dt_shp_i)

      clm_shp_av_dt_i <-
        tibble(rownames_to_column(global(
          clm_dt_shp_i, fun = "mean", na.rm = T
        ), "yrmn")) %>%
        dplyr::select(yrmn, clm_av = mean)
      clm_shp_av_dt_i

      #For percentage calculation
      ano_shp_av_dt_i %<>%
        mutate(av_clm_val = clm_shp_av_dt_i$clm_av) %>%
        mutate(
          per_ano = (ano / av_clm_val) * 100,
          yr = as.numeric(str_extract(yrmn, "[0-9]+")),
          par = parr,
          mon = mon,
          subregion = unique(shp_fl_i$name),
          region = eco_region_name
        ) %>%
        select(yr, par, mon, ano, av_clm_val, per_ano, subregion, region)
      ano_shp_av_dt_i
      tail(ano_shp_av_dt_i)
      shp_fl_mean_ano_list[[j]] <- ano_shp_av_dt_i
    }
    shp_sub_rg_mean_ano_dt <- bind_rows(shp_fl_mean_ano_list)

    #Combine BC and eco-regions

    bc_shp_sub_rg_mean_ano_dt <-
      bind_rows(bc_ano_shp_av_dt_i, shp_sub_rg_mean_ano_dt)
    tail(bc_shp_sub_rg_mean_ano_dt)

    # write_csv(
    #      bc_shp_sub_rg_mean_ano_dt,
    #      file = paste0(
    #        dump_pth,
    #        parr,
    #        "_ano_per_",
    #        mon,
    #        ".csv"
    #      )
    #    )
    return(bc_shp_sub_rg_mean_ano_dt)
  }

eco_prv_ano_dt_lst <- list()
for (i in 1:nrow(ano_dt_fl)) {
  eco_prv_ano_dt_lst[[i]] <- ano_subregion_clip_fun(
    ano_dt_fl = ano_dt_fl[i,],
    clm_dt_fl = clm_dt_fl,
    eco_rgn_shp = bc_ecoprv_shp,
    eco_region_name = "eco_province"
  )
}
eco_prv_ano_dt <- bind_rows(eco_prv_ano_dt_lst)
eco_prv_ano_dt%<>%
  drop_na()
# eco_prv_ano_dt %<>%
#   filter(yr >= update_year)

# Read csv upto 2022 and add updated month
# ecoprv22_dt <-
#   read_csv('temp_prcp_avg_ano_per_bc_ecoprvnc_data_1950_2022.csv')
# ecoprv22_dt
# tail(ecoprv22_dt)

# # Merge and update with new data
# ecoprv_dt_updated <- bind_rows(ecoprv22_dt, eco_prv_ano_dt)
# ecoprv_dt_updated
# ecoprv_dt_updated %<>%
#   drop_na()

write_csv(
  eco_prv_ano_dt,
  file = paste0(
    "temp_prcp_avg_ano_per_bc_ecoprvnc_data_updated_",
    update_month[[2]],
    "_",
    update_year,
    ".csv"
  )
)


# End -----------------------------------------------------------------
