rm(list = ls())
# Required -------------------------------
rqr_pkgs <-
  c(
    'terra',
    "tidyverse",
    'lubridate',
    'zoo',
    'magrittr',
    "Kendall",
    "zyp",
    'furrr'
  )
# Install packages if not
installed_rqr_pkgs <- rqr_pkgs %in% rownames(installed.packages())
if (any(installed_rqr_pkgs == FALSE)) {
  install.packages(rqr_pkgs[!installed_rqr_pkgs])
}
# Load:
lapply(rqr_pkgs, require, character.only = TRUE)

# Paths ------------------------
# setwd(getwd())
ano_dt_pth <- './ano_clm_trn_data/'
sptl_trn_dt_pth <- ano_dt_pth

min_year <- 1951
max_year <- 2026

update_month <- "May"
update_year <- "2026"

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

parameters <- c("tmean", "tmax", "tmin", "prcp", "vpd", "rh", "soil_moisture")
parameters

years <- seq(min_year, max_year, 1)
years
length(years)
curr_mon_yr <- as.Date(
  paste0(update_year, update_month, "15"),
  format = "%Y%B%d"
)
curr_mon_yr

cur_mon_nam <- format(as.Date(curr_mon_yr), "%B %Y")
cur_yr_nam <- format(as.Date(curr_mon_yr), "%Y")

## Anomalies Data files -----
list.files(
  path = ano_dt_pth,
  pattern = ".*_ano_.*\\.nc",
  full.names = T
) -> ano_dt_fls
ano_dt_fls

ano_dt_fl <- tibble(dt_pth = ano_dt_fls)
ano_dt_fl %<>%
  mutate(
    mon = str_extract(ano_dt_fls, paste(months_nam, collapse = "|")),
    par = str_extract(ano_dt_fls, paste(parameters, collapse = "|"))
  )

list.files(
  path = ano_dt_pth,
  pattern = ".*_clm_.*\\.nc",
  full.names = T
) -> clm_dt_fls
head(clm_dt_fls)

clm_dt_fl <- tibble(dt_pth = clm_dt_fls)
clm_dt_fl %<>%
  mutate(par = str_extract(clm_dt_fls, paste(parameters, collapse = "|")))
clm_dt_fl

# Monthly Seasonal and annual spatial trend cal function -----

spatial_trend_cal_fun <- function(parr, ano_dt_fl, clm_dt_fl, monn) {
  # parr <- 'tmean'
  # ano_dt_fl <- ano_dt_fl
  # clm_dt_fl <- clm_dt_fl
  # monn <- c('Apr','May','Jun')

  # Mann-Kendall trend function
  mk_trend_fun <- function(y) {
    se <- zyp::zyp.trend.vector(y, x = 1:length(y), conf.intervals = FALSE)
    trend <- if ("trend" %in% names(se)) se[["trend"]] else NA
    pval <- if ("sig" %in% names(se)) se[["sig"]] else NA
    c(trend, pval)
  }

  ano_dt_fl %>%
    filter(par == parr) -> ano_dt_fl_par
  ano_dt_fl_par

  # Monthly trend
  ano_dt_fl_par %>%
    filter(mon %in% monn) -> ano_dt_fl_par_mon
  ano_dt_fl_par_mon

  mons_unq <- unique(ano_dt_fl_par_mon$mon)

  for (i in 1:length(mons_unq)) {
    # i <- 1

    mons_unq_i <- mons_unq[i]

    ano_dt_fl_par_mon %>%
      filter(mon == mons_unq_i) -> ano_dt_fl_par_mon_i
    ano_dt_fl_par_mon_i

    #Read data for given month and parameter
    ano_dt_rast <- rast(ano_dt_fl_par_mon_i$dt_pth)
    names(ano_dt_rast)

    # Climatology
    clm_dt_fl %>%
      filter(par == parr) -> clm_dt_fl_i

    clm_dt_rast <- rast(clm_dt_fl_i$dt_pth)
    clm_dt_rast
    names(clm_dt_rast) <- months_nam

    clm_dt_rast_mon <-
      subset(clm_dt_rast, which(names(clm_dt_rast) %in% mons_unq_i))
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
    } else {
      ano_dt_rast <- ano_dt_rast
    }
    # plot(ano_dt_rast,40:44)
    ano_dt_rast

    # Spatial trends -------------------
    ano_dt_rast

    # Prepare metadata
    varname <- parr
    long_name <- paste0(parr, "spatial trends", mons_unq_i)

    # Units
    if (parr == "prcp") {
      unt <- "mm"
    } else if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "vpd") {
      unt <- "kPa"
    } else if (parr == "rh") {
      unt <- "%"
    } else if (parr == "soil_moisture") {
      unt <- "m\U00B3/m"
    }
    unt

    # Calculate trends 1950s -----------
    # Extract year info
    yr_df <- tibble(paryr = names(ano_dt_rast)) %>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    yr_df
    names(ano_dt_rast) <- yr_df$yr

    start_year <- min(yr_df$yr, na.rm = TRUE)
    end_year <- max(yr_df$yr, na.rm = TRUE)

    # Attempt trend calculation
    trend_stack <- tryCatch(
      {
        terra::app(ano_dt_rast, mk_trend_fun)
      },
      error = function(e) {
        message(glue::glue(
          "Trend calculation failed for '{parr}', month '{ mons_unq_i}': {e$message}"
        ))
        # Create empty NA rasters with same dimensions
        na_rast <- terra::rast(ano_dt_rast[[1]])
        na_rast[] <- NA
        c_trend <- na_rast
        c_pval <- na_rast
        terra::rast(list(c_trend, c_pval))
      }
    )
    names(trend_stack) <- c("trend_mag", "pval")

    # Save to NetCDF
    output_file <- file.path(
      sptl_trn_dt_pth,
      glue::glue(
        "{parr}_spatial_trend_mon_{mons_unq_i}_wna_{start_year}_{end_year}.nc"
      )
    )

    # Save new file
    terra::writeCDF(
      trend_stack,
      filename = output_file,
      longname = long_name,
      unit = paste0(unt, '_per_yr)'),
      overwrite = TRUE,
      split = TRUE,
      compression = 9
    )

    # Remove previous years files if exist however with different end year to avoid confusion
    prev_trnd_file <- file.path(
      sptl_trn_dt_pth,
      glue::glue(
        "{parr}_spatial_trend_mon_{mons_unq_i}_wna_{start_year}_{end_year-1}.nc"
      )
    )
    if (file.exists(prev_trnd_file)) {
      message("Removing previous trend file: ", basename(prev_trnd_file))
      file.remove(prev_trnd_file)
    }

    # Trends  1980s ----------
    # Extract year info
    yr_df %>%
      filter(yr > 1979) -> yr_df80

    ano_dt_rast80 <- subset(
      ano_dt_rast,
      which(names(ano_dt_rast) %in% yr_df80$yr)
    )
    # plot(ano_dt_rast80)

    start_year80 <- min(yr_df80$yr, na.rm = TRUE)
    end_year80 <- max(yr_df80$yr, na.rm = TRUE)

    # Attempt trend calculation
    trend_stack80 <- tryCatch(
      {
        terra::app(ano_dt_rast80, mk_trend_fun)
      },
      error = function(e) {
        message(glue::glue(
          "Trend calculation failed for'{parr}', month '{mons_unq_i}': {e$message}"
        ))
        # Create empty NA rasters with same dimensions
        na_rast <- terra::rast(ano_dt_rast80[[1]])
        na_rast[] <- NA
        c_trend <- na_rast
        c_pval <- na_rast
        terra::rast(list(c_trend, c_pval))
      }
    )
    names(trend_stack80) <- c("trend_mag", "pval")

    # Save to NetCDF
    output_file <- file.path(
      sptl_trn_dt_pth,
      glue::glue(
        "{parr}_spatial_trend_mon_{mons_unq_i}_wna_{start_year80}_{end_year80}.nc"
      )
    )
    terra::writeCDF(
      trend_stack80,
      filename = output_file,
      longname = long_name,
      unit = paste0(unt, '_per_yr)'),
      overwrite = TRUE,
      split = TRUE,
      compression = 9
    )

    # Remove previous years files if exist however with different end year to avoid confusion
    prev_trnd_file <- file.path(
      sptl_trn_dt_pth,
      glue::glue(
        "{parr}_spatial_trend_mon_{mons_unq_i}_wna_{start_year80}_{end_year80-1}.nc"
      )
    )
    if (file.exists(prev_trnd_file)) {
      message("Removing previous trend file: ", basename(prev_trnd_file))
      file.remove(prev_trnd_file)
    }
  }
}

# make the vecto of months as required for calculation
ano_dt_fl
# Plan for multicore or multisession
plan(multisession, workers = 7) # adjust number of cores

# Parallelized version over parameters
# monns <- unique(ano_dt_fl$mon)
monns <- c('Jun', 'spring')

# Use future_walk to iterate in parallel (no return value needed)
future_walk(parameters, function(par_j) {
  # Call your function inside the parallel worker
  spatial_trend_cal_fun(
    parr = par_j,
    ano_dt_fl = ano_dt_fl,
    clm_dt_fl = clm_dt_fl,
    monn = monns
  )
})


# Regular foor loop -----------------

for (j in 1:length(parameters)) {
  # j<-4

  par_j <- parameters[[j]]
  par_j

  spatial_trend_cal_fun(
    parr = par_j,
    ano_dt_fl = ano_dt_fl,
    clm_dt_fl = clm_dt_fl,
    monn = monns
  )
}
