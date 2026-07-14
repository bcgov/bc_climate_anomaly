# rm(list = ls())
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
    'kableExtra',
    'foreach',
    'doParallel',
    'future',
    'tictoc'
  )
# Install packages if not
installed_rqr_pkgs <- rqr_pkgs %in% rownames(installed.packages())
if (any(installed_rqr_pkgs == FALSE)) {
  install.packages(rqr_pkgs[!installed_rqr_pkgs])
}
# Load:
lapply(rqr_pkgs, require, character.only = TRUE)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Paths ------------------------
# setwd(getwd())
shp_fls_pth <- './shapefiles/'
ano_dt_pth <- './ano_clm_trn_data/'
results_pth <- './bc_mon_climate_report/mon_2mon_results_plots/'

# Data files -----------------------------------------------------
# Update month

# read following values from render .r file
update_month <- "Aug"
update_year <- "2025"

# List of shape files
# Shape files ----------------------------------------
# Domain
xmi = -140
xmx = -108
ymi = 39
ymx = 60

# List of shape files
list.files(
  path = shp_fls_pth,
  pattern = "\\.(shp|gpkg)$",
  full.names = TRUE,
  ignore.case = TRUE
) -> shp_fls_lst
shp_fls_lst

# Western North America
na_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "north_america") == T])
# plot(na_shp)
wna_shp <- crop(na_shp, ext(xmi, xmx, ymi, ymx))
# plot(wna_shp)

# BC
bc_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_shapefile") == T])
# plot(bc_shp)

sel_area_shpfl <- project(bc_shp, 'EPSG:3005')
region <- "BC"

# Define your lat/lon bounding box as an sf object
bbox_ll <- st_bbox(
  c(xmin = -140, xmax = -113, ymin = 47.5, ymax = 60.0),
  crs = 4326
)

# Convert to polygon, then transform to EPSG:3005
bbox_poly <- st_as_sfc(bbox_ll)
bbox_proj <- st_transform(bbox_poly, 3005)

# Extract the transformed bounding box (in meters)
bbox_proj_bounds <- st_bbox(bbox_proj)
xlim_proj <- c(bbox_proj_bounds["xmin"], bbox_proj_bounds["xmax"])
ylim_proj <- c(bbox_proj_bounds["ymin"], bbox_proj_bounds["ymax"])

xlim_proj[1] <- xlim_proj[1] + 500000

## Months, parameters ---------------------------------------
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

curr_mon_yr <- as.Date(
  paste0(update_year, update_month, "15"),
  format = "%Y%B%d"
)
curr_mon_yr
prvs12_mon_yr <- curr_mon_yr - 365
prvs12_mon_yr
prvs12_mon_yr_1 <- prvs12_mon_yr - 30

# prvs_12_mns <- seq.Date((curr_mon_yr-366),curr_mon_yr, by = "month")[2:13]
prvs_12_mns <- rev(seq(as.Date(curr_mon_yr), by = "-1 month", length.out = 12))

cur_mon_nam <- format(as.Date(curr_mon_yr), "%B %Y")
cur_yr_nam <- format(as.Date(curr_mon_yr), "%Y")
cur_mon_only <- format(as.Date(curr_mon_yr), "%B")
prv_mon_nam <- format(as.Date(prvs12_mon_yr_1), "%B %Y")

## Anomalies Data files -----------------------------------------
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
ano_dt_fl

## Climatology Data files -----------------------------------------
list.files(
  path = ano_dt_pth,
  pattern = ".*_clm_.*\\.nc",
  full.names = T
) -> clm_dt_fls
clm_dt_fls

clm_dt_fl <- tibble(dt_pth = clm_dt_fls)
clm_dt_fl %<>%
  mutate(
    par = str_extract(clm_dt_fls, paste(parameters, collapse = "|")),
    mon = str_extract(ano_dt_fls, paste(months_nam, collapse = "|"))
  )
clm_dt_fl

# Credit  -------------------------------
plt_wtrmrk <-
  "Created by Aseem Sharma (aseem.sharma@gov.bc.ca), BC Ministry of Forests. Data credit: ERA5land/C3S/ECMWF."
plt_wtrmrk


# Anomalies analysis and plots (Current month) --------------------------------------------------
months_nam
# #Colorspace
# hcl_palettes(plot = TRUE)
# hcl_palettes("Diverging",n=20,plot=T)
# divergingx_palettes(plot=T,n=21)

# Monthly anomaly calculations and plot function -------------------------------------------
# i <- 41
ano_mon_summary_plt_fun <- function(ano_dt_fl, current_month, parr) {
  # ano_dt_fl
  # current_month <- curr_mon_yr
  # parr <- 'tmean'

  ano_dt_fl %>%
    dplyr::filter(mon == format(as.Date(current_month), "%b")) -> ano_dt_fl_mn
  ano_dt_fl_mn
  monn <- unique(ano_dt_fl_mn$mon)

  ano_dt_fl_mn_par <- ano_dt_fl_mn %>%
    dplyr::filter(par == parr)

  #Read data for given month and parameter
  ano_dt_rast <- rast(ano_dt_fl_mn_par$dt_pth)
  names(ano_dt_rast)

  ano_dt_rast <- project(ano_dt_rast, 'EPSG:3005')

  # if(nlyr(ano_dt_rast) >75){
  #   ano_dt_rast <- subset(ano_dt_rast,1:74) #temporary due to soil files
  # }

  # Full name and units for parameters and months -----

  # Parameter full name and unit
  param_info <- dplyr::case_when(
    parr == "tmin" ~ c("Minimum temperature", "°C"),
    parr == "tmax" ~ c("Maximum temperature", "°C"),
    parr == "tmean" ~ c("Mean temperature", "°C"),
    parr == "prcp" ~ c("Precipitation", "mm"),
    parr == "rh" ~ c("Relative Humidity (RH)", "%"),
    parr == "vpd" ~ c("Vapor pressure deficit (VPD)", "kPa"),
    parr == "soil_moisture" ~ c("Volumetric soil moisture (0-1m)", "m³/m"),
    TRUE ~ c(parr, "")
  )
  parr_full <- param_info[1]
  unt <- param_info[2]

  # Full month name
  mon_full <- dplyr::case_when(
    monn == "annual" ~ "Annual",
    monn == "spring" ~ "Spring",
    monn == "summer" ~ "Summer",
    monn == "fall" ~ "Fall",
    monn == "winter" ~ "Winter",
    monn %in% month.abb ~ month.name[match(monn, month.abb)],
    TRUE ~ monn # fallback in case of unknown input
  )

  # Read data and create a monthly, 12 months and long term anomaly plot ----
  # Current month anomaly -----
  # Climatology
  clm_dt_fl %>%
    dplyr::filter(par == parr & mon == monn) -> clm_dt_fl_i

  clm_dt_rast <- rast(clm_dt_fl_i$dt_pth)
  clm_dt_rast
  # plot(clm_dt_rast)
  # plot(ano_dt_rast)

  clm_dt_rast <- project(clm_dt_rast, 'EPSG:3005')

  # Subset for current month
  ano_dt_rast_mn <- subset(
    ano_dt_rast,
    which(names(ano_dt_rast) %in% curr_mon_yr)
  )
  names(ano_dt_rast_mn) <- curr_mon_yr

  if (parr == 'prcp' | parr == 'soil_moisture') {
    ano_dt_rast_per1 <- (ano_dt_rast / clm_dt_rast) * 100
    #If prcp anomalies are very high ( > 200 %) then convert and limit to 200.
    ano_dt_rast_per2 <- ifel(ano_dt_rast_per1 > 201, 200, ano_dt_rast_per1)
    ano_dt_rast_per3 <- ifel(ano_dt_rast_per2 < -201, -200, ano_dt_rast_per2)
    ano_dt_rast <- ano_dt_rast_per3
  } else {
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
  minval <- (-1) * (max(abs(ano_rng_lmt), na.rm = T))
  maxval <- (1) * (max(abs(ano_rng_lmt), na.rm = T))
  names(ano_dt_rast_mn) <- curr_mon_yr

  mean_ano_val <- global(
    ano_dt_rast_mn,
    fun = "mean",
    na.rm = T
  )

  # Breaks and labels
  brk_neg <-
    ceiling(c(seq(minval, 0, length.out = 4)))
  brk_pos <-
    floor(c(seq(0, maxval, length.out = 4)))[-1]

  #create breaks with "00"

  if (nchar(abs(brk_neg[[1]])) == 4) {
    brk_negn <- plyr::round_any(brk_neg, 100, f = ceiling)
  } else if (nchar(abs(brk_neg[[1]])) == 3) {
    brk_negn <- plyr::round_any(brk_neg, 10, f = ceiling)
  } else if (nchar(abs(brk_neg[[1]])) == 2) {
    brk_negn <- plyr::round_any(brk_neg, 1, f = ceiling)
  } else if (nchar(abs(brk_neg[[1]])) == 1) {
    brk_negn <- plyr::round_any(brk_neg, 1, f = ceiling)
  }
  brk_negn

  if (nchar(abs(brk_neg[[1]])) == 4) {
    brk_posp <- plyr::round_any(brk_pos, 100, f = floor)
  } else if (nchar(abs(brk_neg[[1]])) == 3) {
    brk_posp <- plyr::round_any(brk_pos, 10, f = floor)
  } else if (nchar(abs(brk_pos[[1]])) == 2) {
    brk_posp <- plyr::round_any(brk_pos, 1, f = floor)
  } else if (nchar(abs(brk_pos[[1]])) == 1) {
    brk_posp <- plyr::round_any(brk_pos, 1, f = floor)
  }
  brk_posp

  brks_seq <- c(brk_negn, brk_posp)
  labels_val <- c(
    paste0("<", brks_seq[[1]]),
    brks_seq[[2]],
    brks_seq[[3]],
    brks_seq[[4]],
    brks_seq[[5]],
    brks_seq[[6]],
    paste0(">", brks_seq[[7]])
  )
  labels_val

  # Plot using terra rast
  # Climate plot title ( use log for prcp)
  # if (parr == "prcp" | parr == "soil_moisture") {
  #   par_title <-
  #     paste0(region,
  #            " ",
  #            parr_full,
  #            " anomaly (% of normal)",
  #            ": ",
  #            mon_full,
  #            " ", update_year
  #     )
  # } else {
  #   par_title <-
  #     paste0(region,
  #            " ",
  #            parr_full,
  #            " anomaly (",
  #            unt,
  #            ")",
  #            ": ",
  #            mon_full,
  #            " ", update_year)
  # }

  if (parr == "prcp" | parr == "soil_moisture") {
    par_title <-
      paste0(parr_full, "(% of normal)")
  } else {
    par_title <-
      paste0(parr_full, " (", unt, ")")
  }

  # For bc projection
  ano_dt_rast_mn

  spatial_ano_mon_plt <- ggplot() +
    geom_spatraster(data = ano_dt_rast_mn) +
    scale_fill_gradientn(
      name = paste0(parr_full, " anomaly ", unt),
      colours = cpt(pal = "ncl_BlWhRe", n = 100, rev = F),
      na.value = "transparent",
      limits = c(minval, maxval),
      breaks = brks_seq
    ) +
    # facet_wrap(. ~ lyr) +
    geom_sf(
      data = sel_area_shpfl,
      colour = "black",
      size = 1,
      fill = NA,
      alpha = 0.5
    ) +
    # scale_x_continuous(
    #   name =  "Longitude (°W) ",
    #   breaks = seq(xmi - 5, xmx + 5, 10),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # ) +
    # scale_y_continuous(
    #   name = "Latitude (°N) ",
    #   breaks = seq((ymi - 1), (ymx + 1), 6),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # ) +
    # coord_sf(xlim = xlim, ylim = ylim) +
    coord_sf(
      xlim = xlim_proj,
      ylim = ylim_proj,
      crs = 3005,
      expand = FALSE
    ) +
    theme(
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray60",
        linewidth = 0.02,
        linetype = "dashed"
      ),
      element_line(colour = "black", linewidth = 1),
      axis.line = element_line(colour = "gray70", linewidth = 0.08),
      axis.ticks.length = unit(-0.20, "cm"),
      axis.title.y = element_text(
        angle = 90,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = margin(t = 1, r = 1, b = 1, l = 1)
      ),
      axis.title.x = element_text(
        angle = 0,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = margin(t = 1, r = 1, b = 1, l = 1)
      ),
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),
      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),
      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 13,
        colour = "Black",
        hjust = 0.5,
        margin = margin(0, 0, 2, 0)
      ),
      legend.position = "right",
      legend.direction = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 16, margin = margin(t = 1)),

      # FACETS
      strip.text.x = element_text(size = 12, angle = 0),
      strip.text.y = element_text(size = 12, face = "bold"),
      strip.background = element_rect(color = "black", fill = "gray90"),
      strip.text = element_text(
        face = "bold",
        size = 18,
        colour = "black"
      )
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1.7,
        barheight = 20,
        label.vjust = 0.5,
        label.hjust = 0.0,
        title.vjust = 0.5,
        title.hjust = 0.5,
        title = NULL,
        # title.position = NULL,
        ticks.colour = 'black',
        # ticks.linewidth = 1,
        frame.colour = 'black',
        # frame.linewidth = 1,
        # draw.ulim = FALSE,
        # draw.llim = TRUE,
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  spatial_ano_mon_plt

  if (
    parr == "prcp" &
      maxval > 200 |
      parr == "soil_moisture" &
        maxval > 200 ||
      parr == "rh" & maxval > 200
  ) {
    spatial_ano_mon_plt <- spatial_ano_mon_plt +
      scale_fill_gradientn(
        name = paste0(parr_full, " anomaly ", unt),
        colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
        na.value = "transparent",
        limits = c(minval, maxval),
        breaks = brks_seq,
        labels = labels_val
      )
  } else if (
    parr == "prcp" |
      parr == "soil_moisture" |
      parr == "rh"
  ) {
    spatial_ano_mon_plt <- spatial_ano_mon_plt +
      scale_fill_gradientn(
        name = paste0(parr_full, "  anomaly (%) "),
        colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
        na.value = "transparent",
        limits = c(minval, maxval),
        breaks = brks_seq
      )
  }

  spatial_ano_mon_plt <- spatial_ano_mon_plt +
    labs(
      # tag = plt_wtrmrk,
      title = par_title
    ) +
    theme(
      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 18,
        hjust = 0.5, # center align
        colour = "Black",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
    ) +
    # theme(
    #   plot.tag.position = "bottom",
    #   plot.tag = element_text(
    #     color = 'gray50',
    #     hjust = 1,
    #     vjust = 0,
    #     size = 6
    #   )
    # )+
    theme_void()
  spatial_ano_mon_plt

  # Previous 12 months average anomaly ------------------------------
  ano_dt_fl %>%
    dplyr::filter(mon %in% format(as.Date(prvs_12_mns), "%b")) -> ano_dt_fl_12mn
  ano_dt_fl_12mn

  ano_dt_fl_12mn_par <- ano_dt_fl_12mn %>%
    dplyr::filter(par == parr)

  ano_dt_rast_i12_all <- rast(ano_dt_fl_12mn_par$dt_pth)
  ano_dt_rast_i12_all
  names(ano_dt_rast_i12_all)
  ano_dt_rast_i12 <- subset(
    ano_dt_rast_i12_all,
    which(
      as.Date(names(ano_dt_rast_i12_all), format = "%Y-%m-%d") %in% prvs_12_mns
    )
  )
  ano_dt_rast_i12
  ano_dt_rast_i12 <- project(ano_dt_rast_i12, 'EPSG:3005')

  #temporary fix
  if (nlyr(ano_dt_rast_i12) > 12) {
    ano_dt_rast_i12 <- subset(
      ano_dt_rast_i12,
      c(1, 5, 9, 13, 17, 21, 25, 27, 31, 35, 39, 43)
    )
  } else {
    ano_dt_rast_i12 <- ano_dt_rast_i12
  }

  rm(ano_dt_rast_i12_all)
  # plot(ano_dt_rast_i12)
  # 12 months average  value
  ano_dt_rast_i12_av <- app(ano_dt_rast_i12, 'mean')
  # plot(ano_dt_rast_i12_av)

  # Climatology
  clm_dt_fl %>%
    dplyr::filter(par == parr) -> clm_dt_fl_i

  clm_dt_rast <- rast(clm_dt_fl_i$dt_pth)
  clm_dt_rast
  names(clm_dt_rast) <- months_nam

  clm_dt_rast <- project(clm_dt_rast, 'EPSG:3005')

  clm_dt_rast_mon12 <-
    subset(
      clm_dt_rast,
      which(names(clm_dt_rast) %in% format(as.Date(prvs_12_mns), "%b"))
    )
  clm_dt_rast12 <- app(clm_dt_rast_mon12, 'mean')
  rm(clm_dt_rast_mon12)
  # plot(clm_dt_rast12)
  # plot(ano_dt_rast_i12_av)

  if (parr == 'prcp' | parr == 'soil_moisture') {
    ano_dt_rast_per1 <- (ano_dt_rast_i12_av / clm_dt_rast12) * 100
    #If prcp anomalies are very high ( > 200 %) then convert and limit to 200.
    ano_dt_rast_per2 <-
      ifel(ano_dt_rast_per1 > 201, 200, ano_dt_rast_per1)
    ano_dt_rast_per3 <-
      ifel(ano_dt_rast_per2 < -201, -200, ano_dt_rast_per2)
    ano_dt_rast_i12_av <- ano_dt_rast_per3
  } else {
    ano_dt_rast_i12_av <- ano_dt_rast_i12_av
  }
  # plot(ano_dt_rast,40:44)
  ano_dt_rast_i12_av

  # 12 months average  spatial anomaly plot
  ano_rng_lmt <- terra::minmax(ano_dt_rast_i12_av, compute = T)
  minval <- (-1) * (max(abs(ano_rng_lmt), na.rm = T))
  maxval <- (1) * (max(abs(ano_rng_lmt), na.rm = T))
  names(ano_dt_rast_mn) <- curr_mon_yr

  # Breaks and labels
  brk_neg <-
    ceiling(c(seq(minval, 0, length.out = 4)))
  brk_pos <-
    floor(c(seq(0, maxval, length.out = 4)))[-1]

  #create breaks with "00"

  if (nchar(abs(brk_neg[[1]])) == 4) {
    brk_negn <- plyr::round_any(brk_neg, 100, f = ceiling)
  } else if (nchar(abs(brk_neg[[1]])) == 3) {
    brk_negn <- plyr::round_any(brk_neg, 10, f = ceiling)
  } else if (nchar(abs(brk_neg[[1]])) == 2) {
    brk_negn <- plyr::round_any(brk_neg, 1, f = ceiling)
  } else if (nchar(abs(brk_neg[[1]])) == 1) {
    brk_negn <- plyr::round_any(brk_neg, 1, f = ceiling)
  }
  brk_negn

  if (nchar(abs(brk_neg[[1]])) == 4) {
    brk_posp <- plyr::round_any(brk_pos, 100, f = floor)
  } else if (nchar(abs(brk_neg[[1]])) == 3) {
    brk_posp <- plyr::round_any(brk_pos, 10, f = floor)
  } else if (nchar(abs(brk_pos[[1]])) == 2) {
    brk_posp <- plyr::round_any(brk_pos, 1, f = floor)
  } else if (nchar(abs(brk_pos[[1]])) == 1) {
    brk_posp <- plyr::round_any(brk_pos, 1, f = floor)
  }
  brk_posp

  brks_seq <- c(brk_negn, brk_posp)
  labels_val <- c(
    paste0("<", brks_seq[[1]]),
    brks_seq[[2]],
    brks_seq[[3]],
    brks_seq[[4]],
    brks_seq[[5]],
    brks_seq[[6]],
    paste0(">", brks_seq[[7]])
  )
  labels_val

  # Plot using terra rast
  # Climate plot title ( use log for prcp)
  # if (parr == "prcp" | parr == "soil_moisture") {
  #   par_title <-
  #     paste0(region,
  #            " ",
  #            parr_full,
  #            " anomaly (% of normal) ",
  #            "for ", format(as.Date(prvs12_mon_yr_1), "%b-%Y")," to ",
  #            format(as.Date(curr_mon_yr), "%b-%Y")
  #     )
  # } else {
  #   par_title <-
  #     paste0(region,
  #            " ",
  #            parr_full,
  #            " anomaly (",
  #            unt,
  #            ") ", "for ", format(as.Date(prvs12_mon_yr_1), "%b-%Y")," to ",
  #            format(as.Date(curr_mon_yr), "%b-%Y"))
  # }

  if (parr == "prcp" | parr == "soil_moisture") {
    par_title <-
      paste0(
        parr_full,
        " (% of normal)"
      )
  } else {
    par_title <-
      paste0(parr_full, "(", unt, ") ")
  }

  # For bc projection
  ano_dt_rast_i12_av

  ano_dt_rast_i12_av <- project(ano_dt_rast_i12_av, 'EPSG: 3005')

  spatial_ano_12mon_plt <- ggplot() +
    geom_spatraster(data = ano_dt_rast_i12_av) +
    scale_fill_gradientn(
      name = paste0(parr_full, " anomaly ", unt),
      colours = cpt(pal = "ncl_BlWhRe", n = 100, rev = F),
      na.value = "transparent",
      limits = c(minval, maxval),
      breaks = brks_seq
    ) +
    # facet_wrap(. ~ lyr) +
    geom_sf(
      data = sel_area_shpfl,
      colour = "black",
      size = 1,
      fill = NA,
      alpha = 0.5
    ) +
    coord_sf(
      xlim = xlim_proj,
      ylim = ylim_proj,
      crs = 3005,
      expand = FALSE
    ) +
    # scale_x_continuous(
    #   name =  "Longitude (°W) ",
    #   breaks = seq(xmi - 5, xmx + 5, 10),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # ) +
    # scale_y_continuous(
    #   name = "Latitude (°N) ",
    #   breaks = seq((ymi - 1), (ymx + 1), 6),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # )+
    # coord_sf(xlim = xlim, ylim = ylim)+
    theme(
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray60",
        linewidth = 0.02,
        linetype = "dashed"
      ),
      element_line(colour = "black", linewidth = 1),
      axis.line = element_line(colour = "gray70", linewidth = 0.08),
      axis.ticks.length = unit(-0.20, "cm"),
      axis.title.y = element_text(
        angle = 90,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      ),

      axis.title.x = element_text(
        angle = 0,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      ),

      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),

      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),

      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 13,
        colour = "Black"
      ),

      legend.position = "right",
      legend.direction = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 16),

      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 12, face = "bold"),

      strip.background = element_rect(color = "black", fill = "gray90"),
      strip.text = element_text(
        face = "bold",
        size = 18,
        colour = "black"
      )
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1.7,
        barheight = 20,
        label.vjust = 0.5,
        label.hjust = 0.0,
        title.vjust = 0.5,
        title.hjust = 0.5,
        title = NULL,
        # title.position = NULL,
        ticks.colour = 'black',
        # ticks.linewidth = 1,
        frame.colour = 'black',
        # frame.linewidth = 1,
        # draw.ulim = FALSE,
        # draw.llim = TRUE,
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  spatial_ano_12mon_plt

  if (
    parr == "prcp" &
      maxval > 200 |
      parr == "soil_moisture" &
        maxval > 200 ||
      parr == "rh" & maxval > 200
  ) {
    spatial_ano_12mon_plt <- spatial_ano_12mon_plt +
      scale_fill_gradientn(
        name = paste0(parr_full, " anomaly ", unt),
        colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
        na.value = "transparent",
        limits = c(minval, maxval),
        breaks = brks_seq,
        labels = labels_val
      )
  } else if (
    parr == "prcp" |
      parr == "soil_moisture" |
      parr == "rh"
  ) {
    spatial_ano_12mon_plt <- spatial_ano_12mon_plt +
      scale_fill_gradientn(
        name = paste0(parr_full, "  anomaly (%) "),
        colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
        na.value = "transparent",
        limits = c(minval, maxval),
        breaks = brks_seq
      )
  }
  spatial_ano_12mon_plt <- spatial_ano_12mon_plt +
    labs(
      # tag = plt_wtrmrk,
      title = par_title
    ) +
    theme(
      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 18,
        hjust = 0.5, # center align
        colour = "Black",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
    ) +
    # theme(
    #   plot.tag.position = "bottom",
    #   plot.tag = element_text(
    #     color = 'gray50',
    #     hjust = 1,
    #     vjust = 0,
    #     size = 6
    #   )
    # )+
    theme_void()
  spatial_ano_12mon_plt

  # Long term monthly trend  -----------------------------------
  ano_dt_rast

  # Clip for selected area :: BC
  ano_dt_shp_rast <-
    ano_dt_rast |>
    terra::crop(sel_area_shpfl, snap = "out") |>
    terra::mask(sel_area_shpfl, touches = TRUE)

  # Spatial trend --------------------------
  ano_dt_shp_rast
  #MK trend analysis
  mk_sig_cal_fun <-
    function(x) {
      m = MannKendall(x)
      as.numeric(m[2])
    } #gives p-value only
  mk_trn_mag_fun <- function(y) {
    se = zyp.trend.vector(
      y,
      x = 1:length(y),
      conf.intervals = TRUE,
      preserve.range.for.sig.test = TRUE
    )
    c(se[[2]])
  }

  ano_trn_sig <- app(ano_dt_rast, mk_sig_cal_fun)
  # ano_trn_sig
  # plot(ano_trn_sig)
  ano_trn_mag <- app(ano_dt_rast, mk_trn_mag_fun)
  # ano_trn_mag
  # plot(ano_trn_mag)

  # Stack trend magnigude and significance
  ano_sp_mk_trn_sig <- c(ano_trn_mag, ano_trn_sig)
  # plot(ano_sp_mk_trn_sig)
  names(ano_sp_mk_trn_sig) <- c("trnmag", "pval")

  #Raster to point conversion
  ano_sp_mk_trn_sig_dt <- as_tibble(ano_sp_mk_trn_sig, xy = T, na.rm = T)
  ano_sp_mk_trn_sig_dt

  # Trend plot
  range(ano_sp_mk_trn_sig_dt$trnmag, na.rm = T)
  summary(ano_sp_mk_trn_sig_dt$trnmag, na.rm = T)
  range(ano_sp_mk_trn_sig_dt$pval, na.rm = T)

  mean(ano_sp_mk_trn_sig_dt$trnmag, na.rm = T) * 74

  ano_dt_sig_trn <- ano_sp_mk_trn_sig_dt %>%
    dplyr::filter(pval <= 0.1)
  ano_dt_sig_trn

  mxtrn <- max(abs(ano_sp_mk_trn_sig_dt$trnmag), na.rm = T)
  mxtrn

  if (parr == "prcp" | parr == "soil_moisture") {
    par_title <-
      bquote(
        .(parr_full) ~ "trend:1950-" ~ .(cur_yr_nam) ~ "( % of normal" ~ yr^{
          -1
        } ~ ")"
      )
  } else {
    par_title <-
      bquote(
        .(parr_full) ~ "trend: 1950-" ~ .(cur_yr_nam) ~ "(" ~ .(unt) ~ yr^{
          -1
        } ~ ")"
      )
  }

  ano_dt_sp_trn_sig_plt <- ggplot() +
    geom_tile(
      data = ano_sp_mk_trn_sig_dt,
      aes(x = x, y = y, fill = trnmag),
      alpha = 1
    ) +
    # geom_spatraster(data = ano_trn_ano_trn_magmag)+
    scale_fill_continuous_diverging(
      palette = "Blue-Red2",
      n_interp = 21,
      limits = c(-mxtrn, mxtrn),
      # breaks=seq(-1.2, 1.2,0.3),
      # labels=seq(-0.8, 0.8,0.2),
      # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
      name = bquote(
        .(parr) ~ "trend" ~ yr^{
          -1
        }
      )
    ) +
    geom_point(
      data = ano_dt_sig_trn,
      aes(x = x, y = y),
      color = "Black",
      fill = "Gray10",
      alpha = 0.8,
      size = 0.3
    ) +
    geom_sf(
      data = sel_area_shpfl,
      colour = "black",
      size = 1,
      fill = NA,
      alpha = 0.5
    ) +
    # scale_x_continuous(
    #   name =  "Longitude (°W) ",
    #   breaks = seq(xmi - 5, xmx + 5, 10),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # ) +
    # scale_y_continuous(
    #   name = "Latitude (°N) ",
    #   breaks = seq((ymi - 1), (ymx + 1), 6),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # )+
    # coord_sf(xlim = xlim, ylim = ylim)+
    coord_sf(
      xlim = xlim_proj,
      ylim = ylim_proj,
      crs = 3005,
      expand = FALSE
    ) +
    theme(
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray60",
        linewidth = 0.02,
        linetype = "dashed"
      ),

      axis.line = element_line(colour = "gray70", linewidth = 0.08),
      axis.ticks.length = unit(-0.20, "cm"),

      element_line(colour = "black", linewidth = 1),

      axis.title.y = element_text(
        angle = 90,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      ),

      axis.title.x = element_text(
        angle = 0,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      ),

      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),

      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),

      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 13,
        colour = "Black"
      ),

      legend.position = "right",
      legend.direction = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 16),

      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 12, face = "bold"),

      strip.background = element_rect(color = "black", fill = "gray90"),
      strip.text = element_text(
        face = "bold",
        size = 18,
        colour = "black"
      )
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1.7,
        barheight = 20,
        label.vjust = 0.5,
        label.hjust = 0.0,
        title.vjust = 0.5,
        title.hjust = 0.5,
        title = NULL,
        # title.position = NULL,
        ticks.colour = 'black',
        # ticks.linewidth = 1,
        frame.colour = 'black',
        # frame.linewidth = 1,
        # draw.ulim = FALSE,
        # draw.llim = TRUE,
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  ano_dt_sp_trn_sig_plt <- ano_dt_sp_trn_sig_plt +
    labs(
      # tag = plt_wtrmrk,
      title = par_title
    ) +
    theme(plot.title = element_text(size = 18, margin = margin(0, 0, 0, 0))) +
    # theme(
    #   plot.tag.position = "bottom",
    #   plot.tag = element_text(
    #     color = 'gray50',
    #     hjust = 1,
    #     vjust = 0,
    #     size = 6
    #   )
    # )+
    theme_void()
  ano_dt_sp_trn_sig_plt

  # Time series trend ----------------------------------
  # plot(ano_dt_shp_rast,72)
  yr_df <- tibble(paryr = names(ano_dt_shp_rast))
  yr_df %<>%
    mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
  names(ano_dt_shp_rast) <- yr_df$yr
  # Shapefile spatial average anomalies by year
  ano_shp_av_dt <-
    tibble(rownames_to_column(
      global(
        ano_dt_shp_rast,
        fun = "mean",
        na.rm = T
      ),
      "yr"
    )) %>%
    dplyr::select(yr, ano = mean)
  ano_shp_dt <- ano_shp_av_dt %>%
    drop_na()
  ano_shp_dt
  ano_shp_dt %<>%
    mutate(ano_rank = rank(-rank(ano)))

  ano_shp_dt$yr <-
    as.numeric(str_extract(ano_shp_dt$yr, "[0-9]+"))
  ano_shp_dt$par <- parr
  ano_shp_dt$mon <- monn
  ano_shp_dt$region <- region
  ano_shp_dt

  # ggplot2/plotly trend plot  ----
  # Trend on average anomaly 1950 - now
  ano_shp_dt %<>%
    dplyr::filter(yr > 1950) %<>%
    mutate(
      # trnd =zyp.trend.vector(ano)[["trend"]],
      # incpt =zyp.trend.vector(ano)[["intercept"]],
      #sig = zyp.trend.vector(ano)[["sig"]])
      sig = round(MannKendall(ano)[[2]], digits = 2)
    )
  ano_shp_dt

  ano_mk_trnd <-
    zyp.sen(ano ~ yr, ano_shp_dt) ##Give the trend###
  ano_mk_trnd$coefficients
  ano_shp_dt$trn <- ano_mk_trnd$coeff[[2]]
  ano_shp_dt$incpt <- ano_mk_trnd$coeff[[1]]

  xs = c(min(ano_shp_dt$yr), max(ano_shp_dt$yr))
  trn_slp = c(unique(ano_shp_dt$incpt), unique(ano_shp_dt$trn))
  ys = cbind(1, xs) %*% trn_slp
  ano_shp_dt$trn_lab = paste(
    "italic(1950-~trend)==",
    round(ano_shp_dt$trn, 2),
    "~yr^{-1}~','~italic(p)==",
    round(ano_shp_dt$sig, 2)
  )
  # Trend on average anomaly 1980 - now
  ano_shp_dt %>%
    dplyr::filter(yr > 1979) %>%
    mutate(
      # trnd =zyp.trend.vector(ano)[["trend"]],
      # incpt =zyp.trend.vector(ano)[["intercept"]],
      #sig = zyp.trend.vector(ano)[["sig"]])
      sig = round(MannKendall(ano)[[2]], digits = 2)
    ) -> ano_shp_dt80
  ano_shp_dt80

  ano_mk_trnd80 <-
    zyp.sen(ano ~ yr, ano_shp_dt80) ##Give the trend###
  ano_mk_trnd80$coefficients
  ano_shp_dt80$trn <- ano_mk_trnd80$coeff[[2]]
  ano_shp_dt80$incpt <- ano_mk_trnd80$coeff[[1]]

  xs80 = c(min(ano_shp_dt80$yr), max(ano_shp_dt80$yr))
  trn_slp80 = c(unique(ano_shp_dt80$incpt), unique(ano_shp_dt80$trn))
  ys80 = cbind(1, xs80) %*% trn_slp80
  ano_shp_dt80$trn_lab = paste(
    "italic(1980-~trend)==",
    round(ano_shp_dt80$trn, 2),
    "~yr^{-1}~','~italic(p)==",
    round(ano_shp_dt80$sig, 2)
  )

  # anomaly plot
  ymin <- (-1) * (max(abs(ano_shp_dt$ano)))
  ymax <- (1) * (max(abs(ano_shp_dt$ano)))
  minyr <- min(ano_shp_dt$yr)
  maxyr <- max(ano_shp_dt$yr)

  if (ymax < 1) {
    ybrk_neg <-
      round(
        c(seq(
          (-1) *
            (max(
              abs(ano_shp_dt$ano)
            )),
          0,
          length.out = 2
        )),
        digits = 2
      )
    ybrk_neg
    ybrk_pos <-
      round(
        c(seq(
          0,
          (1) *
            (max(
              abs(ano_shp_dt$ano)
            )),
          length.out = 2
        ))[-1],
        digits = 2
      )
    ybrk_pos
  } else {
    ybrk_neg <-
      ceiling(c(seq(
        (-1) *
          (max(
            abs(ano_shp_dt$ano)
          )),
        0,
        length.out = 4
      )))
    ybrk_neg
    ybrk_pos <-
      floor(c(seq(
        0,
        (1) *
          (max(
            abs(ano_shp_dt$ano)
          )),
        length.out = 4
      )))[-1]
    ybrk_pos
  }
  #create breaks with "00"

  if (nchar(abs(ybrk_neg[[1]])) == 4) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 100, f = ceiling)
  } else if (nchar(abs(ybrk_neg[[1]])) == 3) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 10, f = ceiling)
  } else if (nchar(abs(ybrk_neg[[1]])) == 2) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 1, f = ceiling)
  } else if (nchar(abs(ybrk_neg[[1]])) == 1) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 1, f = ceiling)
  }
  ybrk_negn

  if (nchar(abs(ybrk_neg[[1]])) == 4) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 100, f = floor)
  } else if (nchar(abs(ybrk_neg[[1]])) == 3) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 10, f = floor)
  } else if (nchar(abs(ybrk_pos[[1]])) == 2) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 1, f = floor)
  } else if (nchar(abs(ybrk_pos[[1]])) == 1) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 1, f = floor)
  }
  ybrk_posp

  if (ymax < 1) {
    ybrks_seq <- c(ybrk_neg, ybrk_pos)
  } else {
    ybrks_seq <- c(ybrk_negn, ybrk_posp)
  }
  ybrks_seq
  # Positive and negative anomalies and 3 years moving average to create bar plot
  ano_shp_dt %<>%
    mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
    mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
  ano_shp_dt
  tail(ano_shp_dt)

  if (parr == "prcp" | parr == "soil_moisture") {
    par_title <- paste0(
      parr_full,
      " ",
      " (% of normal)"
    )
  } else {
    par_title <- paste0(parr_full, " ", " (", unt, ")")
  }

  if (parr == "prcp" | parr == "soil_moisture") {
    y_axis_lab <- paste0(parr, " average anomaly (% of normal)")
  } else {
    y_axis_lab <- paste0(parr, " average anomaly ", "(", unt, ")")
  }

  ano_shp_trn_plt <-
    ggplot(data = ano_shp_dt, aes(x = yr, y = ano)) +
    # annotate(
    #   geom = 'text',
    #   label = plt_wtrmrk,
    #   x = Inf,
    #   y = -Inf,
    #   hjust = 1,
    #   vjust = -0.5,
    #   color = 'gray80',
    #   size = 3.0
    # ) +
    geom_bar(
      stat = "identity",
      aes(fill = ano),
      width = 0.7,
      show.legend = FALSE
    ) +
    geom_hline(
      yintercept = 0,
      color = "gray10",
      linewidth = 0.5
    ) +
    scale_fill_gradientn(
      name = paste0(parr, " anomaly ", unt),
      colours = cpt(pal = "ncl_BlWhRe", n = 100, rev = F),
      limits = c(ymin, ymax),
      breaks = ybrks_seq
    ) +
    geom_line(
      aes(y = ano_mv, color = "3-yrs moving mean"),
      linewidth = 1.1,
      alpha = 0.7,
      na.rm = T
    ) +
    # geom_point(color = "blue", size = 2) +
    geom_segment(
      aes(
        x = xs[[1]],
        xend = xs[[2]],
        y = ys[[1]],
        yend = ys[[2]],
        color = "1950-trend"
      ),
      linetype = "dashed",
      linewidth = 0.9
    ) +
    geom_label(
      aes(x = xs[[1]] + 20),
      color = 'black',
      y = ymax - 0.05,
      fill = NA,
      label = ano_shp_dt$trn_lab[[1]],
      size = 4.0,
      parse = T
    ) +
    # add 80s trend
    geom_segment(
      aes(
        x = xs80[[1]],
        xend = xs80[[2]],
        y = ys80[[1]],
        yend = ys80[[2]],
        color = "1980-trend"
      ),
      linetype = "solid",
      linewidth = 0.9
    ) +
    geom_label(
      aes(x = xs[[1]] + 38),
      y = ymax - 0.05,
      fill = NA,
      color = 'deepskyblue2',
      label = ano_shp_dt80$trn_lab[[1]],
      size = 4.0,
      parse = TRUE
    ) +
    scale_x_continuous(
      name = " ",
      breaks = seq(1950, maxyr, 5),
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      name = y_axis_lab,
      limits = c(ymin, ymax),
      breaks = ybrks_seq
    ) +
    labs(title = par_title) +
    scale_color_manual(
      " ",
      values = c(
        "3-yrs moving mean" = "green",
        "1950-trend" = "black",
        "1980-trend" = "deepskyblue2"
      ),
      labels = c(
        "3-yrs moving mean" = "3-yrs moving mean",
        "1950-trend" = "1950-trend",
        "1980-trend" = "1980-trend"
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray75",
        linewidth = 0.05,
        linetype = "dashed"
      ),
      axis.line = element_line(colour = "black", linewidth = 1),
      axis.ticks.length = unit(-0.20, "cm"),
      element_line(colour = "black", linewidth = 1),

      axis.title.y = element_text(
        angle = 90,
        face = "plain",
        size = 13,
        colour = "Black",
        margin = unit(c(1, 1, 1, 1), "mm")
      ),
      axis.title.x = element_text(
        angle = 0,
        face = "plain",
        size = 13,
        colour = "Black",
        margin = unit(c(1, 1, 1, 1), "mm")
      ),

      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 12,
        margin = margin(2, 2, 2, 2)
      ),
      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 12,
        margin = margin(2, 2, 2, 2)
      ),

      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 14,
        colour = "Black"
      ),

      legend.position = c(0.5, 0.08),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = NA, color = "black"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 13),

      legend.text = element_text(size = 13, margin = margin(t = 0)),

      strip.text.x = element_text(size = 12, angle = 0),
      strip.text.y = element_text(size = 12, face = "bold"),

      axis.text = element_text(margin = margin(0, 0, 0, 0)),

      strip.background = element_rect(fill = "black"),
      strip.text = element_text(colour = "Black")
    )
  ano_shp_trn_plt

  if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
    ano_shp_trn_plt <- ano_shp_trn_plt +
      scale_fill_gradientn(
        name = paste0(parr, "  anomaly ", unt),
        colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
        limits = c(ymin, ymax),
        breaks = ybrks_seq
      )
  }
  ano_shp_trn_plt

  # Summary table for current month anomalies and spatial trends -----------------------
  #  anomaly
  ano_dt_shp_rast_cur_mon <- subset(
    ano_dt_shp_rast,
    which(names(ano_dt_shp_rast) %in% update_year)
  )
  # plot(ano_dt_shp_rast_cur_mon)
  cur_mon_ano_rng <- terra::minmax(ano_dt_shp_rast_cur_mon, compute = T)

  cur_mon_ano_sum_tab <- tibble(rownames_to_column(global(
    ano_dt_shp_rast_cur_mon,
    fun = "mean",
    na.rm = T
  ))) %>%
    dplyr::select(mean_ano = mean) %>%
    mutate(mean_ano = round(mean_ano, digits = 2))

  cur_mon_ano_sum_tab$min_ano <- round(cur_mon_ano_rng[1], digits = 2)
  cur_mon_ano_sum_tab$max_ano <- round(cur_mon_ano_rng[2], digits = 2)

  # Spatial Trend
  ano_trn_mag_shp <-
    ano_trn_mag |>
    terra::crop(sel_area_shpfl, snap = "out") |>
    terra::mask(sel_area_shpfl, touches = TRUE)
  # plot(ano_trn_mag_shp)
  cur_mon_trn_rng <- terra::minmax(ano_trn_mag_shp, compute = T)

  cur_mon_trn_sum_tab <- tibble(rownames_to_column(global(
    ano_trn_mag_shp,
    fun = "mean",
    na.rm = T
  ))) %>%
    dplyr::select(mean_trn = mean) %>%
    mutate(mean_trn = round(mean_trn, digits = 2))

  cur_mon_trn_sum_tab$min_trn <- round(cur_mon_trn_rng[1], digits = 2)
  cur_mon_trn_sum_tab$max_trn <- round(cur_mon_trn_rng[2], digits = 2)

  cur_mon_ano_trn_sum_tab <- bind_cols(cur_mon_ano_sum_tab, cur_mon_trn_sum_tab)

  # linear trend
  cur_mon_ano_trn_sum_tab$lin_trn <- round(ano_shp_dt$trn[1], digits = 2)
  cur_mon_ano_trn_sum_tab$lin_trn_pval <- round(ano_shp_dt$sig[1], digits = 2)

  #annual anomaly ranking
  rank_no <- ano_shp_dt %>%
    filter(yr == max(yr))
  cur_mon_ano_trn_sum_tab$ano_rnk_pos <- round(rank_no$ano_rank, digits = 2)
  cur_mon_ano_trn_sum_tab$mon <- update_month
  cur_mon_ano_trn_sum_tab$yr <- update_year
  cur_mon_ano_trn_sum_tab$par <- parr
  cur_mon_ano_trn_sum_tab

  # Final output list ---------------------

  # To include in the list with name
  plts <-
    list(
      spatial_ano_mon_plt,
      spatial_ano_12mon_plt,
      ano_dt_sp_trn_sig_plt,
      ano_shp_trn_plt,
      cur_mon_ano_trn_sum_tab
    )
  names(plts) <-
    c(
      paste0(parr, "_", monn, "_sp_ano"),
      paste0(parr, "_", monn, "_sp_ano_12mn"),
      paste0(parr, "_", monn, "_sp_ano_trn"),
      paste0(parr, "_", monn, "_ano_trn_lngtrm"),
      paste0(parr, "_", monn, "_ano_trn_summary_table")
    )

  return(plts)
}


# Merge all plots and save for each parameters ----------------------------------------

tmp_mon_plt_lst <- ano_mon_summary_plt_fun(
  ano_dt_fl,
  current_month = curr_mon_yr,
  parr = 'tmean'
)
names(tmp_mon_plt_lst)

vpd_mon_plt_lst <- ano_mon_summary_plt_fun(
  ano_dt_fl,
  current_month = curr_mon_yr,
  parr = 'vpd'
)
names(vpd_mon_plt_lst)

prcp_mon_plt_lst <- ano_mon_summary_plt_fun(
  ano_dt_fl,
  current_month = curr_mon_yr,
  parr = 'prcp'
)
names(prcp_mon_plt_lst)

rh_mon_plt_lst <- ano_mon_summary_plt_fun(
  ano_dt_fl,
  current_month = curr_mon_yr,
  parr = 'rh'
)
names(rh_mon_plt_lst)

sm_mon_plt_lst <- ano_mon_summary_plt_fun(
  ano_dt_fl,
  current_month = curr_mon_yr,
  parr = 'soil_moisture'
)
names(sm_mon_plt_lst)


# Update month anomaly Summary table ----------------------------------------------------------

clm_sum_tab_f <- bind_rows(
  tmp_mon_plt_lst[[5]],
  prcp_mon_plt_lst[[5]],
  vpd_mon_plt_lst[[5]],
  rh_mon_plt_lst[[5]],
  sm_mon_plt_lst[[5]]
)

clm_sum_tab_f %<>%
  dplyr::select(-c(yr, mon)) %<>%
  mutate(ano_rnk_pos = as.integer(ano_rnk_pos))
clm_sum_tab_f

clm_sum_tab_f %<>%
  dplyr::select(
    Parameter = par,
    'Spatial average anomaly' = mean_ano,
    'Spatial minimum anomaly' = min_ano,
    'Spatial maximum anomaly' = max_ano,
    'Average anomaly ranking' = ano_rnk_pos,
    'Spatial average trend' = mean_trn,
    'Spatial minimum trend' = min_trn,
    'Spatial maximum trend' = min_trn,
    'Linear trend' = lin_trn,
    'Linear trend p-val' = lin_trn_pval
  )

clm_sum_tab_f %>%
  pivot_longer(cols = c(-Parameter), names_to = "nam", values_to = 'val') %>%
  pivot_wider(names_from = "Parameter", values_from = 'val') %>%
  dplyr::select(
    'Measures' = nam,
    'Mean Temperature' = tmean,
    'Precipitation' = prcp,
    'Vapour pressure deficiet (VPD)' = vpd,
    'Relative Humidity' = rh,
    'Soil moisture' = soil_moisture
  ) -> clm_sum_tab_ff

write_csv(
  clm_sum_tab_ff,
  paste0(results_pth, update_month, '_', update_year, '_bc_climate_summary.csv')
)

# Final plots save -----------------------------------------------
## Mean temperature and vapor pressure deficit (VPD) ---------------------------------
### current month anomaly plot ------------------------------
tmp_vpd_ano_plt_f <- tmp_mon_plt_lst[[1]] + vpd_mon_plt_lst[[1]]

tmp_vpd_ano_plt_f <- tmp_vpd_ano_plt_f +
  plot_annotation(
    title = paste0("Anomalies for ", cur_mon_nam),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_ano_plt_f

ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_tmean_vpd_anomalies.png'
  ),
  plot = tmp_vpd_ano_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 810,
  scale = 0.7,
  limitsize = F
)

### Last 12 months anomaly: `r prv_mon_nam` to `r cur_mon_nam` anomaly plot --------------------

tmp_vpd_ano_12plt_f <- tmp_mon_plt_lst[[2]] + vpd_mon_plt_lst[[2]]
tmp_vpd_ano_12plt_f <- tmp_vpd_ano_12plt_f +
  plot_annotation(
    title = paste0("Average anomalies for ", prv_mon_nam, " to ", cur_mon_nam),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_ano_12plt_f

ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_tmean_vpd_last12months_anomalies.png'
  ),
  plot = tmp_vpd_ano_12plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Long term trends ------------------------------------

#### Spatial trends: `r format(as.Date(curr_mon_yr), "%B")` 1950 - `r update_year`
tmp_vpd_sp_ano_trn_plt_f <- tmp_mon_plt_lst[[3]] + vpd_mon_plt_lst[[3]]

tmp_vpd_sp_ano_trn_plt_f <- tmp_vpd_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0(
      "Spatial trends on ",
      cur_mon_only,
      " anomalies, 1950 - ",
      update_year
    ),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_tmean_vpd_longterm_spatial_trend.png'
  ),
  plot = tmp_vpd_sp_ano_trn_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Long-term spatially average trend ----------------------------------------
#Time series trends: `r format(as.Date(curr_mon_yr), "%B")` 1950 - `r update_year`

tmp_vpd_lngtrn_plt_f <- ((tmp_mon_plt_lst[[4]] +
  theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((vpd_mon_plt_lst[[4]]) +
    theme(axis.title.y = element_blank())))

tmp_vpd_lngtrn_plt_f <- tmp_vpd_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      cur_mon_only,
      " spatially averaged anomalies for BC 1950 - ",
      cur_yr_nam
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_lngtrn_plt_f


ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_tmean_vpd_bc_timeseries_trend.png'
  ),
  plot = tmp_vpd_lngtrn_plt_f,
  width = 19,
  height = 13,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


## Precipitation, Relative Humidity (RH) and soil moisture ------------------------------------------------------
### current month anomaly plot ---------------------------------------------------------
prcp_rh_sm_ano_plt_f <- prcp_mon_plt_lst[[1]] +
  rh_mon_plt_lst[[1]] +
  sm_mon_plt_lst[[1]] +
  plot_layout(ncol = 2)

prcp_rh_sm_ano_plt_f <- prcp_rh_sm_ano_plt_f +
  plot_annotation(
    title = paste0("Anomalies for ", cur_mon_nam),
    # subtitle = 'Baseline:1981-2010', caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )

prcp_rh_sm_ano_plt_f

ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_prcp_rh_sm_anomalies.png'
  ),
  plot = prcp_rh_sm_ano_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Last 12 months anomaly: `r prv_mon_nam` to `r cur_mon_nam` anomaly plot --------------------

prcp_rh_sm_ano_12plt_f <- prcp_mon_plt_lst[[2]] +
  rh_mon_plt_lst[[2]] +
  sm_mon_plt_lst[[2]] +
  plot_layout(ncol = 2)

prcp_rh_sm_ano_12plt_f <- prcp_rh_sm_ano_12plt_f +
  plot_annotation(
    title = paste0("Anomalies for ", prv_mon_nam, " to ", cur_mon_nam),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_ano_12plt_f


ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_prcp_rh_sm_last12months_anomalies.png'
  ),
  plot = prcp_rh_sm_ano_12plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Long term trends ------------------------------------

prcp_rh_sm_sp_ano_trn_plt_f <-
  prcp_mon_plt_lst[[3]] +
  rh_mon_plt_lst[[3]] +
  sm_mon_plt_lst[[3]] +
  plot_layout(ncol = 2)

prcp_rh_sm_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0(
      "Spatial trends on",
      cur_mon_only,
      " anomalies, 1950 - ",
      update_year
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_prcp_rh_sm_longterm_spatial_trend.png'
  ),
  plot = prcp_rh_sm_sp_ano_trn_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Long-term spatially average trend ----------------------------------------
#Time series trends: `r format(as.Date(curr_mon_yr), "%B")` 1950 - `r update_year`
prcp_rh_sm_lngtrn_plt_f <-
  ((prcp_mon_plt_lst[[4]]) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((rh_mon_plt_lst[[4]]) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((sm_mon_plt_lst[[4]]) +
    theme(axis.title.y = element_blank()))
prcp_rh_sm_lngtrn_plt_f

prcp_rh_sm_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      cur_mon_only,
      " spatially averaged anomalies for BC 1950 - ",
      cur_yr_nam
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_lngtrn_plt_f

ggsave(
  paste0(
    results_pth,
    update_month,
    '_',
    update_year,
    '_prcp_rh_sm_bc_timeseries_trend.png'
  ),
  plot = prcp_rh_sm_lngtrn_plt_f,
  width = 19,
  height = 17,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


# BC seasonal and annual climate trends -----------------------------------------------------
# Seasonal and annual long term spatial and linear trend function -----------------------------------
ano_dt_fl

ano_sea_ann_lngtrn_plt_fun <- function(ano_dt_fl, sea, parr) {
  # sea <- c("annual","winter","spring","summer", "fall")
  # parrs <- c("tmean", "tmax", "tmin", "prcp","vpd","rh","soil_moisture")
  # sea <- "fall"
  # parr <- c("tmean")

  ano_dt_fl %>%
    dplyr::filter(mon == sea) -> ano_dt_fl_mn
  ano_dt_fl_mn
  monn <- unique(ano_dt_fl_mn$mon)

  # Full name and units for parameters and months -----

  # Parameter full name and unit
  param_info <- dplyr::case_when(
    parr == "tmin" ~ c("Minimum temperature", "°C"),
    parr == "tmax" ~ c("Maximum temperature", "°C"),
    parr == "tmean" ~ c("Mean temperature", "°C"),
    parr == "prcp" ~ c("Precipitation", "mm"),
    parr == "rh" ~ c("Relative Humidity (RH)", "%"),
    parr == "vpd" ~ c("Vapor pressure deficit (VPD)", "kPa"),
    parr == "soil_moisture" ~ c("Volumetric soil moisture (0-1m)", "m³/m"),
    TRUE ~ c(parr, "")
  )
  parr_full <- param_info[1]
  unt <- param_info[2]

  # Full month name
  mon_full <- dplyr::case_when(
    monn == "annual" ~ "Annual",
    monn == "spring" ~ "Spring",
    monn == "summer" ~ "Summer",
    monn == "fall" ~ "Fall",
    monn == "winter" ~ "Winter",
    monn %in% month.abb ~ month.name[match(monn, month.abb)],
    TRUE ~ monn # fallback in case of unknown input
  )

  # Filter and read data
  ano_dt_fl_mn_par <- ano_dt_fl_mn %>%
    dplyr::filter(par == parr)

  #Read data for given month and parameer
  ano_dt_rast <- rast(ano_dt_fl_mn_par$dt_pth)
  names(ano_dt_rast)
  ano_dt_rast <- project(ano_dt_rast, 'EPSG:3005')
  ano_dt_rast <- crop(
    ano_dt_rast,
    ext(xlim_proj[1], xlim_proj[2], ylim_proj[1], ylim_proj[2])
  )
  # plot(ano_dt_rast,1)
  max_yr_sea <- parse_number(names(ano_dt_rast)[nlyr(ano_dt_rast)])

  # Create a monthly, 12 months and long term anomaly plot ----
  # Climatology
  clm_dt_fl %>%
    dplyr::filter(par == parr) -> clm_dt_fl_i

  clm_dt_rast <- rast(clm_dt_fl_i$dt_pth)
  clm_dt_rast
  names(clm_dt_rast) <- months_nam

  clm_dt_rast <- project(clm_dt_rast, 'EPSG:3005')
  clm_dt_rast <- crop(
    clm_dt_rast,
    ext(xlim_proj[1], xlim_proj[2], ylim_proj[1], ylim_proj[2])
  )

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
  } else {
    ano_dt_rast <- ano_dt_rast
  }
  # plot(ano_dt_rast,40:44)
  ano_dt_rast

  # Long term seasonal trend : spatial  ---------------------------
  ano_dt_rast

  #MK trend analysis
  mk_sig_cal_fun <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    m <- MannKendall(x)
    m$sl # p-value
  }

  mk_trn_mag_fun <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    res <- zyp.trend.vector(
      x,
      x = seq_along(x),
      conf.intervals = FALSE,
      preserve.range.for.sig.test = TRUE
    )
    res[["trend"]]
  }

  ano_trn_sig <- app(ano_dt_rast, mk_sig_cal_fun)
  # ano_trn_sig
  # plot(ano_trn_sig)
  ano_trn_mag <- app(ano_dt_rast, mk_trn_mag_fun)
  # ano_trn_mag
  # plot(ano_trn_mag)

  # Stack trend magnigude and significance
  ano_sp_mk_trn_sig <- c(ano_trn_mag, ano_trn_sig)
  # plot(ano_sp_mk_trn_sig)
  names(ano_sp_mk_trn_sig) <- c("trnmag", "pval")

  #Raster to point conversion
  ano_sp_mk_trn_sig_dt <- as_tibble(ano_sp_mk_trn_sig, xy = T, na.rm = T)
  ano_sp_mk_trn_sig_dt

  # Trend plot
  ano_dt_sig_trn <- ano_sp_mk_trn_sig_dt %>%
    dplyr::filter(pval <= 0.1)
  ano_dt_sig_trn

  mxtrn <- max(abs(ano_sp_mk_trn_sig_dt$trnmag), na.rm = T)
  mxtrn

  if (parr == "prcp" | parr == "soil_moisture") {
    par_title <-
      bquote(
        .(parr_full) ~ "trend:1950-" ~ .(cur_yr_nam) ~ "( % of normal" ~ yr^{
          -1
        } ~ ")"
      )
  } else {
    par_title <-
      bquote(
        .(parr_full) ~ "trend: 1950-" ~ .(cur_yr_nam) ~ "(" ~ .(unt) ~ yr^{
          -1
        } ~ ")"
      )
  }

  ano_dt_sp_trn_sig_plt <- ggplot() +
    geom_tile(
      data = ano_sp_mk_trn_sig_dt,
      aes(x = x, y = y, fill = trnmag),
      alpha = 1
    ) +
    # geom_spatraster(data = ano_trn_ano_trn_magmag)+
    scale_fill_continuous_diverging(
      palette = "Blue-Red2",
      n_interp = 21,
      limits = c(-mxtrn, mxtrn),
      # breaks=seq(-1.2, 1.2,0.3),
      # labels=seq(-0.8, 0.8,0.2),
      # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
      name = bquote(
        .(parr) ~ "trend" ~ yr^{
          -1
        }
      )
    ) +
    geom_point(
      data = ano_dt_sig_trn,
      aes(x = x, y = y),
      color = "Black",
      fill = "Gray10",
      alpha = 0.8,
      size = 0.3
    ) +
    geom_sf(
      data = sel_area_shpfl,
      colour = "black",
      size = 1,
      fill = NA,
      alpha = 0.5
    ) +
    # scale_x_continuous(
    #   name =  "Longitude (°W) ",
    #   breaks = seq(xmi - 5, xmx + 5, 10),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # ) +
    # scale_y_continuous(
    #   name = "Latitude (°N) ",
    #   breaks = seq((ymi - 1), (ymx + 1), 6),
    #   labels = abs,
    #   expand = c(0.01, 0.01)
    # )+
    # coord_sf(xlim = xlim, ylim = ylim)+
    coord_sf(
      xlim = xlim_proj,
      ylim = ylim_proj,
      crs = 3005,
      expand = FALSE
    ) +
    theme(
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray60",
        linewidth = 0.02,
        linetype = "dashed"
      ),
      axis.line = element_line(colour = "gray70", linewidth = 0.08),
      axis.ticks.length = unit(-0.20, "cm"),
      element_line(colour = "black", linewidth = 1),

      axis.title.y = element_text(
        angle = 90,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = unit(c(0, 0, 0, 0), "mm")
      ),
      axis.title.x = element_text(
        angle = 0,
        face = "plain",
        size = 15,
        colour = "Black",
        margin = unit(c(0, 0, 0, 0), "mm")
      ),

      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(2, 2, 2, 2)
      ),
      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        colour = "black",
        size = 14,
        margin = margin(2, 2, 2, 2)
      ),

      plot.title = element_text(
        angle = 0,
        face = "bold",
        size = 13,
        colour = "Black"
      ),

      legend.position = "right",
      legend.direction = "vertical",
      legend.margin = margin(0, 0, 0, 0),

      legend.box.margin = margin(0, 0, 0, 0),

      legend.title = element_text(size = 15),

      legend.text = element_text(size = 16, margin = margin(t = 0)),

      strip.text.x = element_text(size = 12, angle = 0),
      strip.text.y = element_text(size = 12, face = "bold"),

      axis.text = element_text(margin = margin(0, 0, 0, 0)),

      strip.background = element_rect(color = "black", fill = "gray90"),
      strip.text = element_text(
        face = "bold",
        size = 18,
        colour = "black"
      )
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1.7,
        barheight = 20,
        label.vjust = 0.5,
        label.hjust = 0.0,
        title.vjust = 0.5,
        title.hjust = 0.5,
        title = NULL,
        # title.position = NULL,
        ticks.colour = 'black',
        # ticks.linewidth = 1,
        frame.colour = 'black',
        # frame.linewidth = 1,
        # draw.ulim = FALSE,
        # draw.llim = TRUE,
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  ano_dt_sp_trn_sig_plt <- ano_dt_sp_trn_sig_plt +
    labs(
      # tag = plt_wtrmrk,
      title = par_title
    ) +
    theme(plot.title = element_text(size = 18, margin = margin(0, 0, 0, 0))) +
    # theme(
    #   plot.tag.position = "bottom",
    #   plot.tag = element_text(
    #     color = 'gray50',
    #     hjust = 1,
    #     vjust = 0,
    #     size = 6
    #   )
    # )+
    theme_void()
  ano_dt_sp_trn_sig_plt

  # ggsave(paste0(getwd(),"/plots/",parr_full,"_",sea,"_spatial_trend_1950_2023.png"),
  #        plot = ano_dt_sp_trn_sig_plt,
  #        width = 10,
  #        height =6,
  #        units = "in",
  #        dpi = 300,
  #        scale = 0.9,
  #        limitsize = F
  # )

  # Clip for selected area :: BC
  ano_dt_shp_rast <-
    ano_dt_rast |>
    terra::crop(sel_area_shpfl, snap = "out") |>
    terra::mask(sel_area_shpfl, touches = TRUE)

  # plot(ano_dt_shp_rast)
  ano_dt_shp_rast

  # plot(ano_dt_shp_rast,72)
  yr_df <- tibble(paryr = names(ano_dt_shp_rast))
  yr_df %<>%
    mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
  names(ano_dt_shp_rast) <- yr_df$yr
  # Shapefile spatial average anomalies by year
  ano_shp_av_dt <-
    tibble(rownames_to_column(
      global(
        ano_dt_shp_rast,
        fun = "mean",
        na.rm = T
      ),
      "yr"
    )) %>%
    dplyr::select(yr, ano = mean)
  ano_shp_dt <- ano_shp_av_dt %>%
    drop_na()
  ano_shp_dt
  ano_shp_dt$yr <-
    as.numeric(str_extract(ano_shp_dt$yr, "[0-9]+"))
  ano_shp_dt$par <- parr
  ano_shp_dt$mon <- monn
  ano_shp_dt$region <- region
  ano_shp_dt

  ano_shp_dt %<>%
    mutate(ano_rank = rank(-rank(ano)))

  # ggplot2/plotly trend plot  ----
  # Trend on average anomaly 1950 - now
  ano_shp_dt %<>%
    dplyr::filter(yr > 1950) %<>%
    mutate(
      # trnd =zyp.trend.vector(ano)[["trend"]],
      # incpt =zyp.trend.vector(ano)[["intercept"]],
      #sig = zyp.trend.vector(ano)[["sig"]])
      sig = round(MannKendall(ano)[[2]], digits = 2)
    )
  ano_shp_dt

  ano_mk_trnd <-
    zyp.sen(ano ~ yr, ano_shp_dt) ##Give the trend###
  ano_mk_trnd$coefficients
  ano_shp_dt$trn <- ano_mk_trnd$coeff[[2]]
  ano_shp_dt$incpt <- ano_mk_trnd$coeff[[1]]

  xs = c(min(ano_shp_dt$yr), max(ano_shp_dt$yr))
  trn_slp = c(unique(ano_shp_dt$incpt), unique(ano_shp_dt$trn))
  ys = cbind(1, xs) %*% trn_slp
  ano_shp_dt$trn_lab = paste(
    "italic(1950-~trend)==",
    round(ano_shp_dt$trn, 2),
    "~yr^{-1}~','~italic(p)==",
    round(ano_shp_dt$sig, 2)
  )
  # Trend on average anomaly 1980 - now
  ano_shp_dt %>%
    dplyr::filter(yr > 1979) %>%
    mutate(
      # trnd =zyp.trend.vector(ano)[["trend"]],
      # incpt =zyp.trend.vector(ano)[["intercept"]],
      #sig = zyp.trend.vector(ano)[["sig"]])
      sig = round(MannKendall(ano)[[2]], digits = 2)
    ) -> ano_shp_dt80
  ano_shp_dt80

  ano_mk_trnd80 <-
    zyp.sen(ano ~ yr, ano_shp_dt80) ##Give the trend###
  ano_mk_trnd80$coefficients
  ano_shp_dt80$trn <- ano_mk_trnd80$coeff[[2]]
  ano_shp_dt80$incpt <- ano_mk_trnd80$coeff[[1]]

  xs80 = c(min(ano_shp_dt80$yr), max(ano_shp_dt80$yr))
  trn_slp80 = c(unique(ano_shp_dt80$incpt), unique(ano_shp_dt80$trn))
  ys80 = cbind(1, xs80) %*% trn_slp80
  ano_shp_dt80$trn_lab = paste(
    "italic(1980-~trend)==",
    round(ano_shp_dt80$trn, 2),
    "~yr^{-1}~','~italic(p)==",
    round(ano_shp_dt80$sig, 2)
  )

  # anomaly plot
  ymin <- (-1) * (max(abs(ano_shp_dt$ano)))
  ymax <- (1) * (max(abs(ano_shp_dt$ano)))
  minyr <- min(ano_shp_dt$yr)
  maxyr <- max(ano_shp_dt$yr)

  if (ymax < 1) {
    ybrk_neg <-
      round(
        c(seq(
          (-1) *
            (max(
              abs(ano_shp_dt$ano)
            )),
          0,
          length.out = 2
        )),
        digits = 2
      )
    ybrk_neg
    ybrk_pos <-
      round(
        c(seq(
          0,
          (1) *
            (max(
              abs(ano_shp_dt$ano)
            )),
          length.out = 2
        ))[-1],
        digits = 2
      )
    ybrk_pos
  } else {
    ybrk_neg <-
      ceiling(c(seq(
        (-1) *
          (max(
            abs(ano_shp_dt$ano)
          )),
        0,
        length.out = 4
      )))
    ybrk_neg
    ybrk_pos <-
      floor(c(seq(
        0,
        (1) *
          (max(
            abs(ano_shp_dt$ano)
          )),
        length.out = 4
      )))[-1]
    ybrk_pos
  }
  #create breaks with "00"

  if (nchar(abs(ybrk_neg[[1]])) == 4) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 100, f = ceiling)
  } else if (nchar(abs(ybrk_neg[[1]])) == 3) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 10, f = ceiling)
  } else if (nchar(abs(ybrk_neg[[1]])) == 2) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 1, f = ceiling)
  } else if (nchar(abs(ybrk_neg[[1]])) == 1) {
    ybrk_negn <- plyr::round_any(ybrk_neg, 1, f = ceiling)
  }
  ybrk_negn

  if (nchar(abs(ybrk_neg[[1]])) == 4) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 100, f = floor)
  } else if (nchar(abs(ybrk_neg[[1]])) == 3) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 10, f = floor)
  } else if (nchar(abs(ybrk_pos[[1]])) == 2) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 1, f = floor)
  } else if (nchar(abs(ybrk_pos[[1]])) == 1) {
    ybrk_posp <- plyr::round_any(ybrk_pos, 1, f = floor)
  }
  ybrk_posp

  if (ymax < 1) {
    ybrks_seq <- c(ybrk_neg, ybrk_pos)
  } else {
    ybrks_seq <- c(ybrk_negn, ybrk_posp)
  }
  ybrks_seq

  # Positive and negative anomalies and 3 years moving average to create bar plot
  ano_shp_dt %<>%
    mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
    mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
  ano_shp_dt
  tail(ano_shp_dt)

  if (parr == "prcp" | parr == "soil_moisture") {
    par_title <- paste0(
      parr_full,
      " ",
      " (% of normal)"
    )
  } else {
    par_title <- paste0(parr_full, " ", " (", unt, ")")
  }

  if (parr == "prcp" | parr == "soil_moisture") {
    y_axis_lab <- paste0(parr, " average anomaly (% of normal)")
  } else {
    y_axis_lab <- paste0(parr, " average anomaly ", "(", unt, ")")
  }

  ano_shp_trn_plt <-
    ggplot(data = ano_shp_dt, aes(x = yr, y = ano)) +
    # annotate(
    #   geom = 'text',
    #   label = plt_wtrmrk,
    #   x = Inf,
    #   y = -Inf,
    #   hjust = 1,
    #   vjust = -0.5,
    #   color = 'gray80',
    #   size = 3.0
    # ) +
    geom_bar(
      stat = "identity",
      aes(fill = ano),
      width = 0.7,
      show.legend = FALSE
    ) +
    geom_hline(
      yintercept = 0,
      color = "gray10",
      linewidth = 0.5
    ) +
    scale_fill_gradientn(
      name = paste0(parr, " anomaly ", unt),
      colours = cpt(pal = "ncl_BlWhRe", n = 100, rev = F),
      limits = c(ymin, ymax),
      breaks = ybrks_seq
    ) +
    geom_line(
      aes(y = ano_mv, color = "3-yrs moving mean"),
      linewidth = 1.1,
      alpha = 0.7,
      na.rm = T
    ) +
    # geom_point(color = "blue", size = 2) +
    geom_segment(
      aes(
        x = xs[[1]],
        xend = xs[[2]],
        y = ys[[1]],
        yend = ys[[2]],
        color = "1950-trend"
      ),
      linetype = "dashed",
      linewidth = 0.9
    ) +
    geom_label(
      aes(x = xs[[1]] + 20),
      color = 'black',
      y = ymax - 0.05,
      fill = NA,
      label = ano_shp_dt$trn_lab[[1]],
      size = 4.0,
      parse = T
    ) +
    # add 80s trend
    geom_segment(
      aes(
        x = xs80[[1]],
        xend = xs80[[2]],
        y = ys80[[1]],
        yend = ys80[[2]],
        color = "1980-trend"
      ),
      linetype = "solid",
      linewidth = 0.9
    ) +
    geom_label(
      aes(x = xs[[1]] + 38),
      y = ymax - 0.05,
      fill = NA,
      color = 'deepskyblue2',
      label = ano_shp_dt80$trn_lab[[1]],
      size = 4.0,
      parse = TRUE
    ) +
    scale_x_continuous(
      name = " ",
      breaks = seq(1950, maxyr, 5),
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      name = y_axis_lab,
      limits = c(ymin, ymax),
      breaks = ybrks_seq
    ) +
    labs(title = par_title) +
    scale_color_manual(
      " ",
      values = c(
        "3-yrs moving mean" = "green",
        "1950-trend" = "black",
        "1980-trend" = "deepskyblue2"
      ),
      labels = c(
        "3-yrs moving mean" = "3-yrs moving mean",
        "1950-trend" = "1950-trend",
        "1980-trend" = "1980-trend"
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray75",
        linewidth = 0.05,
        linetype = "dashed"
      ),

      # Axes
      axis.line = element_line(colour = "black", linewidth = 1),
      axis.ticks.length = unit(-0.20, "cm"),

      axis.title.y = element_text(
        angle = 90,
        face = "plain",
        size = 13,
        colour = "black",
        margin = unit(c(1, 1, 1, 1), "mm")
      ),
      axis.title.x = element_text(
        face = "plain",
        size = 13,
        colour = "black",
        margin = unit(c(1, 1, 1, 1), "mm")
      ),

      axis.text.x = element_text(
        colour = "black",
        size = 12,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),
      axis.text.y = element_text(
        angle = 0,
        colour = "black",
        size = 12,
        margin = margin(t = 2, r = 2, b = 2, l = 2)
      ),

      # Title
      plot.title = element_text(
        face = "bold",
        size = 13,
        colour = "black"
      ),

      # Legend
      legend.position = c(0.5, 0.08),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = NA, color = "black"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13, margin = margin(t = -5)),

      # Facet strips
      strip.background = element_rect(fill = "black"),
      strip.text = element_text(
        colour = "white",
        face = "bold",
        size = 12
      )
    )
  ano_shp_trn_plt

  if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
    ano_shp_trn_plt <- ano_shp_trn_plt +
      scale_fill_gradientn(
        name = paste0(parr, "  anomaly ", unt),
        colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
        limits = c(ymin, ymax),
        breaks = ybrks_seq
      )
  }
  ano_shp_trn_plt

  #     ggsave(paste0(getwd(),"/plots/",parr_full,"_",sea,"_timeseries_trend_1950_2023.png"),
  #        plot = ano_shp_trn_plt,
  #        width = 16,
  #        height =7.5,
  #        units = "in",
  #        dpi = 300,
  #        scale = 0.9,
  #        limitsize = F
  # )

  # Summary table for current season anomalies and spatial trends -----
  if (length(years) > nlyr(ano_dt_shp_rast)) {
    ano_dt_shp_rast_cur_mon <- subset(
      ano_dt_shp_rast,
      which(names(ano_dt_shp_rast) %in% cur_yr_nam)
    )
  } else {
    ano_dt_shp_rast_cur_mon <- subset(
      ano_dt_shp_rast,
      which(names(ano_dt_shp_rast) %in% (as.numeric(cur_yr_nam) - 1))
    )
  }
  #  anomaly
  cur_sea_ano_rng <- terra::minmax(ano_dt_shp_rast_cur_mon, compute = T)

  cur_sea_ano_sum_tab <- tibble(rownames_to_column(global(
    ano_dt_shp_rast_cur_mon,
    fun = "mean",
    na.rm = T
  ))) %>%
    dplyr::select(mean_ano = mean) %>%
    mutate(mean_ano = round(mean_ano, digits = 2))

  cur_sea_ano_sum_tab$min_ano <- round(cur_sea_ano_rng[1], digits = 2)
  cur_sea_ano_sum_tab$max_ano <- round(cur_sea_ano_rng[2], digits = 2)

  # Spatial Trend
  ano_trn_mag_shp <-
    ano_trn_mag |>
    terra::crop(sel_area_shpfl, snap = "out") |>
    terra::mask(sel_area_shpfl, touches = TRUE)

  # plot(ano_trn_mag_shp)
  cur_sea_trn_rng <- terra::minmax(ano_trn_mag_shp, compute = T)

  cur_sea_trn_sum_tab <- tibble(rownames_to_column(global(
    ano_trn_mag_shp,
    fun = "mean",
    na.rm = T
  ))) %>%
    dplyr::select(mean_trn = mean) %>%
    mutate(mean_trn = round(mean_trn, digits = 2))

  cur_sea_trn_sum_tab$min_trn <- round(cur_sea_trn_rng[1], digits = 2)
  cur_sea_trn_sum_tab$max_trn <- round(cur_sea_trn_rng[2], digits = 2)

  cur_sea_ano_trn_sum_tab <- bind_cols(cur_sea_ano_sum_tab, cur_sea_trn_sum_tab)

  # linear trend
  cur_sea_ano_trn_sum_tab$lin_trn <- round(ano_shp_dt$trn[1], digits = 2)
  cur_sea_ano_trn_sum_tab$lin_trn_pval <- round(ano_shp_dt$sig[1], digits = 2)

  #annual anomaly ranking by season
  rank_no <- ano_shp_dt %>%
    filter(yr == max(yr))
  cur_sea_ano_trn_sum_tab$ano_rnk_pos <- round(rank_no$ano_rank, digits = 2)
  cur_sea_ano_trn_sum_tab$seaa <- sea
  cur_sea_ano_trn_sum_tab$yr <- max_yr_sea
  cur_sea_ano_trn_sum_tab$par <- parr
  cur_sea_ano_trn_sum_tab

  # Final outputs list ------
  # To include in the list with name
  plts <- list(ano_shp_trn_plt, ano_dt_sp_trn_sig_plt, cur_sea_ano_trn_sum_tab)
  names(plts) <- c(
    paste0(parr, "_", monn, "_ano_trn_lngtrm"),
    paste0(parr, "_", monn, "_sp_ano_trn"),
    paste0(parr, "_", monn, "_ano_trn_summary_table")
  )
  return(plts)
}

seas <- c('winter', 'spring', 'summer', 'fall', 'annual')
seas
cur_yr_nam <- format(as.Date(curr_mon_yr), "%Y")

parrs <- c("tmean", "tmax", "tmin", "prcp", "vpd", "rh", "soil_moisture")


# seas <- c('winter')
# seas
# cur_yr_nam <- format(as.Date(curr_mon_yr), "%Y")
# parrs <- c("tmean")
# took 890sec
tic()

all_sea_par_lngtrm_trn_plt_lst <- list()
for (j in 1:length(seas)) {
  seaa <- seas[[j]]
  par_lngtrn_plt_lst <- list()
  for (k in 1:length(parrs)) {
    parrr <- parrs[[k]]
    par_lngtrn_plt_lst[[k]] <- ano_sea_ann_lngtrn_plt_fun(
      ano_dt_fl,
      sea = seaa,
      parr = parrr
    )
  }
  all_sea_par_lngtrm_trn_plt_lst[[j]] <- par_lngtrn_plt_lst
}

toc()


### Seasonal and annual summary ---------------------------------
sea_summary_final_f <-
  bind_rows(
    all_sea_par_lngtrm_trn_plt_lst[[1]][[1]]$tmean_winter_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[1]][[4]]$prcp_winter_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[1]][[5]]$vpd_winter_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[1]][[6]]$rh_winter_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[1]][[
      7
    ]]$soil_moisture_winter_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[2]][[1]]$tmean_spring_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[2]][[4]]$prcp_spring_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[2]][[5]]$vpd_spring_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[2]][[6]]$rh_spring_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[2]][[
      7
    ]]$soil_moisture_spring_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[3]][[1]]$tmean_summer_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[3]][[4]]$prcp_summer_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[3]][[5]]$vpd_summer_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[3]][[6]]$rh_summer_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[3]][[
      7
    ]]$soil_moisture_summer_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[4]][[1]]$tmean_fall_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[4]][[4]]$prcp_fall_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[4]][[5]]$vpd_fall_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[4]][[6]]$rh_fall_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[4]][[
      7
    ]]$soil_moisture_fall_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[5]][[1]]$tmean_annual_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[5]][[4]]$prcp_annual_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[5]][[5]]$vpd_annual_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[5]][[6]]$rh_annual_ano_trn_summary_table,
    all_sea_par_lngtrm_trn_plt_lst[[5]][[
      7
    ]]$soil_moisture_annual_ano_trn_summary_table
  )
sea_summary_final_f

write.csv(
  sea_summary_final_f,
  paste0(
    results_pth,
    'sea_ann_anomalies_trend_summaries_all_par_',
    update_year,
    '.csv'
  )
)


## Winter plots ------------------------------------------------------
### Mean temperature and VPD ---------------------------------------------
##### Spatial trends--------------------------------------------------------------

sea_yr <- sea_summary_final_f %>%
  filter(seaa == 'winter') %>%
  distinct(yr) %>%
  pull(yr)
sea_yr

tmp_vpd_win_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[1]][[1]]$tmean_winter_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[1]][[5]]$vpd_winter_sp_ano_trn

tmp_vpd_win_sp_ano_trn_plt_f <- tmp_vpd_win_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Winter anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_win_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    'winter_tmean_vpd_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_win_sp_ano_trn_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends---------------------------------------------

tmp_vpd_win_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[1]][[1]]$tmean_winter_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[1]][[5]]$vpd_winter_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))

tmp_vpd_win_lngtrn_plt_f <- tmp_vpd_win_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Winter spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ),
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )

tmp_vpd_win_lngtrn_plt_f

ggsave(
  paste0(
    results_pth,
    'winter_tmean_vpd_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_win_lngtrn_plt_f,
  width = 19,
  height = 13,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Precipitation, relative humidity and soil moisture --------------------------------

##### Spatial trends ------------------------

prcp_rh_sm_win_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[1]][[4]]$prcp_winter_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[1]][[6]]$rh_winter_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[1]][[7]]$soil_moisture_winter_sp_ano_trn +
  plot_layout(ncol = 2)
prcp_rh_sm_win_sp_ano_trn_plt_f

prcp_rh_sm_win_sp_ano_trn_plt_f <- prcp_rh_sm_win_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Winter anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_win_sp_ano_trn_plt_f


ggsave(
  paste0(
    results_pth,
    'winter_prcp_rh_sm_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_win_sp_ano_trn_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends ------------------------------------

prcp_rh_sm_win_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[1]][[4]]$prcp_winter_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[1]][[6]]$rh_winter_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[1]][[
    7
  ]]$soil_moisture_winter_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))
prcp_rh_sm_win_lngtrn_plt_f

prcp_rh_sm_win_lngtrn_plt_f <- prcp_rh_sm_win_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Winter spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_win_lngtrn_plt_f


ggsave(
  paste0(
    results_pth,
    'winter_prcp_rh_sm_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_win_lngtrn_plt_f,
  width = 19,
  height = 17,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


## Spring plots ------------------------------------------------------
### Mean temperature and VPD ---------------------------------------------
##### Spatial trends--------------------------------------------------------------

sea_yr <- sea_summary_final_f %>%
  filter(seaa == 'spring') %>%
  distinct(yr) %>%
  pull(yr)
sea_yr

tmp_vpd_spr_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[2]][[1]]$tmean_spring_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[2]][[5]]$vpd_spring_sp_ano_trn

tmp_vpd_spr_sp_ano_trn_plt_f <- tmp_vpd_spr_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Spring anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_spr_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    'spring_tmean_vpd_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_spr_sp_ano_trn_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends---------------------------------------------

tmp_vpd_spr_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[2]][[1]]$tmean_spring_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[2]][[5]]$vpd_spring_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))

tmp_vpd_spr_lngtrn_plt_f <- tmp_vpd_spr_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Spring spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ),
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )

tmp_vpd_spr_lngtrn_plt_f

ggsave(
  paste0(
    results_pth,
    'spring_tmean_vpd_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_spr_lngtrn_plt_f,
  width = 19,
  height = 13,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


### Precipitation, relative humidity and soil moisture --------------------------------

##### Spatial trends ------------------------

prcp_rh_sm_spr_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[2]][[4]]$prcp_spring_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[2]][[6]]$rh_spring_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[2]][[7]]$soil_moisture_spring_sp_ano_trn +
  plot_layout(ncol = 2)
prcp_rh_sm_spr_sp_ano_trn_plt_f

prcp_rh_sm_spr_sp_ano_trn_plt_f <- prcp_rh_sm_spr_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Spring anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_spr_sp_ano_trn_plt_f


ggsave(
  paste0(
    results_pth,
    'spring_prcp_rh_sm_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_spr_sp_ano_trn_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends ------------------------------------

prcp_rh_sm_spr_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[2]][[4]]$prcp_spring_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[2]][[6]]$rh_spring_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[2]][[
    7
  ]]$soil_moisture_spring_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))
prcp_rh_sm_spr_lngtrn_plt_f

prcp_rh_sm_spr_lngtrn_plt_f <- prcp_rh_sm_spr_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Spring spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_spr_lngtrn_plt_f


ggsave(
  paste0(
    results_pth,
    'spring_prcp_rh_sm_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_spr_lngtrn_plt_f,
  width = 19,
  height = 17,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


## Summer plots ------------------------------------------------------
### Mean temperature and VPD ---------------------------------------------
##### Spatial trends--------------------------------------------------------------

sea_yr <- sea_summary_final_f %>%
  filter(seaa == 'summer') %>%
  distinct(yr) %>%
  pull(yr)
sea_yr

tmp_vpd_sum_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[3]][[1]]$tmean_summer_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[3]][[5]]$vpd_summer_sp_ano_trn

tmp_vpd_sum_sp_ano_trn_plt_f <- tmp_vpd_sum_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Summer anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_sum_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    'summer_tmean_vpd_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_sum_sp_ano_trn_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends---------------------------------------------

tmp_vpd_sum_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[3]][[1]]$tmean_summer_ano_trn_lngtrm) +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_blank()
    )) /
  ((all_sea_par_lngtrm_trn_plt_lst[[3]][[5]]$vpd_summer_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))

tmp_vpd_sum_lngtrn_plt_f <- tmp_vpd_sum_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Summer spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ),
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = "gray60")
    )
  )

tmp_vpd_sum_lngtrn_plt_f

ggsave(
  paste0(
    results_pth,
    'summer_tmean_vpd_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_sum_lngtrn_plt_f,
  width = 19,
  height = 13,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Precipitation, relative humidity and soil moisture --------------------------------

##### Spatial trends ------------------------

prcp_rh_sm_sum_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[3]][[4]]$prcp_summer_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[3]][[6]]$rh_summer_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[3]][[7]]$soil_moisture_summer_sp_ano_trn +
  plot_layout(ncol = 2)
prcp_rh_sm_sum_sp_ano_trn_plt_f

prcp_rh_sm_sum_sp_ano_trn_plt_f <- prcp_rh_sm_sum_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Summer anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_sum_sp_ano_trn_plt_f


ggsave(
  paste0(
    results_pth,
    'summer_prcp_rh_sm_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_sum_sp_ano_trn_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends ------------------------------------

prcp_rh_sm_sum_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[3]][[4]]$prcp_summer_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[3]][[6]]$rh_summer_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[3]][[
    7
  ]]$soil_moisture_summer_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))
prcp_rh_sm_sum_lngtrn_plt_f

prcp_rh_sm_sum_lngtrn_plt_f <- prcp_rh_sm_sum_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Summer spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_sum_lngtrn_plt_f


ggsave(
  paste0(
    results_pth,
    'summer_prcp_rh_sm_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_sum_lngtrn_plt_f,
  width = 19,
  height = 17,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


## Fall plots ------------------------------------------------------
### Mean temperature and VPD ---------------------------------------------
##### Spatial trends--------------------------------------------------------------

sea_yr <- sea_summary_final_f %>%
  filter(seaa == 'fall') %>%
  distinct(yr) %>%
  pull(yr)
sea_yr

tmp_vpd_fal_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[4]][[1]]$tmean_fall_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[4]][[5]]$vpd_fall_sp_ano_trn

tmp_vpd_fal_sp_ano_trn_plt_f <- tmp_vpd_fal_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Fall anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_fal_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    'fall_tmean_vpd_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_fal_sp_ano_trn_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends---------------------------------------------

tmp_vpd_fal_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[4]][[1]]$tmean_fall_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[4]][[5]]$vpd_fall_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))

tmp_vpd_fal_lngtrn_plt_f <- tmp_vpd_fal_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Fall spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ),
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )

tmp_vpd_fal_lngtrn_plt_f

ggsave(
  paste0(
    results_pth,
    'fall_tmean_vpd_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_fal_lngtrn_plt_f,
  width = 19,
  height = 13,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Precipitation, relative humidity and soil moisture --------------------------------

##### Spatial trends ------------------------

prcp_rh_sm_fal_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[4]][[4]]$prcp_fall_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[4]][[6]]$rh_fall_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[4]][[7]]$soil_moisture_fall_sp_ano_trn +
  plot_layout(ncol = 2)
prcp_rh_sm_fal_sp_ano_trn_plt_f

prcp_rh_sm_fal_sp_ano_trn_plt_f <- prcp_rh_sm_fal_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Fall anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_fal_sp_ano_trn_plt_f


ggsave(
  paste0(
    results_pth,
    'fall_prcp_rh_sm_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_fal_sp_ano_trn_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends ------------------------------------

prcp_rh_sm_fal_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[4]][[4]]$prcp_fall_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[4]][[6]]$rh_fall_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[4]][[
    7
  ]]$soil_moisture_fall_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))
prcp_rh_sm_fal_lngtrn_plt_f

prcp_rh_sm_fal_lngtrn_plt_f <- prcp_rh_sm_fal_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Fall spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_fal_lngtrn_plt_f


ggsave(
  paste0(
    results_pth,
    'fall_prcp_rh_sm_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_fal_lngtrn_plt_f,
  width = 19,
  height = 17,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)


## Annual plots ------------------------------------------------------
### Mean temperature and VPD ---------------------------------------------
##### Spatial trends--------------------------------------------------------------

sea_yr <- sea_summary_final_f %>%
  filter(seaa == 'annual') %>%
  distinct(yr) %>%
  pull(yr)
sea_yr

tmp_vpd_ann_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[5]][[1]]$tmean_annual_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[5]][[5]]$vpd_annual_sp_ano_trn

tmp_vpd_ann_sp_ano_trn_plt_f <- tmp_vpd_ann_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Annual anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
tmp_vpd_ann_sp_ano_trn_plt_f

ggsave(
  paste0(
    results_pth,
    'annual_tmean_vpd_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_ann_sp_ano_trn_plt_f,
  width = 15,
  height = 7,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends---------------------------------------------

tmp_vpd_ann_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[5]][[1]]$tmean_annual_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[5]][[5]]$vpd_annual_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))

tmp_vpd_ann_lngtrn_plt_f <- tmp_vpd_ann_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Annual spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ),
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )

tmp_vpd_ann_lngtrn_plt_f

ggsave(
  paste0(
    results_pth,
    'annual_tmean_vpd_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = tmp_vpd_ann_lngtrn_plt_f,
  width = 19,
  height = 13,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

### Precipitation, relative humidity and soil moisture --------------------------------

##### Spatial trends ------------------------

prcp_rh_sm_ann_sp_ano_trn_plt_f <-
  all_sea_par_lngtrm_trn_plt_lst[[5]][[4]]$prcp_annual_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[5]][[6]]$rh_annual_sp_ano_trn +
  all_sea_par_lngtrm_trn_plt_lst[[5]][[7]]$soil_moisture_annual_sp_ano_trn +
  plot_layout(ncol = 2)
prcp_rh_sm_ann_sp_ano_trn_plt_f

prcp_rh_sm_ann_sp_ano_trn_plt_f <- prcp_rh_sm_ann_sp_ano_trn_plt_f +
  plot_annotation(
    title = paste0("Annual anomaly spatial trends: 1950-", sea_yr),
    # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_ann_sp_ano_trn_plt_f


ggsave(
  paste0(
    results_pth,
    'annual_prcp_rh_sm_spatial_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_ann_sp_ano_trn_plt_f,
  width = 18,
  height = 16,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)

##### Time-series trends ------------------------------------

prcp_rh_sm_ann_lngtrn_plt_f <-
  ((all_sea_par_lngtrm_trn_plt_lst[[5]][[4]]$prcp_annual_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[5]][[6]]$rh_annual_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank())) /
  ((all_sea_par_lngtrm_trn_plt_lst[[5]][[
    7
  ]]$soil_moisture_annual_ano_trn_lngtrm) +
    theme(axis.title.y = element_blank()))
prcp_rh_sm_ann_lngtrn_plt_f

prcp_rh_sm_ann_lngtrn_plt_f <- prcp_rh_sm_ann_lngtrn_plt_f +
  plot_annotation(
    title = paste0(
      "Annual spatially averaged anomalies and trends in BC: 1950 - ",
      sea_yr
    ), # subtitle = 'Baseline:1981-2010',
    caption = plt_wtrmrk,
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.caption = element_text(hjust = 0.5, size = 7, color = 'gray60')
    )
  )
prcp_rh_sm_ann_lngtrn_plt_f


ggsave(
  paste0(
    results_pth,
    'annual_prcp_rh_sm_bc_timeseries_trend_1950_',
    update_year,
    '.png'
  ),
  plot = prcp_rh_sm_ann_lngtrn_plt_f,
  width = 19,
  height = 17,
  units = "in",
  dpi = 310,
  scale = 0.7,
  limitsize = F
)
