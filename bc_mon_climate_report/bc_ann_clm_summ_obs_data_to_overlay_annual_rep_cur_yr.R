rm(list = ls())
# Read obsered station data, perfrom futher QAQC
# combine and for annual anomaly calcualtion and  overlay on annual summary report

# Required + Paths ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)
library(zoo)
library(fs)

bc_obs_stn_dt_pth <- '../../data_database/bc_stns_data_2025/'
dump_pth <- './annual_obs_stn_dt_dump/'

report_yr <- '2025'

# This is to calcualate stions anomaly and values for update year
per_mis_incld <- 10 # Missing % threshold for QC of data
dt_ana_min_date <- as.Date('1979-01-01')
dt_ana_max_date <- as.Date(paste0(report_yr, '-12-31'), format = '%Y-%m-%d')

# Obs stn data  -----------------------------------------------------------------
# list CSV files in the folder and read them
list.files(
  path = file.path(bc_obs_stn_dt_pth, "bc_stn_dt_final"),
  pattern = "\\.csv$",
  full.names = TRUE
) %>%
  tibble(dt_pth = .) %>%
  mutate(fl_nam = basename(dt_pth)) %>%
  mutate(
    str_yr = as.integer(str_sub(fl_nam, -21, -18)),
    end_yr = as.integer(str_sub(fl_nam, -12, -9))
  ) %>%
  mutate(
    n_yrs = end_yr - str_yr + 1,
    agency = str_extract(fl_nam, "[^/_]+(?=_)")
  ) %>%
  dplyr::select(str_yr, end_yr, n_yrs, agency, dt_pth) -> obs_stn_dt_tbl
obs_stn_dt_tbl
# View(obs_stn_dt_tbl)
obs_stn_dt_tbl %<>%
  filter(!str_detect(dt_pth, 'metadata')) %<>%
  filter(n_yrs >= 30)
obs_stn_dt_tbl

# Filtering and quality control of the station data  --------------

obs_stn_dt_lst <- list()
for (i in 1:nrow(obs_stn_dt_tbl)) {
  # for (i in 1:5) {
  # i <- 97
  tryCatch(
    {
      obs_stn_dt_tbl_i <- obs_stn_dt_tbl[i, ]
      obs_stn_dt_i <- read_csv(obs_stn_dt_tbl_i$dt_pth)
      obs_stn_dt_i
      tail(obs_stn_dt_i)
      # If agency column is missing add , if NA remvoe those rows
      obs_stn_dt_i %>%
        {
          # If 'agency' column is missing, add it from main datapath file
          if (!"agency" %in% names(.)) {
            mutate(., agency = obs_stn_dt_tbl_i$agency)
          } else {
            .
          }
        } %>%
        # Remove rows where agency is NA or empty
        # filter(!is.na(agency) & agency != "") %>%
        dplyr::select(
          stn_grp,
          date,
          pr,
          tmin,
          tmax,
          lat,
          lon,
          ele,
          agency
        ) %>%
        #remove rows if all tmin, tmax and pr is NA
        filter(!(is.na(tmin) & is.na(tmax) & is.na(pr))) %>%
        mutate(
          lat = round(as.numeric(lat), 3),
          lon = round(as.numeric(lon), 3),
          ele = round(as.numeric(ele), 3)
        ) -> obs_stn_dt_i
      tail(obs_stn_dt_i)
      # check max date
      max_date_i <- max(obs_stn_dt_i$date, na.rm = TRUE)

      # if max date < report year -12-31 → skip
      if (max_date_i < dt_ana_max_date) {
        message(paste0(
          "Skipping row ",
          i,
          ": max date ",
          max_date_i,
          " <= ",
          dt_ana_max_date
        ))
        next
      }

      # Filter for period of analysis
      obs_stn_dt_i %>%
        filter(date >= dt_ana_min_date & date <= dt_ana_max_date) %>%
        # Create a date seq and fill with NA for missing
        complete(date = seq(min(date), max(date), by = "1 day")) %>%
        fill(
          stn_grp,
          lat,
          lon,
          ele,
          agency,
          .direction = "downup"
        ) -> obs_stn_dt_i

      # Quality control -1
      obs_stn_dt_i %>%
        ## Data range: daily precipitation 0 - 200, tmin/tmax : mena+-7*SD.
        mutate(pr = ifelse(pr < 0, NA, ifelse(pr > 260, NA, pr))) %>% # precipitation range
        mutate(tmax = ifelse(tmax < tmin, NA, tmax)) %>% # tmax < tmin
        mutate(tmin = ifelse(tmin > tmax, NA, tmin)) %>% # tmax < tmin
        mutate(
          tmin = ifelse(
            tmin < (mean(tmin, na.rm = T) - (7 * sd(tmin, na.rm = T))),
            NA,
            ifelse(
              tmin > (mean(tmin, na.rm = T) + (7 * sd(tmin, na.rm = T))),
              NA,
              tmin
            )
          ), # temperature range
          tmax = ifelse(
            tmax < (mean(tmax, na.rm = T) - (7 * sd(tmax, na.rm = T))),
            NA,
            ifelse(
              tmax > (mean(tmax, na.rm = T) + (7 * sd(tmax, na.rm = T))),
              NA,
              tmax
            )
          )
        ) %>%
        ## Fill with approx: if 5 or less days values are missing them fill those as average of previous and last values
        mutate(
          pr = na.approx(pr, maxgap = 3, na.rm = FALSE),
          tmin = na.approx(tmin, maxgap = 5, na.rm = FALSE),
          tmax = na.approx(tmax, maxgap = 5, na.rm = FALSE)
        ) -> obs_stn_dt_i
      obs_stn_dt_i

      # ggplot(obs_stn_dt_i, aes(x = date)) +
      #   geom_line(aes(y = tmax), color = 'red', alpha = 0.8) +
      #   geom_line(aes(y = tmin), color = 'blue', alpha = 0.8)

      # ggplot(obs_stn_dt_i, aes(x = date)) +
      #   geom_line(aes(y = pr), color = 'green', alpha = 0.8)

      # Calculate missing percentage . If less than 10 % missing data over the period of analysis fill with daily average.
      ## If more than 10% missing remove that station/parameter

      missing_perc <- obs_stn_dt_i %>%
        group_by(stn_grp) %>%
        summarize(
          pr_na_per = mean(is.na(pr)) * 100,
          tmin_na_per = mean(is.na(tmin)) * 100,
          tmax_na_per = mean(is.na(tmax)) * 100
        )

      pr_na_per <- missing_perc$pr_na_per
      tmin_na_per <- missing_perc$tmin_na_per
      tmax_na_per <- missing_perc$tmax_na_per

      #If all parameters >10% missing, skip
      if (all(c(pr_na_per, tmin_na_per, tmax_na_per) > per_mis_incld)) {
        message("All parameters >10% missing — skipping this station/group.")
        next
      }

      # Otherwise, process only variables with ≤10% missing
      obs_stn_dt_i <- obs_stn_dt_i %>%
        mutate(jday = yday(date)) %>%
        group_by(jday) %>%
        mutate(
          pr_j_av = round(mean(pr, na.rm = TRUE), 3),
          tmax_j_av = round(mean(tmax, na.rm = TRUE), 3),
          tmin_j_av = round(mean(tmin, na.rm = TRUE), 3)
        ) %>%
        ungroup() %>%
        # fill missing dates using the average of that day over the period of analysis
        mutate(
          pr = if (pr_na_per > 10) NA else ifelse(is.na(pr), pr_j_av, pr),
          tmax = if (tmax_na_per > 10) {
            NA
          } else {
            ifelse(is.na(tmax), tmax_j_av, tmax)
          },
          tmin = if (tmin_na_per > 10) {
            NA
          } else {
            ifelse(is.na(tmin), tmin_j_av, tmin)
          }
        ) %>%
        select(-c(jday, pr_j_av, tmax_j_av, tmin_j_av))
      obs_stn_dt_i

      # Filtered and selected data
      obs_stn_dt_i %>%
        dplyr::select(
          stn = stn_grp,
          date,
          prcp = pr,
          tmin,
          tmax,
          lat,
          lon,
          ele
        ) -> obs_stn_dt_i_f
      obs_stn_dt_i_f
      tail(obs_stn_dt_i_f)

      obs_stn_dt_lst[[i]] <- obs_stn_dt_i_f

      # Save data
      write_csv(
        obs_stn_dt_i_f,
        paste0(
          dump_pth,
          obs_stn_i_mtdt$stn,
          '_obs_dly_pr_tmp_dt_',
          obs_stn_i_mtdt$str_date,
          '_',
          obs_stn_i_mtdt$end_date,
          '.csv'
        )
      )

      # Plot
      # Pivot longer
      obs_stn_dt_i_f_l <- obs_stn_dt_i_f %>%
        pivot_longer(
          cols = c(tmin, tmax, prcp),
          names_to = 'par',
          values_to = 'val'
        ) %>%
        mutate(temp = ifelse(par %in% c('tmin', 'tmax'), 'temp', par))

      # Identify parameters with all NA
      na_labels <- obs_stn_dt_i_f_l %>%
        group_by(par) %>%
        summarize(all_na = all(is.na(val)), .groups = "drop") %>%
        filter(all_na) %>%
        mutate(
          x = as.Date(mean(range(obs_stn_dt_i_f_l$date))), # middle of x-axis
          y = 0, # fixed y, can adjust
          label = "more than 10% missing"
        )

      # Plot
      obs_stn_dt_i_plt <- obs_stn_dt_i_f_l %>%
        ggplot(aes(x = date, y = val)) +
        geom_line(color = 'navy', na.rm = TRUE) +
        geom_text(
          data = na_labels,
          aes(x = x, y = y, label = label),
          color = "red",
          size = 4
        ) +
        facet_wrap(. ~ par, scales = 'free_y', ncol = 1) +
        theme_bw() +
        scale_x_date(
          date_breaks = "1 year",
          date_minor_breaks = "1 month",
          date_labels = "%Y"
        ) +
        labs(
          title = paste0(
            obs_stn_i_mtdt$stn,
            '_daily_prcp_tmin_tmax',
            ' [',
            obs_stn_i_mtdt$lon,
            '°, ',
            obs_stn_i_mtdt$lat,
            '°, ',
            obs_stn_i_mtdt$ele,
            'm]'
          )
        ) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 15)
        )
      obs_stn_dt_i_plt

      ggsave(
        paste0(
          dump_pth,
          obs_stn_i_mtdt$stn,
          '_obs_dly_pr_tmp_dt_',
          obs_stn_i_mtdt$str_date,
          '_',
          obs_stn_i_mtdt$end_date,
          '.png'
        ),
        plot = obs_stn_dt_i_plt,
        width = 12,
        height = 7,
        units = "in",
        dpi = 300,
        scale = 0.8,
        limitsize = FALSE
      )
    },
    error = function(e) {
      message(paste0("Error in row ", i, ": ", e$message))
    }
  )
}

length(obs_stn_dt_lst)

obs_stn_dly_dt <- bind_rows(obs_stn_dt_lst)
obs_stn_dly_dt
tail(obs_stn_dly_dt)
unique(obs_stn_dly_dt$stn)

# Annual summary
obs_stn_dly_dt %>%
  mutate(yr = year(date)) %>%
  group_by(stn, lat, lon, ele, yr) %>%
  summarise(
    prcp = sum(prcp, na.rm = T),
    tmin = mean(tmin, na.rm = T),
    tmax = mean(tmax, na.rm = T)
  ) %>%
  ungroup() %>%
  dplyr::select(stn, yr, prcp, tmin, tmax, lat, lon, ele) -> obs_stn_ann_dt
obs_stn_ann_dt

rm(obs_stn_dly_dt)
rm(obs_stn_dt_lst)
gc()

write_csv(obs_stn_ann_dt, paste0('obs_stns_annual_dt_1979_', report_yr, '.csv'))

## Annual climatology and anomaly -----------------------
obs_stn_ann_dt <- read_csv(paste0(
  'obs_stns_annual_dt_1979_',
  report_yr,
  '.csv'
))
unique(obs_stn_ann_dt$stn)

obs_stn_ann_dt %<>%
  mutate(tmean = (tmin+tmax)/2) %<>%
  dplyr::select(-c(tmin,tmax))
obs_stn_ann_dt

obs_stn_ann_dt %<>%
  pivot_longer(
    cols = c(prcp, tmean),
    names_to = 'par',
    values_to = 'val'
  ) %<>%
  group_by(stn, par) %>%
  mutate(n_yrs = n_distinct(yr)) %>%
  filter(n_yrs >= 30) %<>%
  ungroup()
obs_stn_ann_dt
range(obs_stn_ann_dt$n_yrs)

# Climatology
obs_stn_ann_dt %>%
  filter(yr >= 1981 & yr <= 2010) %>%
  group_by(stn, par) %>%
  summarise(
    clim_val = round(mean(val, na.rm = T), 3)
  ) %>%
  ungroup() -> obs_stn_ann_clm_dt
obs_stn_ann_clm_dt
tail(obs_stn_ann_clm_dt)

# Merge climatology data
obs_stn_ann_dt %>%
  left_join(
    obs_stn_ann_clm_dt,
    by = c("stn", "par")
  ) -> obs_stn_ann_dt
obs_stn_ann_dt

# Anomalies
obs_stn_ann_dt %>%
  mutate(
    ano = case_when(
      par == "prcp" ~ round(((val-clim_val) / clim_val) * 100, 1),
      par %in% c("tmin", "tmax", "tmean") ~ round(val - clim_val, 3),
      TRUE ~ NA_real_
    )
  )%>%
  dplyr::select(
    stn,
    yr,
    par,
    val,
    clim_val,
    ano,
    lat,
    lon,
    ele
  ) -> obs_stn_ann_dt
obs_stn_ann_dt

unique(obs_stn_ann_dt$stn)

tst <- obs_stn_ann_dt %>%
  filter(stn == "grp172")
tst

tst %>%
  filter(par == "prcp") %>%
  ggplot(aes(x = yr, y = ano, group = stn)) +
  geom_line(alpha = 0.4)

tst %>%
  filter(par == "tmean") %>%
  ggplot(aes(x = yr, y = ano, group = stn)) +
  geom_line(alpha = 0.4)

ggplot(obs_stn_ann_dt)+
  geom_point(aes(x=lon,y=lat))


# # Process stations observed anomaly data to overlay on the anomaly plot
# # remove too close stations.  ( stations within 10km radius for each parameter)
#
# # Convert the data into spatvector and filter out nearby stations
# obs_stn_ano_vect <- vect(obs_stn_ann_dt, geom = c("lon", "lat"),
#                          crs  = "EPSG:4326",
#                          keepgeom = TRUE)
# obs_stn_ano_vect
#
# # filter for station that has data for 2025 and subset only those stations
# stns_with_2025 <- unique(obs_stn_ano_vect$stn[obs_stn_ano_vect$yr == 2025])
#
# obs_stn_ano_vect <- obs_stn_ano_vect[obs_stn_ano_vect$stn %in% stns_with_2025, ]
#
# # for each parameter filter stations that are too close ( <10km ) to each other
# parss <- unique(obs_stn_ano_vect$par)
#
# fltr_stn_ano_dt_list <- list()
# for (p in parss) {
#   # Subset to current parameter (e.g., prcp or tmean)
#   v_p <- obs_stn_ano_vect[obs_stn_ano_vect$par == p, ]
#
#   # Calculate metadata: Number of years per station
#   v_p %<>%
#     group_by(stn) %<>%
#     mutate(n_yrs = n_distinct(yr))
#
#    # Create a unique station vector for distance processing (1 row per station)
#   v_p_unq_stns <- v_p[
#     order(v_p$stn, -v_p$n_yrs),
#   ][!duplicated(v_p$stn), ]
#
#   # Calculate distance matrix (Great Circle distance in meters)
#   dist_mat <- as.matrix(distance(v_p_unq_stns, pairs = FALSE))
#
#   n_stns <- nrow(v_p_unq_stns)
#   keep_idx <- rep(TRUE, n_stns)
#
#   # Spatial Thinning Loop
#   for (i in 1:n_stns) {
#     if (!keep_idx[i]) next
#
#     # Identify neighbors within 10km (10,000 meters)
#     neighbors <- which(dist_mat[i, ] < 10000 & dist_mat[i, ] > 0)
#
#     for (j in neighbors) {
#       if (!keep_idx[j]) next
#
#       # Criterion A: Keep station with more years of data
#       if (v_p_unq_stns$n_yrs[j] < v_p_unq_stns$n_yrs[i]) {
#         keep_idx[j] <- FALSE
#       } else if (v_p_unq_stns$n_yrs[j] > v_p_unq_stns$n_yrs[i]) {
#         keep_idx[i] <- FALSE
#         break
#       } else {
#         # Criterion B: Tie-breaker - Keep the station that is more isolated
#         # (Compare distance to the nearest neighbor for both i and j)
#         dist_i <- min(dist_mat[i, dist_mat[i, ] > 0])
#         dist_j <- min(dist_mat[j, dist_mat[j, ] > 0])
#
#         if (dist_i >= dist_j) {
#           keep_idx[j] <- FALSE
#         } else {
#           keep_idx[i] <- FALSE
#           break
#         }
#       }
#     }
#   }
#
#   # Extract the final station names for this parameter
#   selected_stns <- v_p_unq_stns$stn[keep_idx]
#
#   fltr_stns_vect <- v_p[v_p$stn %in% selected_stns, ]
#   fltr_stns_vect
#
#   fltr_stns_tbl <- fltr_stns_vect %>%
#     as.data.frame() %>%
#     as_tibble()
#
#   # Subset the original data (with all years) using the selected station names
#   fltr_stn_ano_dt_list[[p]] <- fltr_stns_tbl
# }
#
# # Combined filtered stations anomaly data and filter tible for 2025
# obs_stn_ano_f_rpt_yr <- bind_rows(fltr_stn_ano_dt_list)
#
# obs_stn_ano_f_rpt_yr %<>%
#   filter(yr == report_yr)
# tail(obs_stn_ano_f_rpt_yr)


# obs_stn_ano_f_rpt_yr %<>%
#   filter(yr == report_yr)
# tail(obs_stn_ano_f_rpt_yr)

obs_stn_ano_f_rpt_yr <- obs_stn_ann_dt

write_csv(
  obs_stn_ano_f_rpt_yr,
  paste0('obs_stns_annual_anomaly_', report_yr, '.csv')
)

# End of script -------------
