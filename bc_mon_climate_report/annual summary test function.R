ann_clm_sum_cal_plt_fun <- function(
    ano_dt_fls,
    clm_dt_fls,
    bc_shp,
    parr,
    cur_yr
) {


  # parr <- 'prcp'

  ### metadata -----------------------------------------------------------
  par_meta <- tibble::tribble(
    ~par,            ~par_full,                          ~unit,
    "tmin",          "Minimum temperature",              "°C",
    "tmax",          "Maximum temperature",              "°C",
    "tmean",         "Average temperature",              "°C",
    "prcp",          "Precipitation",                    "%",
    "rh",            "Relative Humidity (RH)",           "%",
    "vpd",           "Vapor Pressure Deficit (VPD)",     "kPa",
    "soil_moisture", "Volumetric soil moisture (0–1 m)", "%"
  )

  meta <- par_meta |> filter(par == parr)
  parr_full <- meta$par_full
  unt <- meta$unit

  ## helper functions ---------------------------------------------------------

  ### Calculation helper functions --------------
  # Percentage of anomaly

  calc_percent_anomaly <- function(ano, clm) {
  ano_r_per <- (ano / clm) * 100
  # ano_r_per <- clamp(ano_r_per, -101,101,values =T)
  return(ano_r_per)
  }

  # Function to calculate summaries
  spatial_summ_fun <- function(
    rast_dt
  ) {

    # rast_dt <- mon_cur_yr_dt
    ## spatial summaries (mean / min / max)
    sp_mean <- global(rast_dt, "mean", na.rm = TRUE)
    sp_min  <- global(rast_dt, "min",  na.rm = TRUE)
    sp_max  <- global(rast_dt, "max",  na.rm = TRUE)

    summ_tbl <- tibble(
      rst_fl  = rownames(sp_mean),
      mean = round(sp_mean[, 1], 2),
      min  = round(sp_min[, 1],  2),
      max  = round(sp_max[, 1],  2)
    ) %>%
      mutate(
        yr = cur_yr
      )

    summ_tbl
  }


  # Spatial Mann-kendall trends
  # Calculate MK trend
  mk_pval_fun <- function(x) {
    if (all(is.na(x))) return(NA_real_)
    MannKendall(x)[[2]]
  }
  sen_slope_fun <- function(x) {
    if (all(is.na(x))) return(NA_real_)
    zyp.trend.vector(x, conf.intervals = FALSE)[[2]]
  }


  ### Plotting helper functions -------------

  calc_sym_breaks <- function(minval, maxval) {
    # Find the largest absolute value
    rng <- max(abs(minval), abs(maxval))

    # Determine step size based on range
    step <- dplyr::case_when(
      rng < 5   ~ 0.5,
      rng < 11  ~ 1,
      rng < 21  ~ 2,
      rng < 41  ~ 5,
      rng < 101 ~ 10,
      TRUE      ~ 20
    )

    # Compute symmetric breaks
    lower <- -ceiling(rng / step) * step
    upper <-  ceiling(rng / step) * step

    seq(lower, upper, by = step)
  }

  ###  Raster Map plot theme  and guide

  theme_bc_map <- function(nlyr = 1) {
    theme(
      # Panel
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray60",
        linewidth = 0.02,
        linetype = "dashed"
      ),
      axis.line = element_line(colour = "gray70", linewidth = 0.08),

      # Axis ticks
      axis.ticks.length = unit(-0.20, "cm"),
      axis.ticks = element_line(colour = "black", linewidth = 1),

      # # Axis titles
      # axis.title.x = element_text(
      #   angle = 0, face = "plain", size = 15,
      #   colour = "black",
      #   margin = margin(t = 0, r = 0, b = 0, l = 0)
      # ),
      # axis.title.y = element_text(
      #   angle = 90, face = "plain", size = 15,
      #   colour = "black",
      #   margin = margin(t = 0, r = 0, b = 0, l = 0)
      # ),
      #
      # # Axis text
      # axis.text.x = element_text(
      #   angle = 0, hjust = 0.5, vjust = 0.5,
      #   colour = "black", size = 14,
      #   margin = margin(t = 2, r = 2, b = 2, l = 2)
      # ),
      # axis.text.y = element_text(
      #   angle = 90, hjust = 0.5, vjust = 0.5,
      #   colour = "black", size = 14,
      #   margin = margin(t = 2, r = 2, b = 2, l = 2)
      # ),

      # Plot title
      plot.title = element_text(angle = 0, face = "bold", size = 13, colour = "black"),

      # Facet strips
      strip.background = element_rect(color = "black", fill = "white"),
      strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
      strip.text.y = element_text(size = 12, face = "bold"),

      # Legend
      legend.position = if(nlyr == 1) c(0.1, 0.4) else "right",
      legend.direction = "vertical",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 16, margin = margin(t = 0, r = 0, b = 0, l = 0)),

      # General axis.text margin (removed negative to avoid warnings)
      axis.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white"),

      # Remove axis text
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }

  # Guide bar

  fill_guide <- guide_colorbar(
    barwidth = 1.5,
    barheight = 22,
    label.vjust = 0.5,
    label.hjust = 0.0,
    title.vjust = 0.5,
    title.hjust = 0.5,
    title = NULL,
    ticks.colour = 'black',
    frame.colour = 'black',
    draw.ulim = TRUE,
    draw.llim = TRUE,
  )

  ### Linear time series plot theme
  theme_ts_linear <- function(
    legend_pos = c(0.5, 0.08),
    legend_dir = "horizontal"
  ) {
    theme_bw() +
      theme(
        # Grid
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          color = "gray75",
          linewidth = 0.03,
          linetype = "dashed"
        ),

        # Axes
        axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.ticks.length = unit(-0.20, "cm"),

        axis.title.x = element_text(
          size = 13, colour = "black",
          margin = margin(t = 3)
        ),
        axis.title.y = element_text(
          size = 13, colour = "black",
          margin = margin(r = 3)
        ),

        axis.text.x = element_text(
          angle = 90, size = 12, colour = "black",
          hjust = 0.5, vjust = 0.5,
          margin = margin(t = 2)
        ),
        axis.text.y = element_text(
          angle = 0, size = 12, colour = "black",
          hjust = 0.5, vjust = 0.5,
          margin = margin(r = 2)
        ),

        # Title
        plot.title = element_text(
          face = "bold", size = 13, colour = "black"
        ),
        plot.subtitle = element_text(size = 11),

        # Legend
        legend.position = legend_pos,
        legend.direction = legend_dir,
        legend.background = element_rect(fill = NA, color = "black"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),

        # Facets (if used)
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 12, face = "bold", colour = "black"),

        # Tag / watermark
        plot.tag.position = "bottom",
        plot.tag = element_text(
          color = "gray50",
          size = 8,
          hjust = 1
        )
      )
  }



  ## Load raster data for quick calculations  ---------

 ano_dt_paths <- ano_dt_fls |>
    distinct(dt_pth) |>
    pull(dt_pth)
  ano_dt_paths

   # Add all rasters for quick calculations
  rast_ano_cache <- purrr::map(
    setNames(ano_dt_paths, ano_dt_paths),
    terra::rast
  )

  ## monthly spatial anomalies and summary --------------------------------------------------
  months <- c("Jan" ,"Feb", "Mar","Apr","May","Jun","Jul",
                        "Aug","Sep","Oct","Nov","Dec")

  ### Monthly anomaly & plots -----------------------

  mon_rasts <- map(months, function(mon_i) {

    ano_fl <- ano_dt_fls |> filter(mon == mon_i & par == parr)

    ano_r <- rast_ano_cache[[ano_fl$dt_pth]]


    clm_fl <- clm_dt_fls |> filter(mon == mon_i & par == parr)

    clm_r <- rast(clm_fl$dt_pth)

    if (parr %in% c("prcp", "soil_moisture")) {
      ano_r <- calc_percent_anomaly(ano_r, clm_r)
    }

    ano_r <- crop(ano_r, bc_shp, mask = TRUE)
  })

  mon_ano_dt_stk <- rast(mon_rasts)
  names(mon_ano_dt_stk)

  terra::time(mon_ano_dt_stk) <- as.Date(names(mon_ano_dt_stk), format= '%Y-%m-%d')
  mon_ano_dt_stk

  # Filter for current year
  mon_cur_yr_dt <- subset(
    mon_ano_dt_stk,
    format(as.Date(names(mon_ano_dt_stk)), "%Y") == cur_yr
  )
  names(mon_cur_yr_dt)
  mon_cur_yr_dt
 plot(mon_cur_yr_dt)

 # Clamp anomaly percentage for above 100
 mon_cur_yr_dt <-  clamp( mon_cur_yr_dt, -101,101,values =T)

 # Monthly ano spatial plotting
 mon_cur_yr_dt <- project(mon_cur_yr_dt, crs(bc_shp_prj)) #'+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs' )
 mon_cur_yr_dt <- mask(mon_cur_yr_dt, bc_shp_prj)

 # Monthly  breaks
 mon_minval <- min(values(mon_cur_yr_dt), na.rm = TRUE)
 mon_maxval <- max(values(mon_cur_yr_dt), na.rm = TRUE)
 mon_brks   <- calc_sym_breaks(mon_minval, mon_maxval)

 mon_ano_plt <-  ggplot()+
 geom_spatraster(data = mon_cur_yr_dt)+
   scale_fill_gradientn(
     colours = cpt("ncl_BlWhRe", 100),
     limits = c((-1* max(abs(c(mon_minval, mon_maxval)))),
                (1* max(abs(c(mon_minval, mon_maxval))))),
     breaks = mon_brks,
     guide = fill_guide,
     na.value = "transparent"
   ) +
   facet_wrap(
     ~ factor(format(as.Date(lyr), "%b"), levels = month.abb),
     ncol = 4
   ) +
   geom_spatvector(data = bc_shp_prj, fill = NA, color = 'black', linewidth = 0.3)
 mon_ano_plt

 if (parr == "prcp" | parr == "soil_moisture"){
   mon_ano_plt <- mon_ano_plt  +
     scale_fill_gradientn(

       colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
       limits = c((-1* abs(max(c(mon_minval, mon_maxval)))),
                  (1* abs(max(c(mon_minval, mon_maxval))))),
       breaks = mon_brks,
       guide = fill_guide,
       na.value = "transparent"
     )+
     theme_bc_map(nlyr = nlyr(mon_cur_yr_dt)) +
     labs(
       title = paste0(cur_yr, " Monthly Anomalies: ", parr_full ),
       subtitle = paste0(parr_full,' (', unt,' )', ' relative to 1981-2010 climatology')
     )
 } else {
   mon_ano_plt <- mon_ano_plt+
     theme_bc_map(nlyr = nlyr(mon_cur_yr_dt)) +
     labs(
       title = paste0(cur_yr, " Monthly Anomalies: ", parr_full ),
       subtitle = paste0(parr_full,' (', unt,' )', ' relative to 1981-2010 climatology')
     )
 }
 mon_ano_plt

 ggsave(
   paste0(
     ann_summ_rslts_pth, parr,
     '_monthly_spatial_anomaly_maps_plt_',
     cur_yr,'.png'
   ),
   plot = mon_ano_plt,
   width = 13,
   height = 9.5,
   units = "in",
   dpi = 350,
   scale = 0.9,
   limitsize = F
 )

 ### Monthly spatial anomaly summary ---------------------
 mon_summ_tbl <- spatial_summ_fun(
   rast_dt = mon_cur_yr_dt
 )
 mon_summ_tbl %>%
   mutate(parr = parr) %>%
   mutate(
     rst_fl = as.Date(rst_fl),
     mon = factor(month(rst_fl, label = TRUE, abbr = TRUE),
                  levels = month.abb)
   ) %>%
   rename(type_prd = rst_fl) -> mon_summ_tbl
 mon_summ_tbl


 ## Annual spatial and time series anomaly -----------------------------------------------------
 # annual raster data
 ann_ano_fl <- ano_dt_fls |> filter(mon == 'annual' & par == parr)

 ann_ano_r <- rast_ano_cache[[ann_ano_fl$dt_pth]]

 ann_clm_fl <- clm_dt_fls |> filter(mon == 'annual' & par == parr)
 ann_clm_r <- rast(ann_clm_fl$dt_pth)

 if (parr %in% c("prcp", "soil_moisture")) {
   ann_ano_r <- calc_percent_anomaly(ann_ano_r, ann_clm_r)
 } else {
   ann_ano_r <- ann_ano_r
 }

 ann_ano_r <- crop(ann_ano_r, bc_shp, mask = TRUE)
 names(ann_ano_r)

 yrs <- str_remove(names(ann_ano_r), "^ann_") |> as.integer()
 names(ann_ano_r) <- yrs

 ### Annual spatial anomaly plot and summary -------------------
 ann_ano_r_cur_yr <- subset(ann_ano_r, which(names(ann_ano_r) %in% cur_yr))
 ann_ano_r_cur_yr

 # Spatial Plot
 # Clamp anomaly percentage for above 100
 ann_ano_r_cur_yr <-  clamp(ann_ano_r_cur_yr, -101,101,values =T)

  # breaks
 ann_minval <- min(values(ann_ano_r_cur_yr), na.rm = TRUE)
 ann_maxval <- max(values(ann_ano_r_cur_yr), na.rm = TRUE)
 ann_brks   <- calc_sym_breaks(ann_minval, ann_maxval)

 ann_ano_r_cur_yr <- project(ann_ano_r_cur_yr, crs(bc_shp_prj))
 ann_ano_r_cur_yr <- mask(ann_ano_r_cur_yr, bc_shp_prj)

 yr_ann_sp_ano_plt <-  ggplot()+
   geom_spatraster(data = ann_ano_r_cur_yr)+
   scale_fill_gradientn(
     colours = cpt("ncl_BlWhRe", 100),
     limits = c((-1* max(abs(c(ann_minval, ann_maxval)))),
                (1* max(abs(c(ann_minval, ann_maxval))))),
     breaks = ann_brks,
     guide = fill_guide,
     na.value = "transparent"
   ) +
   # facet_wrap(
   #   ~ factor(format(as.Date(lyr), "%b"), levels = month.abb),
   #   ncol = 4
   # ) +
   geom_spatvector(data = bc_shp_prj, fill = NA, color = 'black', linewidth = 0.3)
 yr_ann_sp_ano_plt

 if (parr == "prcp" | parr == "soil_moisture"){
   yr_ann_sp_ano_plt <- yr_ann_sp_ano_plt  +
     scale_fill_gradientn(

       colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
       limits = c((-1* max(abs(c(ann_minval, ann_maxval)))),
                  (1* max(abs(c(ann_minval, ann_maxval))))),
       breaks = ann_brks,
       guide = fill_guide,
       na.value = "transparent"
     )+
     theme_bc_map(nlyr = nlyr(ann_ano_r_cur_yr)) +
     labs(
       title = paste0(cur_yr, " Annual Anomalies: ", parr_full ),
       subtitle = paste0(parr_full,' (', unt,' )', ' relative to 1981-2010 climatology')
     )
 } else {
   yr_ann_sp_ano_plt  <-  yr_ann_sp_ano_plt +
     theme_bc_map(nlyr = nlyr(ann_ano_r_cur_yr)) +
     labs(
       title = paste0(cur_yr, "Annual Anomalies: ", parr_full ),
       subtitle = paste0(parr_full,' (', unt,' )', ' relative to 1981-2010 climatology')
     )
 }
 yr_ann_sp_ano_plt

 ggsave(
   paste0(
     ann_summ_rslts_pth, parr,
     '_annual_spatial_anomaly_maps_plt_',
     cur_yr,'.png'
   ),
   plot = yr_ann_sp_ano_plt,
   width = 11,
   height = 9.5,
   units = "in",
   dpi = 350,
   scale = 0.9,
   limitsize = F
 )

 ###  Annual anomaly spatial summary of current year --------------------------
 ann_ano_sp_summ_tbl <- spatial_summ_fun(
   rast_dt = ann_ano_r_cur_yr
 )
 ann_ano_sp_summ_tbl %>%
   mutate(parr = parr) %>%
   mutate(mon = 'ann') %>%
   mutate(type_prd = paste0(cur_yr,'_ann_ano_sp')) %>%
  dplyr::select(-rst_fl) -> ann_ano_sp_summ_tbl
 ann_ano_sp_summ_tbl

 ### Annual anomaly spatial trend summary ------------------------
 # Calculate spatial trend and rate of change summary
 # Spatial trend 1950 -now

 ann_ano_r_1950 <- ann_ano_r[[which(as.numeric(names(ann_ano_r)) >= 1950)]]

 sp_trn_mag_1950 <- app(ann_ano_r_1950, sen_slope_fun)
 sp_trn_sig_1950 <- app(ann_ano_r_1950, mk_pval_fun)

 sp_trn_1950 <- c(sp_trn_mag_1950, sp_trn_sig_1950)
 names(sp_trn_1950) <- c("sen_slope_1950", "mk_pval_1950")

 # Spatial trend  1980 -now
 ann_ano_r_1980 <- ann_ano_r[[which(as.numeric(names(ann_ano_r)) >= 1950)]]

 sp_trn_mag_1980 <- app(ann_ano_r_1980, sen_slope_fun)
 sp_trn_sig_1980 <- app(ann_ano_r_1980, mk_pval_fun)

 sp_trn_1980 <- c(sp_trn_mag_1980, sp_trn_sig_1980)
 names(sp_trn_1980) <- c("sen_slope_1980", "mk_pval_1980")
 # plot(sp_trn_1980)

 ann_sp_trns_dt <- c(sp_trn_1950, sp_trn_1980)

 # Spatial trend summary
 ann_sp_trn_summ_tbl <- spatial_summ_fun(
   rast_dt = ann_sp_trns_dt
 )
 ann_sp_trn_summ_tbl%>%
   mutate(parr = parr) %>%
   mutate(mon = 'ann'
   )%>%
   rename(type_prd = rst_fl) -> ann_sp_trn_summ_tbl
 ann_sp_trn_summ_tbl

 ### Spatial y averaged time series anomaly and trend plot ----------------------
 ano_ann_ts_dt <- tibble(
   yr  = as.numeric(names(ann_ano_r)),
   ano = global(ann_ano_r, "mean", na.rm = TRUE)[, 1]
 ) %>%
   drop_na() %>%
   mutate(par = parr, region = region)

# Time series trends
# Mean trend 1950s
 ann_ts_trn_1950 <- zyp.sen(ano ~ yr,  ano_ann_ts_dt  %>% filter(yr >= 1950))

 ann_ts_trn_1950_tbl <- tibble(
   type_prd = "1950-ts-summ-trn",
   slope  = ann_ts_trn_1950$coefficients[[2]],
   intercept = ann_ts_trn_1950$coefficients[[1]],
   mk_pval = MannKendall(
     ano_ann_ts_dt%>% filter(yr >= 1950) %>% pull(ano)
   )[[2]]
 ) %>%
   mutate(parr = parr, mon = 'ann')

 # Mean trend 1980s
 ann_ts_trn_1980 <- zyp.sen(ano ~ yr,  ano_ann_ts_dt  %>% filter(yr >= 1980))

 ann_ts_trn_1980_tbl <- tibble(
   type_prd = "1950-ts-summ-trn",
   slope  = ann_ts_trn_1980$coefficients[[2]],
   intercept = ann_ts_trn_1980$coefficients[[1]],
   mk_pval = MannKendall(
     ano_ann_ts_dt%>% filter(yr >= 1980) %>% pull(ano)
   )[[2]]
 ) %>%
   mutate(parr = parr,
          mon = 'ann')

 # Final annual summary table
 ann_trn_summ_tbl <- bind_rows(
   ann_ts_trn_1950_tbl,
   ann_ts_trn_1980_tbl
 )
 ann_trn_summ_tbl %>%
   mutate( min = min(ano_ann_ts_dt$ano),
           max = max(ano_ann_ts_dt$ano),
           mean = mean(ano_ann_ts_dt$ano),
           yr = cur_yr )
 ann_trn_summ_tbl

 # Time series plot
 ano_ann_ts_dt

 ann_ts_minval <- -max(abs(ano_ann_ts_dt$ano))
 ann_ts_maxval <-  max(abs(ano_ann_ts_dt$ano))
 ann_ts_brks <- calc_sym_breaks(ann_ts_minval, ann_ts_maxval)

  # Title and y-axis label
 par_title <- if (parr %in% c("prcp", "soil_moisture")) {
   paste0("BC ", parr_full, " (% of normal) 1950–", cur_yr)
 } else {
   paste0("BC ", parr_full, " (", unt, ") 1950–", cur_yr)
 }

 y_axis_lab <- if (parr %in% c("prcp", "soil_moisture")) {
   paste0(parr_full, " anomaly (% of normal)")
 } else {
   paste0(parr_full, " anomaly (", unt, ")")
 }

 # plot
 ann_ano_ts_plt <- ggplot(ano_ann_ts_dt, aes(x = yr, y = ano)) +
   geom_bar(stat = "identity", aes(fill = ano), width = 0.7, show.legend = FALSE) +
   geom_smooth(method = "loess", color = "magenta", size = 1.2, se = FALSE) +
   geom_hline(yintercept = 0, color = "gray10", linewidth = 0.5) +

   scale_fill_gradientn(colours = cpt("ncl_BlWhRe", 100),
                        limits = c((-1* max(abs(c(ann_ts_minval, ann_ts_maxval)))),
                                   (1* max(abs(c(ann_ts_minval, ann_ts_maxval))))),
                        breaks = ann_ts_brks,
                        na.value = "transparent")+
   theme_ts_linear()+
   scale_x_continuous(
     name = " ",
     breaks = seq(1950, cur_yr, 1),
     expand = c(0.02, 0.02)
   ) +
   scale_y_continuous(name = y_axis_lab,
                      limits = c((-1* max(abs(c(ann_ts_minval, ann_ts_maxval)))),
                                 (1* max(abs(c(ann_ts_minval, ann_ts_maxval))))),
                      breaks = ann_ts_brks)
 labs(title = par_title)
 ann_ano_ts_plt

 if (parr %in% c("prcp", "soil_moisture"))  {
   ann_ano_ts_plt <- ann_ano_ts_plt +
     scale_fill_gradientn(
       name = paste0(parr, " anomaly ", unt),
       colours = cpt("cmocean_curl", 100, rev = TRUE),
       limits = c((-1* max(abs(c(ann_ts_minval, ann_ts_maxval)))),
                  (1* max(abs(c(ann_ts_minval, ann_ts_maxval))))),
       breaks = ann_ts_brks,
       na.value = "transparent")

 } else {
   ann_ano_ts_plt <- ann_ano_ts_plt
 }
 ann_ano_ts_plt

 ggsave(
   paste0(ann_summ_rslts_pth,parr,"_annual_ano_ts_plt_1950_", cur_yr,
          ".png"),
   plot =ann_ano_ts_plt,
   width = 16,
   height = 7.5,
   units = "in",
   dpi = 320,
   scale = 0.8,
   limitsize = F
 )

 ###  Final summary table ----------------------------------
 mon_summ_tbl%<>%
   mutate(type_prd = as.character(type_prd))

 ann_ano_sp_summ_tbl
 ann_sp_trn_summ_tbl
 ann_trn_summ_tbl

 fnl_summ_tbl <- bind_rows(mon_summ_tbl,
                           ann_ano_sp_summ_tbl,
                           ann_sp_trn_summ_tbl,
                           ann_trn_summ_tbl)
 fnl_summ_tbl



## Ranking table  ----------------------------------
ano_ann_ts_dt %>%
  mutate(ano_rank = rank(-rank(ano)))%>%
  dplyr::select(yr, rank = ano_rank, change = ano) %>%
  arrange(rank) %>%
  mutate(parr = parr) -> ann_ano_rnk_chng_tbl

write_csv(
  ann_ano_rnk_chng_tbl,
  paste0(
    ann_summ_rslts_pth,
    parr,
    '_bc_ann_ranking_change_table_', cur_yr,'.csv'
  )
)

## return final items list  -------------------------------------------------------------
  list(
    mon_ano_plt =  mon_ano_plt,
    ann_ano_plt  = yr_ann_sp_ano_plt,
    ann_ts_plt   = ann_ano_ts_plt,
    ann_ts_rnk_tbl    = ann_ano_rnk_chng_tbl,
    summ_tbl =  fnl_summ_tbl
  )
}
