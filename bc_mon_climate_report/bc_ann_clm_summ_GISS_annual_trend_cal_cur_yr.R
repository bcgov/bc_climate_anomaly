rm(list = ls())
# READ GISS temeprature data, clip for BC and calcualte annual trend and anomaly for relative comaprison with ERA5 land to include in annual status report

# Required + Paths ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)
library(zoo)
library(fs)
library(zyp)

library(terra)

grd_dt_pth <- '../../data_database/data_gistemp/'
shp_dt_pth <- '../../data_database/data_shapefiles/'

report_yr <- '2025'

# Read gridded temp data and crop for BC ---------------
gis_mon_ano_tmp_dt <- list.files(
  path = grd_dt_pth,
  pattern = ".nc",
  full.names = TRUE
) %>%
  tibble(dt_pth = .) %>%
  pull(dt_pth) %>%
  rast()
gis_mon_ano_tmp_dt
plot(gis_mon_ano_tmp_dt)
names(gis_mon_ano_tmp_dt)
time(gis_mon_ano_tmp_dt)
names(gis_mon_ano_tmp_dt) <- time(gis_mon_ano_tmp_dt)

# BC shapefile
bc_shp <-
  list.files(
    path = shp_dt_pth,
    pattern = "\\.gpkg$",
    full.names = TRUE
  ) |>
  (\(x) x[str_detect(x, "bc_shapefile")])() |>
  vect()
plot(bc_shp)

# Crop/mask for BC
gis_mon_ano_tmp_bc <- crop( gis_mon_ano_tmp_dt, bc_shp, mask =T)
plot(gis_mon_ano_tmp_bc, (nlyr(gis_mon_ano_tmp_bc)-10): nlyr(gis_mon_ano_tmp_bc))

# Spatial average monthly anomaly
bc_gis_ann_ano_df <-
tibble(
    yr_mn = as.Date(names(gis_mon_ano_tmp_bc)),
    ano     = global(gis_mon_ano_tmp_bc, fun = mean, na.rm = TRUE)[, 1]
  ) %>%
  mutate(yr = year(yr_mn)) %>%
  group_by(yr) %>%
  summarise(ano = mean(ano))
bc_gis_ann_ano_df
tail(bc_gis_ann_ano_df)

ggplot(bc_gis_ann_ano_df) +
  geom_line(aes(x= yr, y = ano))

# trend
gis_ann_ts_trn_1950 <- zyp.sen(ano ~ yr,  bc_gis_ann_ano_df %>% filter(yr >= 1950))
gis_ann_ts_trn_1950

gis_ann_ts_trn_1950_tbl <- tibble(
  type_prd = "1950-ts-summ-trn",
  slope  = gis_ann_ts_trn_1950$coefficients[[2]],
  intercept = gis_ann_ts_trn_1950$coefficients[[1]],
  mk_pval = MannKendall(
    bc_gis_ann_ano_df %>% filter(yr >= 1950) %>% pull(ano)
  )[[2]]
)

# Mean trend 1980s
gis_ann_ts_trn_1980 <- zyp.sen(ano ~ yr,   bc_gis_ann_ano_df  %>% filter(yr >= 1980))

gis_ann_ts_trn_1980_tbl <- tibble(
  type_prd = "1980-ts-summ-trn",
  slope  = gis_ann_ts_trn_1980$coefficients[[2]],
  intercept = gis_ann_ts_trn_1980$coefficients[[1]],
  mk_pval = MannKendall(
    bc_gis_ann_ano_df  %>% filter(yr >= 1980) %>% pull(ano)
  )[[2]]
)
gis_ann_ts_trn_1980_tbl
gis_ann_ts_trn_1950_tbl

# Time series plot

## Plot help functions -----------------
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

# FinaL PLOT

bc_gis_ann_ano_df

ann_ts_minval <- -max(abs(bc_gis_ann_ano_df$ano))
ann_ts_maxval <-  max(abs(bc_gis_ann_ano_df$ano))
ann_ts_brks <- calc_sym_breaks(ann_ts_minval, ann_ts_maxval)

parr <- 'tmean'
region <- 'BC'
report_yr <- 2025
parr_full <- 'GISS TMean'
unt <- '°C'

# Title and y-axis label
par_title <- if (parr %in% c("prcp", "soil_moisture")) {
  paste0(region,' ', parr_full, " (% of normal) 1950–", report_yr)
} else {
  paste0(region,' ', parr_full, " (", unt, ") 1950–", report_yr)
}

y_axis_lab <- if (parr %in% c("prcp", "soil_moisture")) {
  paste0(parr_full, " anomaly (% of normal)")
} else {
  paste0(parr_full, " anomaly (", unt, ")")
}

bc_gis_ann_ano_df %>%
  filter(yr >= 1950)

# plot
giss_ann_ano_ts_plt <- ggplot(bc_gis_ann_ano_df, aes(x = yr, y = ano)) +
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
    breaks = seq(1800, report_yr, 2),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(name = y_axis_lab,
                     limits = c((-1* max(abs(c(ann_ts_minval, ann_ts_maxval)))),
                                (1* max(abs(c(ann_ts_minval, ann_ts_maxval))))),
                     breaks = ann_ts_brks)
labs(title = par_title)
giss_ann_ano_ts_plt



# End of script -------------
