# Required ----------------------------------------------------------------
rqr_pkgs <-
  c("tidyverse",
    'lubridate',
    'magrittr',
    'RColorBrewer',
    'stringr',
    'scales',
    'glue')
# Install packages if not
installed_rqr_pkgs <- rqr_pkgs %in% rownames(installed.packages())
if (any(installed_rqr_pkgs == FALSE)) {
  install.packages(rqr_pkgs[!installed_rqr_pkgs])
}
# Load:
lapply(rqr_pkgs , require, character.only = TRUE)

# Stripe plots ------------------------------------------------------------
# Annual----------------
cur_yr <- as.character(as.integer(format(Sys.Date(), "%Y")) - 1)

bc_tmn_ann_ano_dt <- read_csv('BC_tmean_anomaly_timeseries_annual_1951_2025_data.csv')

# stripe plot of BC's annual temperature --- with title
bc_tmn_ann_ano_dt %>%
  ggplot(aes(x = yr, y = 1, fill = ano)) +
  geom_tile(show.legend = FALSE, alpha =1) +
  scale_fill_stepsn(
    colors = c('#04346c', '#0483c3', 'white', '#ed2016', '#740404'),
    values = scales::rescale(c(
      min(bc_tmn_ann_ano_dt$ano), 0, max(bc_tmn_ann_ano_dt$ano)
    )),
    n.breaks = 12
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  scale_x_continuous(breaks = seq(1950, cur_yr, 10)) +
  labs(
    title = glue(
      'BC mean annual temperature change ({min(bc_tmn_ann_ano_dt$yr)}-{max(bc_tmn_ann_ano_dt$yr)})'
    )
  ) +
  theme(
    axis.text.x = element_text(
      color = 'white',
      margin = margin(t = 5, b = 10, unit = 'pt')
    ),
    plot.title = element_text(
      color = 'white',
      margin = margin(t = 5, b = 10, unit = 'pt'),
      hjust = 0.05
    ),
    plot.background = element_rect(fill = 'black'),
  )

ggsave(
  'bc_annual_tmean_ano_stripe_withtitle.png',
  width = 20,
  height = 11,
  units = "in",
  dpi = 350,
  scale = 1,
  limitsize = F
)

# stripe plot of BC's annual temperature --- blank
bc_tmn_ann_ano_dt %>%
  ggplot(aes(x = yr, y = 1, fill = ano)) +
  geom_tile(show.legend = FALSE, alpha =1) +
  scale_x_continuous(expand = c(0.00,0.00))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_stepsn(
    colors = c('#04346c', '#0483c3', 'white', '#ed2016', '#740404'),
    values = scales::rescale(c(
      min(bc_tmn_ann_ano_dt$ano), 0, max(bc_tmn_ann_ano_dt$ano)
    )),
    n.breaks = 12
  ) +
  coord_cartesian(expand = T) +
  theme_void()+
  theme(plot.background = element_rect(fill ='black', colour = 'black'))

ggsave(
  'bc_annual_tmean_ano_stripe.png',
  width = 20,
  height = 11,
  units = "in",
  dpi = 350,
  scale = 1,
  limitsize = F
)

# Copy stripes plots to www folder for anomaly app
from_files <- c(
  "bc_annual_tmean_ano_stripe.png",
  "bc_annual_tmean_ano_stripe_withtitle.png"
)
from_files
file.exists(from_files)

# destination relative to getwd()
to_dir <- "../www"

dir.exists(to_dir)

# copy files
file.copy(
  from = from_files,
  to   = to_dir,
  overwrite = TRUE
)


