## R Shiny app An app to visualize monthly and annual temperature and precipitation anomalies in BC and its sub regions ( eco-regions and watersheds) along with their trends.
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

# Run this after running trend calculation, bc monthly report quarto and upload history scripts.

# Required -------------------
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library("shinythemes")
library("shinyjs")
library('shinyalert')
library('shinycssloaders')
library('plotly')

library('markdown')
library('rmarkdown')

library('terra')
library('tidyterra')
library('leaflet')

library('tidyverse')
library('magrittr')
library('lubridate')

library('zoo')
library('zyp')
library('colorspace')
library('cptcity')


# Load and process input data -------
## Paths --
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shp_fls_pth <- './shapefiles/'
ano_dt_pth <-  './ano_clm_trn_data/'

# Credit  -----
plt_wtrmrk <-
  "@Aseem R. Sharma, BC Ministry of Forests. Data credit: ERA5land/C3S/ECMWF."
plt_wtrmrk

# Date of deployment -----------------------
app_deployment_date <- format(Sys.Date(), "%d %B, %Y")
app_deployment_date

# Shape files --------------
# Domain
xmi = -140
xmx = -108
ymi = 39
ymx = 60

# List of shape files
list.files(path = shp_fls_pth,
           pattern = "\\.(shp|gpkg)$",
           full.names = TRUE,
           ignore.case = TRUE) -> shp_fls_lst
shp_fls_lst

# Western North America
na_shp <-   vect(shp_fls_lst[str_detect(shp_fls_lst, "north_america") == T])
# plot(na_shp)
wna_shp <- crop(na_shp, ext(xmi,xmx,ymi,ymx))
# plot(wna_shp)

# BC
bc_shp <-vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_shapefile") == T])
# plot(bc_shp)

# BC eco-province
bc_ecoprv_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoprovince") == T])
# plot(bc_ecoprv_shp)
# text(bc_ecoprv_shp, "code", cex = 0.8, col = "black")

# Remove coastal ecoprovince
bc_ecoprv_shp %<>%
  filter(code != 'NEP')
# plot(bc_ecoprv_shp)
# text(bc_ecoprv_shp, "code", cex = 0.8, col = "black")

# BC eco-regions
bc_ecorgn_shp <-vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecoregions") == T])
# plot(bc_ecorgns_shp)

# BC eco-sections
bc_ecosec_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_ecosections") == T])
# plot(bc_ecosec_shp)
# bc_ecosec_shp$ECOSEC_NM
bc_ecosec_shp <- project(bc_ecosec_shp, "EPSG:4326")

# FLP tables ( Forest landscape planning)
bc_flp_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "flp") == T])
# plot(bc_flp_shp)

bc_flp_shp %<>%
  mutate(flp_unit_nam = paste0('FLP- ', ORG_UNIT))

# BC watersheds
bc_wtrshd_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_watersheds") == T])
# plot(bc_wtrshd_shp)

# BC FWA watersheds ( Freshwater atlas watersheds)
bc_fwa_shp <- vect(shp_fls_lst[str_detect(shp_fls_lst, "fwa_watersheds") == T])
# plot(bc_wtrshd_shp)
bc_fwa_shp <- project(bc_fwa_shp, "EPSG:4326")

# BC municipalities
bc_muni_shp <-  vect(shp_fls_lst[str_detect(shp_fls_lst, "bc_municipalities") == T])
# plot(bc_muni_shp)
bc_muni_shp <- project(bc_muni_shp, "EPSG:4326")

## Months, parameters ----
months_nam <-
  c(
    "annual","winter","spring","summer","fall",
    "Jan","Feb","Mar","Apr","May","Jun","Jul",
    "Aug","Sep","Oct","Nov","Dec"
  )
months_nam

parameters <- c("tmean", "tmax", "tmin", "prcp","vpd","rh","soil_moisture")
parameters

min_year <- 1951
max_year <- 2026 # current year of preparation

update_month <- "December"
update_year <- "2025"

years <- seq(min_year, max_year, 1)
yr_choices <- sort(years, decreasing = T)

report_years <- c(2023,2024,2025)

## Anomalies climatology and trend Data files -----
list.files(path = ano_dt_pth,
           pattern = ".nc",
           full.names = T) -> ano_clm_trn_dt_fls
ano_clm_trn_dt_fls

ano_clm_trn_dt_fl <- tibble(dt_pth = ano_clm_trn_dt_fls) %>%
  mutate(fl_nam = basename(dt_pth)) %>%
  mutate(
    par = str_extract(fl_nam, paste(parameters, collapse = "|")),                # prcp
    dt_type = str_extract(fl_nam, "(ano|clm|spatial_trend)"),     # ano, clm, spatial_trend
    mon = str_extract(fl_nam, "(annual|fall|summer|winter|spring|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"),
    start_year = str_extract(fl_nam, "(19|20)\\d{2}")              # 1950 or 1980
  ) %>%
  # Optional cleanup
  mutate(
    dt_type = case_when(
      dt_type == "spatial_trend" ~ "trend",
      TRUE ~ dt_type
    )
  ) %>%
  dplyr::select(-fl_nam)
ano_clm_trn_dt_fl

# For summary reports ---------------
report_suffixes <- c(
  "ann2025",
  "dec2025", "nov2025","oct2025","sep2025","aug2025","jul2025","jun2025", "may2025","apr2025", "mar2025", "feb2025", "jan2025",
  "ann2024",
  "dec2024", "nov2024", "oct2024", "sep2024",
  "aug2024", "jul2024", "jun2024", "may2024", "apr2024", "mar2024",
  "feb2024", "jan2024", "dec2023", "nov2023", "oct2023", "sep2023",
  "longterm"
)


#  UI --------------------------------------
ui <- fluidPage(
  navbarPage(
    id = "bc_clm",
    title = "BC Climate Anomaly",
    theme = "bcgov.css",
    selected = "ano_app",

    ## Intro page --------------------------
    tabPanel(
      title = "Introduction",
      value = "intro",
      column(
        width = 12,
        wellPanel(
          HTML(
            "<h3><b>BC climate anomaly app</b>: Visualizing Climate Anomalies in British Columbia (BC) </h2>"
          )),
          includeMarkdown("intro_bc_climate_anomaly_app.Rmd"),
        column(
          width = 12,
          HTML(
            "<h4><b>Citation</b></h4>
                            <h5> <u>Please cite the contents of this app as:</u>
                            <br>
                            Sharma, A.R. 2023. BC climate anomaly app: Visualizing monthly, seasonal, and annual climate anomalies in British Columbia (BC).</a>
                            British Columbia Ministry of Forests.
                  <a href='https://bcgov-env.shinyapps.io/bc_climate_anomaly/'
            target='_blank'>https://bcgov-env.shinyapps.io/bc_climate_anomaly/</a> </h5>"
          )
        ),
        column(
          width = 12,
          HTML(
            "<h5> <u>App created by:</u>
             <br>
             <b>Aseem R. Sharma, PhD</b><br>
              Research Climatologist<br>
              FFEC, FEA, OCF, BC Ministry of Forests<br>
              <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> <br>
              <br>
              <h4><b>Code</b></h4>
              <h5> The code and data of this app are available through GitHub at <a href='https://github.com/bcgov/bc_climate_anomaly.git' target='_blank'> https://github.com/bcgov/bc_climate_anomaly.</a></h5>"
          )
        ),
        column(
          width = 12,
          HTML(
            "<h5> <b>Disclaimer</b><h5>
              <h8>  This app and the climate reports here have been prepared using <a href='https://www.ecmwf.int/en/era5-land'>ERA5-Land</a> data
              from the European Centre for Medium-Range Weather Forecasts (ECMWF),
              as available at the time of preparation.
              Please note that the original data may be subject to updates or revisions.
              Any modifications to the original data may result in adjustments to the findings presented in this report.</h8>"
          )
        ),
        column(width = 12,
               textOutput("deploymentDate"),),

    ###### footer ----------------------------
        column(
          width = 12,
          style = "background-color:#003366; border-top:2px solid #fcba19;",
          column(
            width = 12,
            style = "background-color:#003366; border-top:2px solid #fcba19;",
            tags$footer(
              class = "footer",
              tags$div(
                class = "container",
                style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(
                  style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  )
                )
              )
            )
          )
        )
      )
    ),

    ## About page ---------------------
    tabPanel(
      title = "About",
      value = "about",
      withMathJax(includeMarkdown("about_bc_climate_anomaly_app.Rmd")),
      ###### footer ---------------------------
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        column(
          width = 12,
          style = "background-color:#003366; border-top:2px solid #fcba19;",
          tags$footer(
            class = "footer",
            tags$div(
              class = "container",
              style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
              tags$ul(
                style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                tags$li(
                  a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                ),
                tags$li(
                  a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                      "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                ),
                tags$li(
                  a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                      "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                ),
                tags$li(
                  a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                      "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                ),
                tags$li(
                  a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                      "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                ),
                tags$li(
                  a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                      "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                )
              )
            )
          )
        )
      )
    ),

    ## Ano App page ----------------------------
    tabPanel(
      title = "Anomaly app",
      value = "ano_app",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "selection-panel",
          # style = "position:fixed; width:24%; max-height: 100vh;",
          width = 3,

          ##### Filters/selectors ------------------
          # Custom CSS
          tags$head(tags$style(HTML("
    select.form-control {
      transition: background-color 0.3s ease;
    }
    select.form-control:focus {
      background-color: #d4edda !important;
    }
    .selectize-dropdown .option {
      border-bottom: 1px solid #ccc;
      padding: 6px 10px;
    }
  "))),
          helpText(HTML("<h4><b> Filter/Selections</b> </h4>",)),
          helpText(HTML('
  <style>
    .flash-text {
      color: red;
      animation: flash 1s infinite;
    }
    @keyframes flash {
      0%   { opacity: 1; }
      50%  { opacity: 0; }
      100% { opacity: 1; }
    }
  </style>
  <p>After selection, click <b><i class="flash-text">Run Analysis</i></b> to get outputs.</p>
')),
            # helpText(HTML("<h4><b> Filter/Selections</b> </h4>",)),
          # helpText(HTML("<p> After selection click <i> <b> Run Analysis</i></b> to get outputs. </p>",)),
          fluidRow(
            useShinyjs(),
            pickerInput(
              "major_area",
              "Select region ",
              choices = c("Western North America",
                          "BC", "Ecoprovinces", "Ecoregions", 'Ecosections', "Major watersheds",
                          'FWA watersheds', 'FLP boundaries', 'Municipalities'),
              selected = 'BC'
            ),
            hidden(
              pickerInput(
                "ecoprov_area",
                "Ecoprovinces",
                choices = c("Ecoprovinces (select one)", c(bc_ecoprv_shp$name)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "ecorgn_area",
                "Ecoregions",
                choices = c("Ecoregions (select one)", c(bc_ecorgn_shp$CRGNNM)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "ecosec_area",
                "Ecosections",
                choices = c("Ecosections (select one)", c(bc_ecosec_shp$ECOSEC_NM)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "wtrshd_area",
                "Watershed",
                choices = c("Major watersheds (select one)", c(bc_wtrshd_shp$MJR_WTRSHM)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "fwa_area",
                "FWA watersheds",
                choices = c("FWA watersheds (select one)", c(bc_fwa_shp$WATERSHE_2)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "flp_area",
                "FLP boundaries",
                choices = c("FLP boundaries (select one)", c(bc_flp_shp$flp_unit_nam)),
                multiple = F
              )
            ),
            hidden(
              pickerInput(
                "muni_area",
                "Municipalities",
                choices = c("Municipalities (select one)", c(bc_muni_shp$ABRVN)),
                multiple = F
              )
            ),
            HTML("(Western North America, BC, Eco-provinces/regions/sections,
                 Major Watersheds, FWA watersheds, FLP boundaries, Municipalities)"),
          ),
          br(),
          fluidRow(offset = 3,
                   # div(style = "height:70px;width:100%;background-color: #999999;border-style: dashed;border-color: #000000",)
                   uiOutput("par_picker"),
                   HTML("(Temperature, VPD, Precipitaiton, RH, Soil moisture)"),),
          br(),
          fluidRow(title = "Month",
                   uiOutput("month_picker")),
          br(),
          fluidRow(
            helpText(HTML("<h5><b> Choose range of years or specific year(s)</b> </h5>",)),
            actionButton("rng_years_choose", "Range of years"),
            actionButton("ab_years_choose", "Specific year(s)"),
           sliderInput(
              "year_range",
              "year range",
              min_year,
              max_year
              ,
              value = c((max_year - 5), (max_year)),
              sep = ""
            ),
            chooseSliderSkin(skin = "Shiny"),
            tags$style(
              HTML(
                ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}"
              )
            ),
            hidden(selectInput("year_specific",
                        "year(s)",
                        choices = yr_choices,
                        multiple = T,
                        selected = max_year)),

            # Run analysis and Reset selection
           # Run analysis and Reset selection
           br(),
           actionButton("run_ana_button", tags$b(tags$span(style = "color: red;", "Run analysis"))),
           actionButton("reset_input", "Reset"),
           br()
          ),
          fluidRow(column(
            HTML("<h4><b>Location Map</b> </h4>"),
            title = "Map Location",
            width = 12,
            withSpinner(leafletOutput("loc_map", height = "22vh"),type = 6)
          )),
          br(),
          br(),
          fluidRow(column(width = 12, wellPanel(
            style = "background-color: white;",
            HTML(
              '<h4>For climate extreme indices (CEI) refer to <a href="https://bcgov-env.shinyapps.io/bc_climate_extremes_app/" target="_blank"><b>bc_climate_extremes_app</b></a></h4>'
            )
            ,
          ))),

      ),
        mainPanel(
          tags$head(tags$style(HTML(
            '.box {margin: 25px;}'
          ))),
          width = 9,
      ##### Linear trends and spatial anomaly map plots and summary ---------------------
          column(width = 10,
                 wellPanel(
                   HTML("<h4><b> Time series, linear trends and spatial anomaly maps</b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            offset = 0.1,
            tabBox(
              width = 12,
              tabPanel(
                width = 12,
                status = 'primary',
                title = "Time-series plot",
                withSpinner(plotlyOutput("lnr_trn_plt", height = "60vh"),type =6),
                downloadButton(outputId = "download_lnr_trn_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_ano_ts_data",
                               label = "Download anomaly time series data"),
              ),
              tabPanel(
                width = 12,
                status = 'primary',
                title = "Spatial anomaly maps",
                withSpinner(plotOutput("sptl_ano_map", height = "70vh"),type =6),
                downloadButton(outputId = "download_sptl_ano_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_sptl_ano_data",
                               label = "Download raster data"),
              ),
            )
          )),
  ###### climate normal map and  spatial trends maps (1950s 1980s) --------------------------
          fluidRow(
            box(
              width = 4,
              align="left",
              wellPanel(HTML(
                "<h5><b>Climate Normal (1981-2010)</b> </h5>"
              )),
              uiOutput("clm_nor_title", height = "30vh"),
              withSpinner(plotOutput("clm_nor_map", width = "100%", height = "30vh"),type =6),
              downloadButton(outputId = "download_clm_nor_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_clm_nor_data",
                             label = "Download raster data"),
            ),
            box(
              width = 4,
              align="left",
              wellPanel(HTML(
                "<h5><b> Spatial trends since 1950 </b> </h5>"
              )),
              uiOutput("clm_trn50_title", height = "30vh"),
              withSpinner(plotOutput("clm_trn50_map", width = "100%", height = "30vh"),type =6),
              downloadButton(outputId = "download_clm_trn50_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_clm_trn50_data",
                             label = "Download raster data"),
            ),
            box(
              width = 4,
              align="left",
              wellPanel(HTML(
                "<h5><b> Spatial trends since 1980 </b> </h5>"
              )),
              uiOutput("clm_trn80_title", height = "30vh"),
              withSpinner(plotOutput("clm_trn80_map", width = "100%", height = "30vh"),type =6),
              downloadButton(outputId = "download_clm_trn80_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_clm_trn80_data",
                             label = "Download raster data"),
            )
          ),

  ##### App disclaimer -----------------------

  column(width = 12,
           HTML("<h5><b> Disclaimer:</b> </h5> <h6> This analysis utilizes ERA5-Land data.
                Any modifications to the dataset or discrepancies in the results due to data
                changes should be carefully considered by users. </h6>")
         ),
        ),
      ),

      ##### footer ---------------------------
      HTML("<br>",
           "<br>"),
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;position:relative;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    ),

    ## Report -----------------------------
    tabPanel(
      title = "Reports",
      value = "report",
      column(
        width = 12,

        wellPanel(
          HTML("<h3><b>BC climate summary and anomaly reports</b></h3>"),
          HTML("<h4>Monthly summaries, annual reports, and long-term trends (HTML)</h4>")
        ),

        fluidRow(
          box(
            width = 12,
            status = "primary",

            tags$div(
              style = "
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
            gap: 24px;
          ",

              lapply(unique(report_years), function(yr) {
                tags$div(
                  style = "
                border: 1px solid #ddd;
                border-radius: 6px;
                padding: 12px;
                background-color: #fafafa;
              ",

                  tags$h4(
                    style = "text-align: center; margin-bottom: 12px;",
                    yr
                  ),

                  uiOutput(paste0("reports_year_", yr))
                )
              })
            )
          )
        )
      ),

      ###### footer ----------------------------
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    ),
    ## Climate Stripes  -----------------------
    tabPanel(
      title = "Climate stripes",
      value = "clm_stripes",
      column(
        width = 12,
        wellPanel(
          HTML(
            "<h3><b>BC climate stripes </b> </h2>"
          ))),
          HTML(
            "<h5> Inspired by the work of British climate scientist <a href= 'https://showyourstripes.info/'> Prof. Ed Hawkins </a> ,
            the climate stripes (also known as warming stripes) visually represent
            changes in annual temperatures relative to the long-term average.
            Below are the 'climate stripes' plots for British Columbia (BC) since 1950.
            Each stripe corresponds to a single year's temperature compared to the 1981–2010 average.
            Red stripes indicate warmer-than-average years, while blue stripes represent cooler-than-average years.
            The intensity of the color reflects the magnitude of the difference from the average.
            <br>
            Feel free to download and use these visuals!
            <br>
            <br> </h5>"
          ),
          fluidRow(
            wellPanel(HTML(
              "<h3><b>  BC climate stripes (mean temperature): with title </b> </h3>"
            )),
            box(
              width = 12,
              height = "100vh",
              status = "primary",
              downloadButton(outputId = "clm_strp_plt_ttl_dnwld",
                             label = "Download climate stripe plot with title"),
              imageOutput("bc_clm_strp_withtitle"))
          ),
      fluidRow(
        wellPanel(HTML(
          "<h3><b>  BC climate stripes (mean temperature): without title </b> </h3>"
        )),
        box(
          width = 12,
          height = "100vh",
          status = "primary",
          downloadButton(outputId = "clm_strp_plt_wttl_dnwld",
                         label = "Download climate stripe plot wihtout title"),
          imageOutput("bc_clm_strp_withouttitle"))
      ),
        ###### footer ---------------------
        column(
          width = 12,
          style = "background-color:#003366; border-top:2px solid #fcba19;",
          column(
            width = 12,
            style = "background-color:#003366; border-top:2px solid #fcba19;",
            tags$footer(
              class = "footer",
              tags$div(
                class = "container",
                style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(
                  style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  ),
                  tags$li(
                    a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                        "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
                  )
                )
              )
            )
          )
        )
      ),

    ## Feedback and links --------------------------
    tabPanel(
      title = "Feedback & Links",
      value = "feed_link",
      column(width = 12,
             wellPanel(HTML(
               "<h3><b>Feedback</h3>"
             )), fluidRow(
               box(
                 width = 12,
                 status = 'primary',
                 # title = "Note",
                 uiOutput("feedback_text"),
               )
             )),
      column(
        width = 12,
        wellPanel(HTML("<h4><b>Links to other app </h4>")),
        HTML(
          "<h5><b>Here are the links to other apps developed in FFEC.</b></h5>
          <a href= 'https://bcgov-env.shinyapps.io/cmip6-BC/'> CMIP6-BC </a>
                               <br>
         <a href= 'https://bcgov-env.shinyapps.io/bc_climate_extremes_app/'> BC_climate_extremes_app </a>
                               <br>
          <br>"
        )
      ),
      ###### footer -----------------------
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    )
  )
)


# Define server and reactive contents from data

# Server ----
server <- function(session, input, output) {
  options(warn = -1)

# Maps and plots tab ---------------------
  # Filters, selections, namings  -----------------------
  ## Filter : Area ----------------------------
    observeEvent(input$major_area, {
      if (input$major_area == "Ecoprovinces") {
        showElement("ecoprov_area")
        hideElement("wtrshd_area")
        hideElement("fwa_area")
        hideElement("flp_area")
        hideElement("ecorgn_area")
        hideElement("ecosec_area")
        hideElement("muni_area")
      } else if (input$major_area == "Ecoregions"){
        hideElement("ecoprov_area")
        hideElement("wtrshd_area")
        hideElement("fwa_area")
        hideElement("flp_area")
        showElement("ecorgn_area")
        hideElement("ecosec_area")
        hideElement("muni_area")
      } else if (input$major_area == "Ecosections"){
        hideElement("ecoprov_area")
        hideElement("wtrshd_area")
        hideElement("fwa_area")
        hideElement("flp_area")
        hideElement("ecorgn_area")
        showElement("ecosec_area")
        hideElement("muni_area")
      } else if (input$major_area == "Major watersheds") {
        hideElement("ecoprov_area")
        showElement("wtrshd_area")
        hideElement("fwa_area")
        hideElement("flp_area")
        hideElement("ecorgn_area")
        hideElement("ecosec_area")
        hideElement("muni_area")
      } else if (input$major_area == "FWA watersheds") {
        hideElement("ecoprov_area")
        hideElement("wtrshd_area")
        showElement("fwa_area")
        hideElement("flp_area")
        hideElement("ecorgn_area")
        hideElement("ecosec_area")
        hideElement("muni_area")
      } else if (input$major_area == "Municipalities") {
        hideElement("ecoprov_area")
        hideElement("wtrshd_area")
        hideElement("fwa_area")
        hideElement("flp_area")
        hideElement("ecorgn_area")
        hideElement("ecosec_area")
        showElement("muni_area")
      }  else if (input$major_area == "FLP boundaries") {
        hideElement("ecoprov_area")
        hideElement("wtrshd_area")
        hideElement("fwa_area")
        showElement("flp_area")
        hideElement("ecorgn_area")
        hideElement("ecosec_area")
        hideElement("muni_area")
      } else {
        hideElement("muni_area")
        hideElement("ecosec_area")
        hideElement("ecorgn_area")
        hideElement("ecoprov_area")
        hideElement("wtrshd_area")
        hideElement("fwa_area")
        hideElement("flp_area")
      }
    })

    ### Area (interactive shapefiles )

    get_shapefile <- reactive({
      if (input$major_area == "Western North America") {
        sel_area_shpfl <- wna_shp

      } else if (input$major_area == "BC") {
        sel_area_shpfl <- bc_shp

      } else if (input$major_area == "Ecoprovinces") {
        if (input$ecoprov_area == "Ecoprovinces (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_ecoprv_shp %>%
            filter(name == input$ecoprov_area)
        }

      } else if (input$major_area == "Ecoregions") {
        if (input$ecorgn_area == "Ecoregions (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_ecorgn_shp %>%
            filter(CRGNNM == input$ecorgn_area)
        }

      } else if (input$major_area == "Ecosections") {
        if (input$ecosec_area == "Ecosections (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_ecosec_shp %>%
            filter(ECOSEC_NM == input$ecosec_area)
        }

      } else if (input$major_area == "Major watersheds") {
        if (input$wtrshd_area == "Major watersheds (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_wtrshd_shp %>%
            filter(MJR_WTRSHM == input$wtrshd_area)
        }

      } else if (input$major_area == "FWA watersheds") {
        if (input$fwa_area == "FWA watersheds (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_fwa_shp %>%
            filter(WATERSHE_2 == input$fwa_area)
        }

      } else if (input$major_area == "FLP boundaries") {
        if (input$flp_area == "FLP boundaries (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_flp_shp %>%
            filter(flp_unit_nam == input$flp_area)
        }
      } else if (input$major_area == "Municipalities") {
        if (input$muni_area == "Municipalities (select one)") {
          sel_area_shpfl <- bc_shp
        } else {
          sel_area_shpfl <- bc_muni_shp %>%
            filter(ABRVN == input$muni_area)
        }
      }

      sel_area_shpfl
    })

    # Region name (interactive)
    get_region <- reactive({
      region <- NULL

      if (input$major_area == "BC") {
        region <- "BC"
      } else if (input$major_area == "Western North America") {
        region <- "Western North America"
      } else if (input$major_area == "Ecoprovinces") {
        region <- input$ecoprov_area
      } else if (input$major_area == "Ecoregions") {
        region <- input$ecorgn_area
      } else if (input$major_area == "Ecosections") {
        region <- input$ecosec_area
      } else if (input$major_area == "Municipalities") {
        region <- input$muni_area
      } else if (input$major_area == "Major watersheds") {
        region <- input$wtrshd_area
      } else if (input$major_area == "FWA watersheds") {
        region <- input$fwa_area
      } else if (input$major_area == "FLP boundaries") {
        region <- input$flp_area
      }

      region
    })

  ## Filter : variable ---------------------------------

  output$par_picker <- renderUI({
    par_choices <- parameters
    par_choices <-
      list(
        "Minimum Temperature" = 'tmin',
        "Maximum Temperature" = 'tmax',
        "Mean Temperature" = 'tmean',
        "Precipitation" = 'prcp',
        "Vapor pressure deficit (vpd)" = 'vpd',
        "Relative Humidity (RH)" = 'rh',
        "Soil moisture (0-1m)" = 'soil_moisture'
      )
    pickerInput(
      "par_picker",
      "Select climate variable",
      choices = par_choices,
      selected = "tmean"
    )
  })

  ## Filter : time  --------------------------------------
  output$month_picker <- renderUI({
    mon_choices <- months_nam
    mon_choices <- list(
      "Annual" = 'annual',
      "Summer" = 'summer',
      "Fall" = 'fall',
      "Winter" = 'winter',
      "Spring" = 'spring',
      "January" = 'Jan',
      "February" = 'Feb',
      "March" = 'Mar',
      "April" = 'Apr',
      "May" = 'May',
      "June" = 'Jun',
      "July" = 'Jul',
      "August" = 'Aug',
      "September" = 'Sep',
      "October" = 'Oct',
      "November" = 'Nov',
      "December" = 'Dec'
    )
    pickerInput(
      "month_picker",
      "Select month or season or annual"
      ,
      choices = mon_choices,
      selected = "annual"
    )
  })

  #interactive years choices
  whichInput <- reactiveValues(type = "range")
  observeEvent(input$rng_years_choose, {
    showElement("year_range")
    hideElement("year_specific")
    whichInput$type <- "range"
  })

  observeEvent(input$ab_years_choose, {
    showElement("year_specific")
    hideElement("year_range")
    whichInput$type <- "specific"
  })


  # Get values (variables name and unit )-------------------------
  ### Years
  get_years <- reactive({
    if (whichInput$type == "specific") {
      sel_yrs <- input$year_specific
    } else{
      sel_yrs <- seq(input$year_range[1], input$year_range[2], 1)
    }
  })


  # Variables ( parameters) full name (interactive)
  get_par_full <-  reactive({
    req(input$par_picker)
    if (input$par_picker == 'tmin') {
      parr_full = "minimum temperature"
    } else if (input$par_picker == 'tmax') {
      parr_full = "maximum temperature"
    } else if (input$par_picker == 'tmean') {
      parr_full = "mean temperature"
    } else if (input$par_picker == 'prcp') {
      parr_full = "total precipitation"
    }else if (input$par_picker == 'rh') {
      parr_full = "relative humidity (RH)"
    }else if (input$par_picker == 'vpd') {
      parr_full = "vapor pressure deficit (VPD)"
    }else if (input$par_picker == 'soil_moisture') {
      parr_full = "volumetric soil moisture (0-1m)"
    }
    parr_full
  })

  ## Units ( interactive)
  get_unit <- reactive({
    req(input$par_picker)
    if (input$par_picker == "tmax" | input$par_picker == "tmin" | input$par_picker == "tmean") {
      unt <- "°C"
    } else if (input$par_picker == "prcp") {
      unt <- "mm"
    } else if (input$par_picker == "rh") {
      unt <- "%"
    } else if (input$par_picker == "vpd") {
      unt <- "kPa"
    } else if (input$par_picker == "soil_moisture") {
      unt <- "m\U00B3/m"
    }else {
      unt <- " "
    }
    unt
  })

# Months/Seasons full name ( interactive)
  get_mon_full <- reactive({
    req(input$month_picker)

    month_lookup <- c(
      annual = "Annual",
      spring = "Spring",
      summer = "Summer",
      fall = "Fall",
      winter = "Winter",
      Jan = "January", Feb = "February", Mar = "March", Apr = "April",
      May = "May", Jun = "June", Jul = "July", Aug = "August",
      Sep = "September", Oct = "October", Nov = "November", Dec = "December"
    )

    mon_full <- month_lookup[[input$month_picker]]

    if (is.null(mon_full)) mon_full <- "Unknown"
    mon_full
  })

  # Reset  selection /filters -----
  observeEvent(input$reset_input, {
    shinyjs::reset("selection-panel")

  })

  # Location map plot -------------------------------------------
  output$loc_map <- renderLeaflet({
    req(
      input$ecoprov_area,
      input$wtrshd_area,
      input$major_area,
      input$fwa_area,
      input$flp_area
    )

    # Default shape
    sel_area_shpfl <- get_shapefile()
    lyr_id <- NULL

    # Select appropriate shapefile based on inputs
    if (input$major_area == "Major watersheds" &&
        input$wtrshd_area == "Major watersheds (select one)") {
      sel_area_shpfl <- bc_wtrshd_shp['MJR_WTRSHM']
      lyr_id <- "MJR_WTRSHM"

    } else if (input$major_area == "Ecoprovinces" &&
               input$ecoprov_area == "Ecoprovinces (select one)") {
      sel_area_shpfl <- bc_ecoprv_shp['name']
      lyr_id <- "name"

    } else if (input$major_area == "Ecoregions" &&
               input$ecorgn_area == "Ecoregions (select one)") {
      sel_area_shpfl <- bc_ecorgn_shp['CRGNNM']
      lyr_id <- "CRGNNM"

    } else if (input$major_area == "Ecosections" &&
               input$ecosec_area == "Ecosections (select one)") {
      sel_area_shpfl <- bc_ecosec_shp['ECOSEC_NM']
      lyr_id <- "ECOSEC_NM"

    } else if (input$major_area == "Municipalities" &&
               input$muni_area == "Municipalities (select one)") {
      sel_area_shpfl <- bc_muni_shp['ABRVN']
      lyr_id <- "ABRVN"

    } else if (input$major_area == "FLP boundaries" &&
               input$flp_area == "FLP boundaries (select one)") {
      sel_area_shpfl <- bc_flp_shp['flp_unit_nam']
      lyr_id <- "flp_unit_nam"
    }

    # Render leaflet map
    leaflet(sel_area_shpfl) %>%
      addTiles() %>%
      addPolygons(
        layerId = if (!is.null(lyr_id)) as.formula(paste0("~", lyr_id)) else NULL,
        popup = if (!is.null(lyr_id)) as.formula(paste0("~", lyr_id)) else NULL,
        color = "Red",
        weight = 1,
        opacity = 1,
        fill = TRUE,
        fillOpacity = 0
      )
  })

  observeEvent(input$loc_map_shape_click, {
    nm <- input$loc_map_shape_click$id
    print(nm)

    switch(input$major_area,
           "Ecoprovinces"   = updatePickerInput(session, "ecoprov_area", selected = nm),
           "Ecoregions"     = updateSelectInput(session, "ecorgn_area", selected = nm),
           "Ecosections"    = updateSelectInput(session, "ecosec_area", selected = nm),
           "Major watersheds" = updateSelectInput(session, "wtrshd_area", selected = nm),
           "FLP boundaries" = updateSelectInput(session, "flp_area", selected = nm),
           "Municipalities" = updateSelectInput(session, "muni_area", selected = nm)
           # "FWA watersheds" = updateSelectInput(session, "fwa_area", selected = nm)
    )
  })

  #  Selected data for calculations and plotting -----------------
 ano_clm_trn_sel_dt_rct <- eventReactive(input$run_ana_button, {
   req(input$month_picker)
   req(input$par_picker)
   req(input$major_area)

   ### For sample run ----
   # monn = "Aug"
   # parr = "tmean"
   # sel_yrs <- seq(1951,2025,1)
   # sel_yrs
   # sel_area_shpfl <- wna_shp
   # sel_area_shpfl
   # region = "WNA"
   # ano_clm_trn_dt_fl %>%
   #   filter(mon == monn &
   #            par == parr) -> ano_clm_trn_dt_fl_mon
   # ano_clm_trn_dt_fl_mon
   # ano_dt_sel_rast <- rast(ano_clm_trn_dt_fl_mon$dt_pth)
   # ano_dt_sel_rast
   # terra::plot(ano_dt_sel_rast,70:nlyr(ano_dt_sel_rast))

   ano_clm_trn_dt_fl %>%
     filter(mon == input$month_picker &
              par == input$par_picker) -> ano_clm_trn_dt_fl_mon

   # Clip by shapefile of the selected area
   sel_area_shpfl <- get_shapefile()

   # other requirements
   monn = unique(ano_clm_trn_dt_fl_mon$mon)
   parr = unique(ano_clm_trn_dt_fl_mon$par)


   # Anomaly
   ano_clm_trn_dt_fl_mon %>%
     filter(dt_type =='ano') -> ano_dt_fl_mon

   ano_dt_sel_rast <- rast(ano_dt_fl_mon$dt_pth)
   ano_dt_sel_rast
   # plot(ano_dt_sel_rast)
   yr_df <- tibble(paryr = names(ano_dt_sel_rast))
   yr_df %<>%
     mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
   names(ano_dt_sel_rast) <- yr_df$yr
   terra::time(ano_dt_sel_rast) <- yr_df$yr

   #crop for selected area
   ano_dt_shp_rast <-
     terra::crop(ano_dt_sel_rast, sel_area_shpfl, snap="out",mask = T)
   ano_dt_shp_rast

   # Climatology
   ano_clm_trn_dt_fl_mon %>%
     filter(dt_type =='clm') -> clm_dt_fl_mon

   clm_dt_sel_rast <- rast(clm_dt_fl_mon$dt_pth)
   clm_dt_sel_rast

   #crop for selected area
   clm_dt_shp_rast <-
     terra::crop(clm_dt_sel_rast, sel_area_shpfl, snap="out",mask = T)
   clm_dt_shp_rast

   #calculate percentage for prcp and soil-moisture
   if (parr == 'prcp' | parr == 'soil_moisture' ) {
     ano_dt_shp_rast1 <- (ano_dt_shp_rast / clm_dt_shp_rast) * 100
     #If prcp anomalies are very high ( > 200 %) then convert and limit to 200.
     ano_dt_shp_rast2 <-
       ifel(ano_dt_shp_rast1 > 201, 200, ano_dt_shp_rast1)
     ano_dt_shp_rast3 <-
       ifel(ano_dt_shp_rast2 < -201, -200, ano_dt_shp_rast2)
     ano_dt_shp_rast <- ano_dt_shp_rast3
   } else{
     ano_dt_shp_rast <- ano_dt_shp_rast
   }
   # plot(aano_dt_shp_rast,40:44)
   ano_dt_shp_rast

   # Spatial trends
   # trends50
   ano_clm_trn_dt_fl_mon %>%
     filter(dt_type =='trend' & start_year == '1950') -> trend_dt_fl_mon50

   trn_dt_sel_rast50 <- rast(trend_dt_fl_mon50$dt_pth)
   trn_dt_sel_rast50
   # plot(trn_dt_sel_rast50)

   #crop for selected area
   trn_dt_shp_rast50 <-
     terra::crop(trn_dt_sel_rast50, sel_area_shpfl, snap="out",mask = T)
   trn_dt_shp_rast50

   # trends80
   ano_clm_trn_dt_fl_mon %>%
     filter(dt_type =='trend' & start_year == '1980') -> trend_dt_fl_mon80

   trn_dt_sel_rast80 <- rast(trend_dt_fl_mon80$dt_pth)
   trn_dt_sel_rast80
   # plot(trn_dt_sel_rast80)

   #crop for selected area
   trn_dt_shp_rast80 <-
     terra::crop(trn_dt_sel_rast80, sel_area_shpfl, snap="out",mask = T)
   trn_dt_shp_rast80


   # Final return list
   result_lst <-  return(list(
     fltr_ano_dt = ano_dt_shp_rast,
     fltr_clm_dt =  clm_dt_shp_rast,
     fltr_trn50_dt =  trn_dt_shp_rast50,
     fltr_trn80_dt =  trn_dt_shp_rast80,
     fltr_mtdt_fl = ano_clm_trn_dt_fl_mon
   ))

   return(result_lst)

 })

  # Time-series and linear trend -------------------------
  time_series_trnd_rct <- eventReactive(input$run_ana_button,{

    withProgress(message = 'Calculating linear trends', value = 0, {
      incProgress(0.02, detail = "Filtering data...")
      ## time series data generate -----------
      # Filtered reactive data
      ano_clm_trn_sel_dt_rct()[[1]] -> ano_dt_shp_rast

      ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt

      # sel_dt_mtdt <- ano_clm_trn_dt_fl_mon

      # Shapefile spatial average anomalies by year
      ano_shp_av_dt <-
        tibble(rownames_to_column(global(
          ano_dt_shp_rast, fun = "mean", na.rm = T
        ), "yr")) %>%
        dplyr::select(yr, ano = mean)

      ano_shp_av_dt$ano <- round(ano_shp_av_dt$ano, digits=4)

      ano_shp_av_dt %<>%
        drop_na()
      ano_shp_av_dt$yr <-
        as.numeric(str_extract(ano_shp_av_dt$yr, "[0-9]+"))
      ano_shp_av_dt$par <- unique(sel_dt_mtdt$par)
      ano_shp_av_dt$mon <- unique(sel_dt_mtdt$mon)
      ano_shp_av_dt$region <- get_region()
      ano_shp_av_dt

      # To download time series
      ano_shp_av_dt %>%
        dplyr::select(yr,ano,par,mon,region) -> av_ano_ts

      ## Trend calculation and plot ------------

      # Background requirements for plots
      parr <- unique(ano_shp_av_dt$par)
      monn <- unique(ano_shp_av_dt$mon)
      region <- unique(ano_shp_av_dt$region)

      # Trend on average anomaly 1950 - now
      ano_shp_av_dt %<>%
        filter(yr > 1950) %<>%
        mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
          # incpt =zyp.trend.vector(ano)[["intercept"]],
          #sig = zyp.trend.vector(ano)[["sig"]])
          sig = round(MannKendall(ano)[[2]], digits = 4))
      ano_shp_av_dt

      ano_mk_trnd <-
        zyp.sen(ano ~ yr, ano_shp_av_dt)##Give the trend###
      ano_mk_trnd$coefficients
      ano_shp_av_dt$trn <-  ano_mk_trnd$coeff[[2]]
      ano_shp_av_dt$incpt <-  ano_mk_trnd$coeff[[1]]

      xs = c(min(ano_shp_av_dt$yr), max(ano_shp_av_dt$yr))
      trn_slp = c(unique(ano_shp_av_dt$incpt), unique(ano_shp_av_dt$trn))
      ys = cbind(1, xs) %*% trn_slp
      ano_shp_av_dt$trn_lab = paste(
        "italic(1950-~trend)==",
        round(ano_shp_av_dt$trn, 2),"~yr^{-1}~','~italic(p)==",
        round(ano_shp_av_dt$sig, 2)
      )

      #     mag_trnd_lab=paste("italic(t)==",round(ano_shp_av_dt$trn,2),get_unit(),
      #                        "~mm~yr^{-1}~','~italic(p)==",round(ano_shp_av_dt$sig,2))

      # Trend on average anomaly 1980 - now
      ano_shp_av_dt %>%
        filter(yr > 1979) %>%
        mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
          # incpt =zyp.trend.vector(ano)[["intercept"]],
          #sig = zyp.trend.vector(ano)[["sig"]])
          sig = round(MannKendall(ano)[[2]], digits = 2)) -> ano_shp_av_dt80
      ano_shp_av_dt80

      ano_mk_trnd80 <-
        zyp.sen(ano ~ yr, ano_shp_av_dt80)##Give the trend###
      ano_mk_trnd80$coefficients
      ano_shp_av_dt80$trn <-  ano_mk_trnd80$coeff[[2]]
      ano_shp_av_dt80$incpt <-  ano_mk_trnd80$coeff[[1]]

      xs80 = c(min(ano_shp_av_dt80$yr), max(ano_shp_av_dt80$yr))
      trn_slp80 = c(unique(ano_shp_av_dt80$incpt), unique(ano_shp_av_dt80$trn))
      ys80 = cbind(1, xs80) %*% trn_slp80
      ano_shp_av_dt80$trn_lab = paste(
        "italic(1980-~trend)==",
        round(ano_shp_av_dt80$trn, 2),"~yr^{-1}~','~italic(p)==",
        round(ano_shp_av_dt80$sig, 2)
      )

      incProgress(0.02, detail = "Plotting linear trend ...")

      # anomaly plot
      ymin <- (-1) * (max(abs(ano_shp_av_dt$ano)))
      ymax <- (1) * (max(abs(ano_shp_av_dt$ano)))
      minyr <- min(ano_shp_av_dt$yr)
      maxyr <- max(ano_shp_av_dt$yr)

      if(ymax < 1){
        ybrk_neg <-
          round(c(seq((-1) * (max(
            abs(ano_shp_av_dt$ano)
          )), 0, length.out = 2)), digits=2)
        ybrk_neg
        ybrk_pos <-
          round(c(seq(0, (1) * (max(
            abs(ano_shp_av_dt$ano)
          )), length.out = 2))[-1], digits=2)
        ybrk_pos
      } else {
        ybrk_neg <-
          ceiling(c(seq((-1) * (max(
            abs(ano_shp_av_dt$ano)
          )), 0, length.out = 4)))
        ybrk_neg
        ybrk_pos <-
          floor(c(seq(0, (1) * (max(
            abs(ano_shp_av_dt$ano)
          )), length.out = 4)))[-1]
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

      if(ymax < 1){
        ybrks_seq <- c(ybrk_neg, ybrk_pos)
      }else {
        ybrks_seq <- c(ybrk_negn, ybrk_posp)
      }
      ybrks_seq
      # Positive and negative anomalies and 3 years moving average to create bar plot
      ano_shp_av_dt %<>%
        mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
        mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
      ano_shp_av_dt
      tail(ano_shp_av_dt)

      if (parr == "prcp" |parr == "soil_moisture") {
        par_title <-  paste0(get_region(), " ",
                             get_par_full(), " ", "anomaly", " (% of normal)",
                             " : ",
                             get_mon_full())
      } else{
        par_title <-  paste0(get_region(), " ",
                             get_par_full(), " ", "anomaly"," (", get_unit(),")",
                             " : ",
                             get_mon_full())
      }

      if (parr == "prcp" |parr == "soil_moisture") {
        y_axis_lab <- paste0(parr, " average anomaly (% of normal)")
      } else{
        y_axis_lab <- paste0(parr, " average anomaly ", "(", get_unit(), ")")
      }

      ano_shp_trn_plt <-
        ggplot(data = ano_shp_av_dt, aes(x = yr, y = ano)) +
        annotate(
          geom = 'text',
          label = plt_wtrmrk,
          x = Inf,
          y = -Inf,
          hjust = 1,
          vjust = -0.5,
          color = 'gray80',
          size = 3.0
        ) +
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
          name = paste0(parr, " anomaly ", "get_unit()"),
          colours = cpt(pal = "ncl_BlWhRe",
                        n = 100,
                        rev = F),
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
          label = ano_shp_av_dt$trn_lab[[1]],
          size = 4.0, parse=T
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
          label = ano_shp_av_dt80$trn_lab[[1]],
          size = 4.0,
          parse = TRUE
        ) +
        scale_x_continuous(
          name = " ",
          breaks = seq(1950, maxyr, 5),
          expand = c(0.02, 0.02)
        ) +
        scale_y_continuous(name = y_axis_lab,
                           limits = c(ymin, ymax),
                           breaks = ybrks_seq) +
        labs(title = par_title, subtitle = "Baseline: 1981-2010") +
        scale_color_manual(
          " ",
          values = c(
            "3-yrs moving mean" = "green",
            "1950-trend" = "black",
            "1980-trend" = "deepskyblue2"
          ),
          labels =  c(
            "3-yrs moving mean" = "3-yrs moving mean",
            "1950-trend" = "1950-trend",
            "1980-trend" = "1980-trend"
          )
        ) +
        theme_bw() +
        theme(
          # panel.spacing=unit(0.1,"lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(
            color = "gray75",
            linewidth = 0.05,
            linetype = "dashed"
          ),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.ticks.length = unit(-0.20, "cm"),
          element_line(colour = "black", linewidth =  1),
          axis.title.y = element_text(
            angle = 90,
            face = "plain",
            size = 13,
            colour = "Black",
            margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "mm")
          ),
          axis.title.x = element_text(
            angle = 0,
            face = "plain",
            size = 13,
            colour = "Black",
            margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "mm")
          ),
          axis.text.x = element_text(
            angle = 0,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            size = 12,
            margin = margin(
              t = 2,
              r = 2,
              b = 2,
              l = 2
            )
          ),
          axis.text.y = element_text(
            angle = 90,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            size = 12,
            margin = margin(
              t = 2,
              r = 2,
              b = 2,
              l = 2
            )
          ),
          plot.title = element_text(
            angle = 0,
            face = "bold",
            size = 13,
            colour = "Black"
          ),
          legend.position = c(0.90, 0.94),
          legend.direction = "vertical",
          legend.background = element_rect(fill = NA, color = NA),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.title = element_text(size = 13),
          legend.text = element_text(margin = margin(t = -5), size = 12),
          strip.text.x = element_text(size = 12, angle = 0),
          strip.text.y = element_text(size = 12, face = "bold"),
          axis.text = element_text(margin = margin(t = -5, r = -5, b = -5, l = -5)),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(colour = 'Black')
        )
      ano_shp_trn_plt

      if (parr == "prcp" | parr == "soil_moisture" |parr == "rh") {
        ano_shp_trn_plt <- ano_shp_trn_plt +
          scale_fill_gradientn(
            name = paste0(parr, "  anomaly ", get_unit()),
            colours = cpt(pal = "cmocean_curl",
                          n = 100,
                          rev = T),
            limits = c(ymin, ymax),
            breaks = ybrks_seq
          )
      }
      ano_shp_trn_plt<- ano_shp_trn_plt +
        theme(axis.title.y = element_blank())
      ano_shp_trn_plt

      # plotly display

      trn1980_lab <-
        paste0('1980-trend = ',
               round(ano_shp_av_dt80$trn[[1]], 2),'yr<sup>-1</sup>','<span>&#44;</span> ',
               ' <i>p<i>=',
               round(ano_shp_av_dt80$sig[[1]], 2) )
      trn1980_lab
      trn1950_lab <-
        paste0('1950-trend = ',
               round(ano_shp_av_dt$trn[[1]], 2),'yr<sup>-1</sup>','<span>&#44;</span> ',
               ' <i>p<i>=',
               round(ano_shp_av_dt$sig[[1]], 2) )
      trn1950_lab

      #Convert to plotly
      ano_shp_trn_plty<-  ggplotly(ano_shp_trn_plt) %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.6,
                             y = 1.0))%>%
        layout(margin = list(l = 0, r = 0, b = 10, t = 80),
               title = list( x = 0.001 ,
                             y = 0.92,
                             text = paste0(par_title,
                                           '<br>',
                                           '<sup>',
                                           'Baseline: 1981-2010', '</sup>')))%>%
        layout(
          annotations = list(
            list(
              x = 1 ,
              y = 0.0,
              text = plt_wtrmrk,
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor='right', yanchor='auto', xshift=0, yshift=0,
              font=list(size=9, color='#e5e5e5')
            )
          ))%>%
        layout(
          annotations = list(
            list(
              x = 0.30 ,
              y = 0.97,
              text = trn1950_lab,
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor='right', yanchor='auto', xshift=0, yshift=0,
              font=list(size=15, color="black")
            )
          ))%>%
        layout(
          annotations = list(
            list(
              x = 0.30 ,
              y = 0.93,
              text = trn1980_lab,
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor='right', yanchor='auto', xshift=0, yshift=0,
              font=list(size=15, color='#00bfff')
            )
          ))%>%
        layout(xaxis = list(showgrid = FALSE),
               yaxis = list(showgrid = FALSE))
      ano_shp_trn_plty

      ### File name for download -----
      # Year range
      if (monn != "annual") {
        mx_yr = max_year
      } else {
        mx_yr = max_year - 1
      }

      fl_nam <-
        paste0(get_region(),
               "_",
               parr,"_anomaly_timeseries",
               "_",
               monn,
               "_",
               min_year,
               "_",
               mx_yr)
      fl_nam
      incProgress(0.05, detail = "Finalizing linear trend ...")
      # Final return list
      return(list(lnr_trn_ptly_plt =  ano_shp_trn_plty,
                  fl_nam_dwnld = fl_nam,
                  lnr_trn_plt_dwnld =  ano_shp_trn_plt,
                  ts_data_csv = av_ano_ts
      ))
    })

  })

  ## display linear trend  ---------------
  output$lnr_trn_plt <- renderPlotly({
    time_series_trnd_rct()[[1]]})

  ## Download linear trend plot and time series data --------
  # Download plot

  output$download_lnr_trn_plt <- downloadHandler(
    filename = function(file) {
      paste0(time_series_trnd_rct()[[2]], "_trend_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = time_series_trnd_rct()[[3]],
        width = 13,
        height = 6,
        units = "in",
        dpi = 300,
        scale = 0.9,
        limitsize = F,
        device = "png"
      )
    }
  )

  # Download time series (.csv)
  output$download_ano_ts_data <- downloadHandler(
    filename = function(file) {
      paste0(time_series_trnd_rct()[[2]],
             "_data.csv")
    },
    content = function(file) {
      write_csv(time_series_trnd_rct()[[4]],
                file, append = FALSE)
    }
  )

  # Spatial anomaly data and plot:  Reactive ----------------------------------------------------------------

  spatial_ano_dt_plt_rct <- eventReactive(input$run_ana_button, {
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    ano_clm_trn_sel_dt_rct()[[1]] -> ano_dt_shp_rast

   ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt
   parr <- unique(sel_dt_mtdt$par)
   monn <- unique(sel_dt_mtdt$mon)

   sel_area_shpfl <- get_shapefile()

    ano_dt_sel_rast <- ano_dt_shp_rast
    names(ano_dt_sel_rast)

    # Filter for selected year (s)
    sel_yrs <- get_years()

    if (length(sel_yrs) > 50) {
      sel_yrs <- sel_yrs[1:50]
      shinyalert(html = T,
                 text = tagList(h3(
                   "Too many years selected, maximum 50 allowed."
                 )),
                 showCancelButton = T)
    }

    yr_df <- tibble(paryr = names(ano_dt_sel_rast))
    yr_df %<>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    names(ano_dt_sel_rast) <- yr_df$yr

    ano_dt_rast  <-  subset(ano_dt_sel_rast, which(names(ano_dt_sel_rast) %in% sel_yrs))
    ano_dt_rast
    names(ano_dt_sel_rast)

    ## Spatial anomaly overview summary  --------

    # Year range
    minyr <- min(sel_yrs)
    maxyr <- max(sel_yrs)

    mn_ano <-
      terra::global(ano_dt_rast, fun = "mean", na.rm = T)
    mn_ano
    mn_ano <- round(mean(mn_ano$mean, na.rm = T), 2)
    mi_ano <- terra::global(ano_dt_rast, fun = "min", na.rm = T)
    mi_ano <- round(min(mi_ano$min, na.rm = T), 2)
    mx_ano <- terra::global(ano_dt_rast, fun = "max", na.rm = T)
    mx_ano <- round(max(mx_ano$max, na.rm = T), 2)

    # Combine for a display table
    if (parr == 'prcp' | parr == 'soil_moisture') {
      mi_ano_val = paste0(mi_ano)
      mn_ano_val = paste0(mn_ano, " % of normal")
      mx_ano_val = paste0(mx_ano)
    } else {
      mi_ano_val = paste0(mi_ano)
      mn_ano_val = paste0(mn_ano, get_unit())
      mx_ano_val = paste0(mx_ano)
    }

    # Create a table
    ano_ovr_dt <-
      data.frame(
        "Anomaly" = c("Minimum", "Mean", "Maximum"),
        "Value" = c(mi_ano_val, mn_ano_val, mx_ano_val)
      )
    ano_ovr_dt

    ## Spatial anomaly plot ----------
    ano_rng_lmt <- terra::minmax(ano_dt_rast, compute = T)
    minval <- (-1) * (max(abs(ano_rng_lmt), na.rm = T))
    maxval <- (1) * (max(abs(ano_rng_lmt), na.rm = T))

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
    if (parr == "prcp" |parr == "soil_moisture") {
      par_title <-  paste0(get_region(), " ",
                           get_par_full(), " anomaly (% of normal)",
                           ": ",
                           get_mon_full())
    } else {
      par_title <-  paste0(get_region(), " ",
                           get_par_full(), " anomaly (", get_unit(),")",
                           ": ",
                           get_mon_full())
    }

    xlim <- c(-140,-113.0)
    ylim <- c(45,61)

    ### plot to display ----

    spatial_ano_plt <-  ggplot() +
      geom_spatraster(data = ano_dt_rast) +
      scale_fill_gradientn(
        name = paste0(parr, " anomaly ", get_unit()),
        colours = cpt(pal = "ncl_BlWhRe",
                      n = 100,
                      rev = F),
        na.value = "transparent",
        limits = c(minval, maxval),
        breaks = brks_seq
      ) +
      facet_wrap(. ~ lyr) +
      geom_sf(
        data = sel_area_shpfl,
        colour = "black",
        size = 1,
        fill = NA,
        alpha = 0.8
      ) +
      # coord_sf(xlim = xlim, ylim = ylim)+
      scale_x_continuous(
        name =  "Longitude (°W) ",
        breaks = seq(xmi - 5, xmx + 5, 10),
        labels = abs,
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        name = "Latitude (°N) ",
        # breaks = seq((ymi - 1), (ymx + 1), 6),
        # labels = abs,
        expand = c(0.01, 0.01)
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
          margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
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
        legend.position = 'right',
        legend.direction = "vertical",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        legend.title = element_text(size = 15),
        legend.text = element_text(margin = margin(t = -5), size = 16),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = margin(t = -5, r = -5, b = -5, l = -5)),
        strip.background = element_rect(color = "black", fill = "gray90"),
        strip.text = element_text(
          face = "bold",
          size = 18,
          colour = 'black'
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

    if (parr == "prcp" & maxval > 200 |parr == "soil_moisture" & maxval > 200 |parr == "rh" & maxval > 200 ) {
      spatial_ano_plt <- spatial_ano_plt +
        scale_fill_gradientn(
          name = paste0(parr, " anomaly ", get_unit()),
          colours = cpt(pal = "cmocean_curl",
                        n = 100,
                        rev = T),
          na.value = "transparent",
          limits = c(minval, maxval),
          breaks = brks_seq,
          labels = labels_val
        )
    } else if (parr == "prcp" |parr == "soil_moisture" |parr == "rh" ) {
      spatial_ano_plt <- spatial_ano_plt +
        scale_fill_gradientn(
          name = paste0(parr, "  anomaly (%) "),
          colours = cpt(pal = "cmocean_curl",
                        n = 100,
                        rev = T),
          na.value = "transparent",
          limits = c(minval, maxval),
          breaks = brks_seq
        )
    }

    spatial_ano_plt <- spatial_ano_plt +
      labs(
        tag = plt_wtrmrk,
        title = par_title,
        subtitle = paste0(
          "Baseline: 1981-2010. ",
          '[',get_region(),  ' anomaly over ',
          minyr,
          '-',
          maxyr,
          ': Mean = ',
          ano_ovr_dt[2, 2],
          ' ,',
          ' Range = ',ano_ovr_dt[1, 2], ' - ',
          ano_ovr_dt[3, 2],
          ']'
        )
      ) +
      theme(
        plot.tag.position = "bottom",
        plot.tag = element_text(
          color = 'gray50',
          hjust = 1,
          vjust = 0,
          size = 8
        )
      )
    spatial_ano_plt


    ### File name for download ------------
    fl_nam <-
      paste0(get_region(),
             "_",
             parr,"_anomaly",
             "_",
             monn,
             "_",
             input$year_range[1],
             "_",
             input$year_range[2])
    fl_nam

    ## final reactive output list  -------------------

    return(list(
      sptl_ano_data =  ano_dt_rast,
      sptl_ano_plt = spatial_ano_plt,
      download_fl_nam = fl_nam
    ))

  })

  ### Spatial anomaly map display ---------------------
  output$sptl_ano_map <- renderPlot({
    spatial_ano_dt_plt_rct()[[2]]
  })

   ### Spatial anomaly map and data download ------------------
   # Spatial anomaly plot download/save
  output$download_sptl_ano_plt <- downloadHandler(
    filename = function(file) {
      paste0(spatial_ano_dt_plt_rct()[[3]], "_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = spatial_ano_dt_plt_rct()[[2]],
        width = 11,
        height = 10,
        units = "in",
        dpi = 300,
        scale = 1.0,
        limitsize = F,
        device = "png"
      )
    }
  )

  # Spatial anomaly data download as raster (tif )
  output$download_sptl_ano_data <- downloadHandler(
    filename = function(file) {
      paste0(spatial_ano_dt_plt_rct()[[3]], "_data.tif")
    },
    content = function(file) {
      writeRaster(spatial_ano_dt_plt_rct()[[1]],
                  file,
                  filetype = "GTiff",
                  overwrite = TRUE)
    }
  )

 # Climate normal plot ----------------------------------------------------------------------------
  clm_nor_plt_rct <- eventReactive(input$run_ana_button,{

    ano_clm_trn_sel_dt_rct()[[2]] -> clm_dt_shp_rast

    ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt
    parr <- unique(sel_dt_mtdt$par)
    monn <- unique(sel_dt_mtdt$mon)

    sel_area_shpfl <- get_shapefile()

  ## Climate normal plot title -----
    if (parr == "prcp") {
      clm_nor_title_txt <-
        # Climate plot title ( use log for prcp)
        paste0(get_region(), " mean ",
               get_par_full()," (average of  1981-2010)","(", get_unit(),")" ," (log-scale)",
               "  : ",
               get_mon_full())
    } else{
      clm_nor_title_txt <-  paste0(get_region(), " ",
                                   get_par_full(), " (average of  1981-2010) ", "(", get_unit(),")" ,
                                   " : ",
                                   get_mon_full())
    }
    clm_nor_title_txt


 ## Climate normal plot for display -------

        # Calculate mean and range of normal values
    mn_clm_val <-
      round(global(clm_dt_shp_rast, 'mean', na.rm = T), digits = 2)
    mi_clm_val <-
      round(global(clm_dt_shp_rast, 'min', na.rm = T), digits = 2)
    mx_clm_val <-
      round(global(clm_dt_shp_rast, 'max', na.rm = T), digits = 2)

    # Plot using terra rast

    if (parr == "prcp") {
      clm_dt_shp_rast1 <- log(clm_dt_shp_rast)
    } else{
      clm_dt_shp_rast1 <- clm_dt_shp_rast
    }

    spatial_clm_plt <-  ggplot() +
      geom_spatraster(data = clm_dt_shp_rast1) +
      scale_fill_continuous(
        type = "viridis",
        name = " ",
        option = "inferno",
        direction = -1,
        na.value = "transparent"
      ) +
      geom_sf(
        data = sel_area_shpfl,
        colour = "black",
        size = 1,
        fill = NA,
        alpha = 0.8
      ) +
      scale_x_continuous(
        name =  "Longitude (°W) ",
        # breaks = seq(xmi - 5, xmx + 5, 10),
        labels = abs,
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        name = "Latitude (°N) ",
        # breaks = seq(ymi - 1, ymx + 1, 6),
        labels = abs,
        expand = c(0.01, 0.01)
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
          margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
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
          size = 15,
          colour = "Black"
        ),
        legend.position = 'right',
        legend.direction = "vertical",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        legend.title = element_text(size = 15),
        legend.text = element_text(margin = margin(t = -5), size = 16),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = margin(t = -5, r = -5, b = -5, l = -5)),
        strip.background = element_rect(color = "black", fill = "gray90"),
        strip.text = element_text(
          face = "bold",
          size = 18,
          colour = 'black'
        )
      ) +
      guides(
        fill = guide_colorbar(
          barwidth = 1.0,
          barheight = 10,
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
    spatial_clm_plt

    if (parr == "prcp" |parr == "soil_moisture" |parr == "rh") {
      spatial_clm_plt <- spatial_clm_plt +
        scale_fill_continuous(
          type = "viridis",
          name = " ",
          option = "viridis",
          direction = -1,
          na.value = "transparent"
        )
    }

  spatial_clm_plt <- spatial_clm_plt +
      labs(tag = plt_wtrmrk) +
      theme(
        plot.tag.position = "bottom",
        plot.tag = element_text(
          color = 'gray50',
          hjust = 1,
          size = 6
        )
      ) +
      labs(
        # title = par_title,
        subtitle = paste0(
          'Mean = ',
          mn_clm_val[[1]]," ",
          "(", get_unit(),")" ,
          "  ",
          "Range = ",
          "[",
          mi_clm_val[[1]],
          " - ",
          mx_clm_val[[1]],
          "]"
        )
      ) +
      theme(
        plot.title = element_text(size = 12, face = 'plain'),
        plot.subtitle = element_text(size = 10)
      )
    spatial_clm_plt

 ## Climate normal data and plot download ---------
  fl_nam <-
    paste0(get_region(),
           "_",
       get_par_full(),"_climate_normal_1981_2010",
         "_",
           get_mon_full())
  fl_nam

  # Plot with title for download

      # Climate plot title ( use log for prcp)
      if (parr == "prcp") {
        par_title <-  paste0(get_region()," ",
                             get_par_full(),"","(", get_unit(),")" ," (log-scale)",
                             " : ",
                             get_mon_full(), " (average 1981-2010)")
      } else{
        par_title <-  paste0(get_region(), " ",
                             get_par_full(), " ", "(", get_unit(),")" ,
                             " : ",
                             get_mon_full(), " (average 1981-2010)")
      }

      spatial_clm_plt_dnwld <-  spatial_clm_plt+
        labs( title = par_title)
      spatial_clm_plt_dnwld

  ### Final reactive output list --------------

      return(list(
        clm_nor_title_txt = clm_nor_title_txt,
        clm_nor_plt = spatial_clm_plt,
        clm_nor_plt_dnwld = spatial_clm_plt_dnwld,
        clm_nor_data =  clm_dt_shp_rast1,
        download_fl_nam = fl_nam
      ))

})

  ## Climate normal plot display & download ------------

  # Plot title
  output$clm_nor_title <- renderText({
    clm_nor_plt_rct()[[1]]
  })

  # plot display
  output$clm_nor_map <- renderPlot({
    clm_nor_plt_rct()[[2]]
  })

  # climate normal plot download/save
    output$download_clm_nor_plt <- downloadHandler(
      filename = function(file) {
        paste0(clm_nor_plt_rct()[[5]], "_plot.png")
      },
      content = function(file) {
        ggsave(
          file,
          plot =  clm_nor_plt_rct()[[3]],
          width = 11,
          height = 9,
          units = "in",
          dpi = 300,
          scale = 0.9,
          limitsize = F,
          device = "png"
        )
      }
    )

    # Climate normal data save in tiff
    output$download_clm_nor_data <- downloadHandler(
      filename = function(file) {
        paste0(clm_nor_plt_rct()[[5]], "_data.tif")
      },
      content = function(file) {
        writeRaster(clm_nor_plt_rct()[[4]],
                    file,
                    filetype = "GTiff",
                    overwrite = TRUE)
      }
    )

    # Spatial anomaly trends for 1950s and 1980s ---------------------------------------------------------

    spatial_ano_trnd_rct <- eventReactive(input$run_ana_button,{

      withProgress(message = 'Calculating spatial trends', value = 0, {
        incProgress(0.1, detail = "Extracting data ...")

        ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt
        parr <- unique(sel_dt_mtdt$par)
        monn <- unique(sel_dt_mtdt$mon)

        sel_area_shpfl <- get_shapefile()

        # 1950s spatial trend ----------
        ano_clm_trn_sel_dt_rct()[[3]] -> ano_trn_mag_sig50
        # ano_trn_mag_sig50 <- trn_dt_shp_rast50

       names(ano_trn_mag_sig50) <- c("trnmag", "pval")
      # plot(ano_trn_mag_sig50
      mn_trn_val50 <-
        round(global(ano_trn_mag_sig50[[1]], 'mean', na.rm = T), digits = 3)
      mi_trn_val50 <-
        round(global(ano_trn_mag_sig50[[1]], 'min', na.rm = T), digits = 3)
      mx_trn_val50 <-
        round(global(ano_trn_mag_sig50[[1]], 'max', na.rm = T), digits = 3)

      # Convert to point data
      ano_sp_mk_trn_sig_dt50 <- as_tibble(ano_trn_mag_sig50, xy = TRUE, na.rm = TRUE) %>%
        mutate(trnmag = round(trnmag, 3))

      if (parr == 'prcp' | parr == 'soil_moisture') {
        trn_unt = '% normal'
      } else {
        trn_unt = get_unit()
      }

      #### Plot trend map (1950-now)
      incProgress(0.1, detail = "Plotting spatial trend (1950-now)...")

      ano_dt_sig_trn50 <- ano_sp_mk_trn_sig_dt50 %>%
        dplyr::filter(pval <= 0.1)
      ano_dt_sig_trn50

      mxtrn50 <- max(abs(ano_sp_mk_trn_sig_dt50$trnmag), na.rm = T)
      mxtrn50

      ano_dt_sp_trn_sig_plt50 <- ggplot() +
        geom_tile(data = ano_sp_mk_trn_sig_dt50, aes(x=x,y=y,fill=trnmag),alpha=1)+
        scale_fill_continuous_diverging(palette="Blue-Red",n_interp=21,
                                        limits=c(-mxtrn50,mxtrn50),
                                        # breaks=seq(-1.2, 1.2,0.3),
                                        # labels=seq(-0.8, 0.8,0.2),
                                        # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
                                        name=bquote(~"trend"~yr^{-1}))+
        geom_point(data=ano_dt_sig_trn50,aes(x=x,y=y),color="Black",fill="Gray10", alpha=0.4,size=0.3, shape =3)+
        geom_sf(
          data = sel_area_shpfl,
          colour = "black",
          size = 1,
          fill = NA,
          alpha = 0.8
        ) +
        scale_x_continuous(
          name =  "Longitude (°W) ",
          # breaks = seq(xmi - 5, xmx + 5, 10),
          labels = abs,
          expand = c(0.01, 0.01)
        ) +
        scale_y_continuous(
          name = "Latitude (°N) ",
          # breaks = seq(ymi - 1, ymx + 1, 6),
          labels = abs,
          expand = c(0.01, 0.01)
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
            margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
          ),
          axis.title.x = element_text(
            angle = 0,
            face = "plain",
            size = 15,
            colour = "Black",
            margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
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
            size = 15,
            colour = "Black"
          ),
          legend.position = 'right',
          legend.direction = "vertical",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
          legend.title = element_text(size = 15),
          legend.text = element_text(margin = margin(t = -5), size = 16),
          strip.text.x = element_text(size = 12, angle = 0),
          strip.text.y = element_text(size = 12, face = "bold"),
          axis.text = element_text(margin = margin(t = -5, r = -5, b = -5, l = -5)),
          strip.background = element_rect(color = "black", fill = "gray90"),
          strip.text = element_text(
            face = "bold",
            size = 18,
            colour = 'black'
          )
        )+
        guides(
          fill = guide_colorbar(
            barwidth = 1.0,
            barheight = 10,
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
      ano_dt_sp_trn_sig_plt50

      if (parr == "prcp" |parr == "soil_moisture" |parr == "rh") {
        ano_dt_sp_trn_sig_plt50 <- ano_dt_sp_trn_sig_plt50 +
          scale_fill_continuous_diverging(palette="green-brown",n_interp=21, rev=T,
                                          limits=c(-mxtrn50,mxtrn50),
                                          # breaks=seq(-1.2, 1.2,0.3),
                                          # labels=seq(-0.8, 0.8,0.2),
                                          # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
                                          name=bquote(~"trend"~yr^{-1}))
      }
      ano_dt_sp_trn_sig_plt50

      ano_dt_sp_trn_sig_plt50 <-  ano_dt_sp_trn_sig_plt50 +
        labs(tag = plt_wtrmrk) +
        theme(
          plot.tag.position = "bottom",
          plot.tag = element_text(
            color = 'gray50',
            hjust = 1,
            size = 6
          )
        ) +
        labs(
          # title = par_title,
          subtitle = paste0(
            'Mean = ',
            mn_trn_val50[[1]]," ",
            "(", trn_unt," yr", "\u207B", "\u00B9)" ,
            "  ",
            "Range = ",
            "[",
            mi_trn_val50[[1]],
            " - ",
            mx_trn_val50[[1]],"]. "
          )
        ) +
        theme(
          plot.title = element_text(size = 12, face = 'plain'),
          plot.subtitle = element_text(size = 10)
        )
      ano_dt_sp_trn_sig_plt50

      ## Spatial trends 1980-now -------------
      incProgress(0.15, detail = "Calculating trend (1980-now)...")

      ano_clm_trn_sel_dt_rct()[[4]] -> ano_trn_mag_sig80
      # ano_trn_mag_sig80 <- trn_dt_shp_rast80

      names(ano_trn_mag_sig80) <- c("trnmag", "pval")

      # plot(ano_trn_mag_sig80
      mn_trn_val80 <-
        round(global(ano_trn_mag_sig80[[1]], 'mean', na.rm = T), digits = 3)
      mi_trn_val80 <-
        round(global(ano_trn_mag_sig80[[1]], 'min', na.rm = T), digits = 3)
      mx_trn_val80 <-
        round(global(ano_trn_mag_sig80[[1]], 'max', na.rm = T), digits = 3)

      # plot (1980-now)
      ano_sp_mk_trn_sig_dt80 <- as_tibble(ano_trn_mag_sig80, xy = TRUE, na.rm = TRUE) %>%
        mutate(trnmag = round(trnmag, 3))

      #### Plot trend maps (1980-now)
      incProgress(0.1, detail = "Plotting spatial trend (1980-now)...")

      ano_dt_sig_trn80 <- ano_sp_mk_trn_sig_dt80 %>%
        dplyr::filter(pval <= 0.1)
      ano_dt_sig_trn80

      mxtrn80 <- max(abs(ano_sp_mk_trn_sig_dt80$trnmag),na.rm = T)
      mxtrn80

      ano_dt_sp_trn_sig_plt80 <-ggplot()+
        geom_tile(data = ano_sp_mk_trn_sig_dt80,aes(x=x,y=y,fill=trnmag),alpha=1)+
        scale_fill_continuous_diverging(palette="Blue-Red",n_interp=21,
                                        limits=c(-mxtrn80,mxtrn80),
                                        # breaks=seq(-1.2, 1.2,0.3),
                                        # labels=seq(-0.8, 0.8,0.2),
                                        # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
                                        name=bquote(~"trend"~yr^{-1}))+
        geom_point(data=ano_dt_sig_trn80,aes(x=x,y=y),color="Black",fill="Gray10", alpha=0.4,size=0.3, shape =3)+
        geom_sf(
          data = sel_area_shpfl,
          colour = "black",
          size = 1,
          fill = NA,
          alpha = 0.8
        ) +
        scale_x_continuous(
          name =  "Longitude (°W) ",
          # breaks = seq(xmi - 5, xmx + 5, 10),
          labels = abs,
          expand = c(0.01, 0.01)
        ) +
        scale_y_continuous(
          name = "Latitude (°N) ",
          # breaks = seq(ymi - 1, ymx + 1, 6),
          labels = abs,
          expand = c(0.01, 0.01)
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
            margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
          ),
          axis.title.x = element_text(
            angle = 0,
            face = "plain",
            size = 15,
            colour = "Black",
            margin = margin(t = -1, r = -1, b = -1, l = -1, unit = "mm")
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
            size = 15,
            colour = "Black"
          ),
          legend.position = 'right',
          legend.direction = "vertical",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
          legend.title = element_text(size = 15),
          legend.text = element_text(margin = margin(t = -5), size = 16),
          strip.text.x = element_text(size = 12, angle = 0),
          strip.text.y = element_text(size = 12, face = "bold"),
          axis.text = element_text(margin = margin(t = -5, r = -5, b = -5, l = -5)),
          strip.background = element_rect(color = "black", fill = "gray90"),
          strip.text = element_text(
            face = "bold",
            size = 18,
            colour = 'black'
          )
        ) +
        guides(
          fill = guide_colorbar(
            barwidth = 1.0,
            barheight = 10,
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
      ano_dt_sp_trn_sig_plt80

      if (parr == "prcp" |parr == "soil_moisture" |parr == "rh") {
        ano_dt_sp_trn_sig_plt80 <- ano_dt_sp_trn_sig_plt80 +
          scale_fill_continuous_diverging(palette="green-brown",n_interp=21, rev=T,
                                          limits=c(-mxtrn80,mxtrn80),
                                          # breaks=seq(-1.2, 1.2,0.3),
                                          # labels=seq(-0.8, 0.8,0.2),
                                          # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
                                          name=bquote(~"trend"~yr^{-1}))
      }
      ano_dt_sp_trn_sig_plt80

      ano_dt_sp_trn_sig_plt80 <-  ano_dt_sp_trn_sig_plt80 +
        labs(tag = plt_wtrmrk) +
        theme(
          plot.tag.position = "bottom",
          plot.tag = element_text(
            color = 'gray80',
            hjust = 1,
            size = 6
          )
        ) +
        labs(
          # title = par_title,
          subtitle = paste0(
            'Mean = ',
            mn_trn_val80[[1]]," ",
            "(", trn_unt," yr", "\u207B", "\u00B9)" ,
            "  ",
            "Range = ",
            "[",
            mi_trn_val80[[1]],
            " - ",
            mx_trn_val80[[1]],

            "]"
          )
        ) +
        theme(
          plot.title = element_text(size = 12, face = 'plain'),
          plot.subtitle = element_text(size = 10)
        )
      ano_dt_sp_trn_sig_plt80

      ### Plots titles --------------
      spl_trn_title_txt50 <-  paste0(get_region(), " ",get_mon_full(), ' ',
                                     get_par_full(), " anomlay trend",
                                     " (", trn_unt," yr", "\u207B", "\u00B9) since 1950: ", get_mon_full(),". Black dots indicate cells with significant trends.")

      spl_trn_title_txt80 <-  paste0(get_region(), " ",get_mon_full(), ' ',
                                     get_par_full(), " anomlay trend",
                                     " (",trn_unt,
                                     " yr", "\u207B", "\u00B9) since 1980: ", get_mon_full(),". Black dots indicate cells with significant trends.")

      ##  For plot and data downloads ---------

      trnd_fl_nam50 <-
        paste0(get_region(),
               "_",
               get_par_full(),"_spatial_trend_1950_present",
               "_",
               get_mon_full())
      trnd_fl_nam50
      trnd_fl_nam80 <-
        paste0(get_region(),
               "_",
               get_par_full(),"_spatial_trend_1980_present",
               "_",
               get_mon_full())
      trnd_fl_nam80

      # Plot with title for download

      par_title50 <-  paste0(get_region(), " ",
                             get_par_full(), " anomaly trend (",trn_unt," yr", "\u207B", "\u00B9): ",get_mon_full(),"1950-present.
                             Black dots indicate cells with significant trends." )
      par_title80 <-  paste0(get_region(), " ",
                             get_par_full(), " anomaly trend (", trn_unt," yr", "\u207B", "\u00B9): ",get_mon_full(),"1980-present.
                             Black dots indicate cells with significant trends." )


      par_title50 <-  paste0(get_region(), " ",
                             get_par_full(), " anomaly trend (", trn_unt," yr", "\u207B", "\u00B9): ",get_mon_full()," 1950-present.
                             Black dots indicate cells with significant trends." )
      par_title80 <-  paste0(get_region(), " ",
                             get_par_full(), " anomaly trend (", trn_unt," yr", "\u207B", "\u00B9): ",get_mon_full()," 1980-present.
                             Black dots indicate cells with significant trends." )

      # Plot download
      ano_dt_sp_trn_sig_plt50_dnwld <-  ano_dt_sp_trn_sig_plt50+
        labs( title = par_title50)
      ano_dt_sp_trn_sig_plt50_dnwld

      ano_dt_sp_trn_sig_plt80_dnwld <-  ano_dt_sp_trn_sig_plt80+
        labs( title = par_title80)
      ano_dt_sp_trn_sig_plt80_dnwld

      incProgress(0.02, detail = "Finalizing spatial trends ...")
      # return plot or data here
      return(list(
        plt_title_1950 = spl_trn_title_txt50,
        trn_plt_1950 =  ano_dt_sp_trn_sig_plt50,
        plt_title_1980 = spl_trn_title_txt80,
        trn_plt_1980 = ano_dt_sp_trn_sig_plt80,

        dnwld_fl_nam50 = trnd_fl_nam50,
        dnwld_trn_plt50 =  ano_dt_sp_trn_sig_plt50_dnwld,
        dnwld_trn_dt50 = ano_trn_mag_sig50,

        download_fl_nam80 = trnd_fl_nam80,
        downalod_trn_plt80 =  ano_dt_sp_trn_sig_plt80_dnwld,
        dnwld_trn_dt80 = ano_trn_mag_sig80
      ))
      })
    })

    ### Display and download trend maps and data ----------
    # Display
    output$clm_trn50_title <- renderText({
      spatial_ano_trnd_rct()[[1]]
    })
    output$clm_trn50_map <- renderPlot({
      spatial_ano_trnd_rct()[[2]]
    })

    output$clm_trn80_title <- renderText({
      spatial_ano_trnd_rct()[[3]]
    })
    output$clm_trn80_map <- renderPlot({
      spatial_ano_trnd_rct()[[4]]
    })

    # Download trend maps and data
    # 1950s plt
    output$download_clm_trn50_plt <- downloadHandler(
      filename = function(file) {
        paste0(spatial_ano_trnd_rct()[[5]], "_plot.png")
      },
      content = function(file) {
        ggsave(
          file,
          plot =   spatial_ano_trnd_rct()[[6]],
          width = 11,
          height = 9,
          units = "in",
          dpi = 300,
          scale = 0.9,
          limitsize = F,
          device = "png"
        )
      }
    )

    # 1950s trend data
    output$download_clm_trn50_data <- downloadHandler(
      filename = function(file) {
        paste0(spatial_ano_trnd_rct()[[5]], "_data.tif")
      },
      content = function(file) {
        writeRaster(spatial_ano_trnd_rct()[[7]],
                    file,
                    filetype = "GTiff",
                    overwrite = TRUE)
      }
    )

    # 1980s plt
    output$download_clm_trn80_plt <- downloadHandler(
      filename = function(file) {
        paste0(spatial_ano_trnd_rct()[[8]], "_plot.png")
      },
      content = function(file) {
        ggsave(
          file,
          plot =   spatial_ano_trnd_rct()[[9]],
          width = 11,
          height = 9,
          units = "in",
          dpi = 300,
          scale = 0.9,
          limitsize = F,
          device = "png"
        )
      }
    )

    # 1980s trend data
    output$download_clm_trn80_data <- downloadHandler(
      filename = function(file) {
        paste0(spatial_ano_trnd_rct()[[8]], "_data.tif")
      },
      content = function(file) {
        writeRaster(spatial_ano_trnd_rct()[[10]],
                    file,
                    filetype = "GTiff",
                    overwrite = TRUE)
      }
    )


  # Feedback text -------
  output$feedback_text <- renderText({
    HTML("<p>We used <a href='https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview' target='_blank'>
ERA5-Land hourly data</a> to calculate the anomalies and climatology.
Anomalies are calculated as the measure of departure from the climatological averages spanning from 1981 to 2010.
Should you have any inquiries or wish to provide feedback, please do not hesitate to use
<a href='https://forms.office.com/r/wN0QYAvSTZ' target='_blank'>this feedback form</a> or write to
<a href='mailto:Aseem.Sharma@gov.bc.ca'><b>Aseem Sharma</b></a>.</p>")


  })

  # Reports --------------------------------------

    ## Years present in reports ----
    report_years <- sort(unique(
      as.numeric(substr(
        report_suffixes[grepl("^[A-Za-z]{3}[0-9]{4}$", report_suffixes)],
        4, 7
      )),
      decreasing = TRUE
    ))

    ## Helper: resolve report filename ----
    get_report_filename <- function(suffix) {

      switch(
        suffix,
        "ann2025"  = "bc_annual_climate_summary_2025.html",
        "ann2024"  = "bc_annual_climate_summary_2024.html",
        "ann2023"  = "bc_annual_climate_summary_2023.html",
        "longterm" = "bc_longterm_temp_prcp_anomaly_report_1980_2022_html.html",
        {

          month_abbr <- toupper(substr(suffix, 1, 3))
          year <- substr(suffix, 4, 7)
          month_num <- match(month_abbr, toupper(month.abb))

          if (!is.na(month_num)) {

            month_full <- format(
              as.Date(paste0(year, "-", month_num, "-01")),
              "%B"
            )

            file1 <- paste0("bc_monthly_climate_summary_", month_full, "_", year, ".html")
            file2 <- paste0(month_full, "_", year, "_bc_mon_sea_ann_climate_summary.html")

            for (f in c(file1, file2)) {
              if (file.exists(file.path("www", f))) return(f)
            }

            file1
          } else {
            paste0("unknown_suffix_", suffix, ".html")
          }
        }
      )
    }

    ## Helper: render a report link ----
    renderReportLink <- function(outputId, label, fileName, type = "monthly") {

      color <- switch(
        type,
        "monthly"  = "#007ACC",
        "annual"   = "#1B7F3B",
        "longterm" = "#8B0000"
      )

      output[[outputId]] <- renderUI({
        tags$div(
          style = "margin-bottom: 6px;",
          tags$a(
            href = fileName,
            target = "_blank",
            style = sprintf(
              "font-size: 16px; font-weight: 600; text-decoration: none; color: %s;",
              color
            ),
            label,
            tags$img(
              src = "html_logo.png",
              height = "18px",
              width = "18px",
              style = "margin-left: 6px; vertical-align: middle;"
            )
          )
        )
      })
    }

    ##  Year-wise report columns ----
    lapply(report_years, function(yr) {

      output[[paste0("reports_year_", yr)]] <- renderUI({

        tagList(

          ## ---- Annual report (TOP) ----
          if (paste0("ann", yr) %in% report_suffixes) {

            output_id <- paste0("doc_ann_", yr)

            renderReportLink(
              output_id,
              paste("Annual", yr),
              get_report_filename(paste0("ann", yr)),
              type = "annual"
            )

            tagList(
              uiOutput(output_id),
              tags$hr()
            )
          },

          ##  Monthly reports ----
          tags$div(
            tags$h5("Monthly"),

            lapply(report_suffixes, function(suffix) {

              if (!grepl("^[A-Za-z]{3}[0-9]{4}$", suffix)) return(NULL)

              year <- substr(suffix, 4, 7)
              if (as.numeric(year) != yr) return(NULL)

              month_abbr <- toupper(substr(suffix, 1, 3))
              month_num <- match(month_abbr, toupper(month.abb))
              if (is.na(month_num)) return(NULL)

              label <- format(
                as.Date(paste0(year, "-", month_num, "-01")),
                "%B %Y"
              )

              output_id <- paste0("doc_", suffix)

              renderReportLink(
                output_id,
                label,
                get_report_filename(suffix),
                type = "monthly"
              )

              uiOutput(output_id)
            })
          ),

          ## Long-term report (ONLY after 2023) ----
          if (yr == 2023 && "longterm" %in% report_suffixes) {

            output_id <- "doc_longterm"

            renderReportLink(
              output_id,
              "Long-term trend (1980–2022)",
              get_report_filename("longterm"),
              type = "longterm"
            )

            tagList(
              tags$hr(),
              uiOutput(output_id)
            )
          }
        )
      })
    })



  ## Climate stripes plots ------------------------------------

  output$bc_clm_strp_withtitle <- renderImage({
    # Render the image
    list(
      src = "www/bc_annual_tmean_ano_stripe_withtitle.png",
      contentType = "image/png",
      width = 1400,
      height = 700 ,
      align ='center'
    )
  })

  # Download stripe plot
  output$clm_strp_plt_ttl_dnwld <- downloadHandler(
    filename = function() {
      "bc_annual_tmean_ano_stripe_withtitle.png"
      },
    content = function(file) {
      # Copy the file from the www folder to the user's download location
      file.copy("www/bc_annual_tmean_ano_stripe_withtitle.png", file)
    }
  )

  output$bc_clm_strp_withouttitle <- renderImage({
    # Render the image
    list(
      src = "www/bc_annual_tmean_ano_stripe.png", # Path to the image file
      contentType = "image/png",
      width = 1400,
      height = 700 ,
      align ='center'
    )
  })

  # Download stripe plot
  output$clm_strp_plt_wttl_dnwld <- downloadHandler(
    filename = function() {
      "bc_annual_tmean_ano_stripe.png"
    },
    content = function(file) {
      # Copy the file from the www folder to the user's download location
      file.copy("www/bc_annual_tmean_ano_stripe.png", file)
    })

  # App deployment date ----
  output$deploymentDate <- renderText({
    paste0("This app was last updated on ",
           readLines("deployment_history.txt"), '.'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
