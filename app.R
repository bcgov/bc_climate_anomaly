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

# Required -------------------
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library("shinythemes")
library("shinyjs")
library('shinyalert')
library('markdown')
library('rmarkdown')
library('sf')
library('terra')
library('leaflet')
library('tidyterra')
library('tidyverse')
library('magrittr')
library('lubridate')
library("DT")
library('zoo')
library('zyp')
library('viridisLite')
library('cptcity')
library('echarts4r')

# Load and process input data -------
## Paths --
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shp_fls_pth <- './shapefiles/'
ano_dt_pth <-  './ano_clm_data/'

## Shape files --------------
# Domain
xmi = -140
xmx = -108
ymi = 39
ymx = 60

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

# BC watersheds
bc_wtrshd_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "bc_watersheds") == T])
# plot(st_geometry(bc_wtrshd_shp))

# Western North America
na_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "north_america") == T])
sf_use_s2(FALSE)
wna_shp <-
  st_crop(
    na_shp,
    xmin = xmi,
    ymin = ymi,
    xmax = xmx,
    ymax = ymx
  )
# plot(st_geometry(wna_shp))

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

update_month <- "August"
update_year <- "2023"

years <- seq(min_year, max_year, 1)
yr_choices <- sort(years, decreasing = T)

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

#  UI ----
ui <- fluidPage(
  navbarPage(
    id = "bc_clm",
    title = "BC Climate Anomaly",
    theme = "bcgov.css",

    ## Intro page ----
    tabPanel(
      title = "Introduction",
      value = "intro",
      column(
        width = 12,
        wellPanel(
          HTML(
            "<h3><b>BC climate anomaly app</b>: monthly, seasonal, and annual climate anomalies and trends </h2>"
          ),
          HTML(
            "<h4>This tool offers spatial visualizations and reports on monthly, seasonal, and annual historical climate changes.
            The climate parameters encompass minimum, maximum, and mean surface air temperatures, and precipitation.
            The app shows climate anomalies in comparison to the average climate conditions across western North America, British Columbia (BC),
            and the ecoprovinces and major watersheds of BC. Additionally, the application
            offers insights into the trends and significance of the spatially averaged anomalies for selected regions and parameters.
            The analysis covers the timeframe from 1951 to 2023, with monthly updates to incorporate the most recent data. We used <a href='https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview'>
            ERA5-Land hourly data</a> for calculating the climatological mean and anomalies. The anomalies are computed as deviations from the average climatic conditions observed between 1981 and 2010 (30-years).
            <br>
            <br>
            This is a product of the <a href ='https://www2.gov.bc.ca/gov/content/environment/natural-resource-stewardship/natural-resources-climate-change/future-forest-ecosystems-centre#:~:text=The%20Future%20Forest%20Ecosystems%20Centre%20(FFEC)%20was%20officially%20established%20in,on%20B.C.%27s%20forest%20ecosystems.'> Future Forest Ecosystems Center (FFEC)</a>.
            The FFEC is a scientific team that forecasts climate change impacts on B.C.’s forest ecosystems.
            The core role of the FFEC is to translate ecological knowledge into data, tools, and guidance to help ecosystem managers account for climate risks.

            <br>
            <br>

            Please navigate to the <b>About</b> page to learn more about the app, data, and methods.
            The <b>Anomaly App</b> page generates the spatial anomaly map and time series plot with trend magnitude and significance of the selected region of interest, parameter, and month/season.
            There are options to download plots and/or data.
            The <b>Report</b> page has a link to full report on BC climate anomalies in HTML format that can be downloaded.
            The <b>Feedback & Links</b> page has a link to a form where users can provide their comments/feedback related to the app.
            Also, this page has links to other similar apps developed and maintained by the FFEC team.
            "
          )
        ),
        column(
          width = 12,
          HTML(
            "<h4><b>Citation</b></h4>
                            <h5> <u>Please cite the contents of this app as:</u>
                            <br>
                            Sharma, A.R. 2023. Monthly, seasonal, and annual climate anomalies of BC: A shiny app.</a>
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
              FFEC, FCCSB, OCF, BC Ministry of Forests<br>
              <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> <br>
              <br>
              <h4><b>Code</b></h4>
              <h5> The code and data of this app are available through GitHub at <a href='https://github.com/bcgov/bc_climate_anomaly.git'> https://github.com/bcgov/bc_climate_anomaly.</a></h5>"
          )
        ),
        column(width = 12,
               textOutput("deploymentDate"),),
        ###### footer ----
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

    ## About page ----
    tabPanel(
      "About",
      includeMarkdown("about_bc_climate_anomaly_app.Rmd"),
      ###### footer ----
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

    ## App page ----
    tabPanel(
      title = "Anomaly App",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "selection-panel",
          # style = "position:fixed; width:24%; max-height: 100vh;",
          width = 3,
          ##### filters -----
          helpText(HTML("<h4><b> Filter/Selections</b> </h4>",)),
          fluidRow(
            useShinyjs(),
            pickerInput(
              "major_area",
              "Select region",
              choices = c("Western North America",
                          "BC", "Ecoregions", "Watersheds")
            ),
            hidden(
              pickerInput(
                "ecoprov_area",
                "Ecoregion",
                choices = c("All_ecoregions", c(bc_ecoprv_shp$name)),
                multiple = F
              )
            ),
            hidden(
              selectInput(
                "wtrshd_area",
                "Watershed",
                choices = c("All_watersheds", c(bc_wtrshd_shp$MJR_WTRSHM)),
                multiple = F
              )
            )
          ),
          br(),
          fluidRow(offset = 3,
                   # div(style = "height:70px;width:100%;background-color: #999999;border-style: dashed;border-color: #000000",)
                   uiOutput("par_picker")),
          br(),
          fluidRow(title = "Month",
                   uiOutput("month_picker")),
          br(),
          fluidRow(
            helpText(HTML("<h5><b> Choose specific year (s) or range of years</b> </h5>",)),
            actionButton("ab_years_choose", "Specific year(s)"),
            actionButton("rng_years_choose", "Range of years"),
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
            # Reset selection
            br(),
            actionButton("reset_input", "Reset")
          ),
          fluidRow(column(
            HTML("<h4><b>Location Map</b> </h4>"),
            title = "Map Location",
            width = 12,
            leafletOutput("loc_map", height = "22vh")
          )),
          br(),
        ),
        mainPanel(
          tags$head(tags$style(HTML(
            '.box {margin: 25px;}'
          ))),
          width = 9,
          ##### Anomaly map plot ------
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b>Spatial anomaly map</b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            box(
              width = 12,
              plotOutput("ano_map", height = "70vh"),
              downloadButton(outputId = "download_ano_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_ano_data",
                             label = "Download raster data"),
            )
          )),
          ###### climate normal map and anomalies overview ----
          fluidRow(
            box(
              width = 6,
              align="left",
              wellPanel(HTML(
                "<h5><b>Climate Normal (1981-2010)</b> </h5>"
              )),
              uiOutput("clm_nor_title", height = "30vh"),
              plotOutput("clm_nor_map", width = "100%", height = "30vh"),
              downloadButton(outputId = "download_clm_nor_plt",
                             label = "Download plot"),
              downloadButton(outputId = "download_clm_nor_data",
                             label = "Download raster data"),
            ),
            box(
              width = 6,
              status = "primary",
              wellPanel(HTML("<h5><b>Anomalies Overview</b> </h5>")),
              uiOutput("anomaly_overview", height = "30vh"),
              br(),
              DTOutput('ano_ovr_tbl')
            )
          ),
          ##### trend plots ----
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b>Time series and trend plot</b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            offset = 0.5,
            tabBox(
              width = 12,
              tabPanel(
                status = 'primary',
                title = "Trend plot",
                plotOutput("spatial_trn_plt", height = "50vh"),
                downloadButton(outputId = "download_avtrn_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_ano_ts_data",
                               label = "Download anomaly time series data"),
              ),
              tabPanel(
                status = 'primary',
                title = "Timeseries line plot",
                echarts4rOutput("echarts_trn_plt", height = "50vh")
              )
            )
          )),
        ),
      ),
      ##### footer -----
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

    ## Report ----
    tabPanel(
      title = "Report",
      value = "report",
      column(width = 12,
             wellPanel(
               HTML("<h3><b>BC climate anomaly report</b></h2>"),
               HTML(
                 "<h4>The following link provide a report on HTML  format that shows the spatial climate anomaly maps of
            western North America and timeseries plot and trend of BC's spatially averaged anomalies along with summary notes.</h>"
               )
             ),
            fluidPage(#   fluidRow(
              #   box(
              #     width = 8,
              #     status = "primary",
              #     height = "12vh",
              #     uiOutput("doc_pdf")
              #   )
              # ),
              fluidRow(
                box(
                  width = 8,
                  height = "12vh",
                  status = "primary",
                  uiOutput("doc_html")
                )
              ))),
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
    ## Feedback and links ----
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
          <br>"
        )
      ),
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
  # Maps and plots tab ----
  # Filters ------
  # # Filter : Area
  observeEvent(input$major_area, {
    if (input$major_area == "Ecoregions") {
      showElement("ecoprov_area")
      hideElement("wtrshd_area")
    } else if (input$major_area == "Watersheds") {
      showElement("wtrshd_area")
      hideElement("ecoprov_area")
    } else{
      hideElement("ecoprov_area")
      hideElement("wtrshd_area")
    }

    # to select watersheds within ecoregions

    # selection1 <- input$ecoprov_area
    # if(input$wtrshd_area != "All"){
    #   updatePickerInput(session, "wtrshd_area", choices = "All")
    # }
    # if(selection1 %in% eco_wrtshd$eco_nm){
    #   options2 <- eco_wrtshd[eco_nm == selection1, wrt_nm]
    #   updatePickerInput(session, "wtrshd_area", choices = c("All", options2),selected = "All")
    # }

  })

  # Filter : parameter

  output$par_picker <- renderUI({
    par_choices <- parameters
    par_choices <-
      list(
        "Minimum Temperature" = 'tmin',
        "Maximum Temperature" = 'tmax',
        "Mean Temperature" = 'tmean',
        "Precipitation" = 'prcp'
      )
    pickerInput(
      "par_picker",
      "Select climate variable"
      ,
      choices = par_choices,
      selected = "tmean"
    )
  })

  # Filter : month
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
      selected = "summer"
    )
  })

  # Filter year range or specific year (s)
  # output$year_range <- renderUI({
  #   min_year <- min_year
  #   max_year <- max_year
  #   sliderInput(
  #     "year_range",
  #     "Select range of years or specific year (s)"
  #     ,
  #     min_year,
  #     max_year
  #     ,
  #     value = c((max_year - 5), (max_year)),
  #     sep = ""
  #   )
  # })

  # # Filter by year of choice
  # output$year_specific <- renderUI({
  #   yr_choices <- sort(years, decreasing = T)
  #   selectInput("year_specific",
  #               "year(s)",
  #               choices = yr_choices,
  #               multiple = T,
  #               selected = max_year)
  # })

  #interactive years choices

  whichInput <- reactiveValues(type = "specific")


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


  get_years <- reactive({
    if (whichInput$type == "specific") {
      sel_yrs <- input$year_specific
      #print(sel_yrs)
    } else{
      sel_yrs <- seq(input$year_range[1], input$year_range[2], 1)
    }
  })

  # Area shape file interactive

  get_shapefile <- reactive({
    if (input$major_area == "BC") {
      sel_area_shpfl <- bc_shp
    } else if (input$major_area == "Western North America") {
      sel_area_shpfl <- wna_shp
    } else if (input$major_area == "Ecoregions") {
      bc_ecoprv_shp %>%
        filter(name == input$ecoprov_area) -> sel_area_shpfl
      if (input$ecoprov_area == "All_ecoregions")
        sel_area_shpfl <- bc_shp
    } else if (input$major_area == "Watersheds") {
      bc_wtrshd_shp %>%
        filter(MJR_WTRSHM == input$wtrshd_area) -> sel_area_shpfl
      if (input$wtrshd_area == "All_watersheds")
        sel_area_shpfl <- bc_shp
    }
    sel_area_shpfl
  })

  # Spatial anomaly data : reactive to selection
  reactive_ano_dt_fl <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    ano_dt_fl %>%
      filter(mon == input$month_picker &
               par == input$par_picker) -> ano_dt_fl_mon
    ano_dt_sel_rast <- rast(ano_dt_fl_mon$dt_pth)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)

    # Clip by shapefile of the selected area
    #browser()
    sel_area_shpfl <- get_shapefile()
    #browser()

    # # For sample run
    # monn = "summer"
    # parr = "prcp"
    # sel_yrs <- seq(1980,2023,1)
    # sel_yrs
    # sel_area_shpfl <- bc_shp
    # region = "BC"
    # ano_dt_fl %>%
    #   filter(mon == monn &
    #            par == parr) -> ano_dt_fl_mon
    # ano_dt_sel_rast <- rast(ano_dt_fl_mon$dt_pth)
    # ano_dt_sel_rast
    # terra::plot(ano_dt_sel_rast)

    ano_dt_sel_rast <-
      terra::crop(ano_dt_sel_rast, sel_area_shpfl, mask = T)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)

    # Filter for selected year (s)
     sel_yrs <- get_years()

    if (length(sel_yrs) > 30) {
      sel_yrs <- sel_yrs[1:30]
      shinyalert(
        html = T,
        text = tagList(h3(
          "Too many years selected, maximum 30 allowed."
        )),
        showCancelButton = T
      )
    }

    yr_df <- tibble(paryr = names(ano_dt_sel_rast))
    yr_df %<>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    names(ano_dt_sel_rast) <- yr_df$yr

    ano_dt_fil_rast <-  subset(ano_dt_sel_rast,
                               which(names(ano_dt_sel_rast) %in% sel_yrs))
    ano_dt_fil_rast
  })

  # Create reactive anomaly plot for display and save ------
  sp_ano_plt_rct <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    # Clip by shapefile of the selected area
    sel_area_shpfl <- get_shapefile()
    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    reactive_ano_dt_fl() -> ano_dt_rast

    # ano_dt_rast <-ano_dt_fil_rast

    # Import climatology and calculate % anomaly for precipitation
    names(ano_dt_rast)

    clm_dt_fl %>%
      filter(par == parr) -> clm_dt_fl_par
    clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # plot(clm_dt_sel_rast)
    names(clm_dt_sel_rast) <- months_nam

    # Select for input month
    clm_dt_sel_rast_mon <-
      subset(clm_dt_sel_rast, which(names(clm_dt_sel_rast) %in% monn))
    clm_dt_sel_rast <- clm_dt_sel_rast_mon
    rm(clm_dt_sel_rast_mon)

    # Crop to shpfile
    clm_dt_rast <-
      terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # plot(clm_dt_rast)
    # plot(ano_dt_rast)

    if (parr == 'prcp') {
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

    ano_rng_lmt <- terra::minmax(ano_dt_rast, compute = T)
    minval <- (-1) * (max(abs(ano_rng_lmt), na.rm = T))
    maxval <- (1) * (max(abs(ano_rng_lmt), na.rm = T))

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- " "
    }
    unt

    if (parr == 'tmin') {
      parr_full = "minimum temperature"
    } else if (parr == 'tmax') {
      parr_full = "maximum temperature"
    } else if (parr == 'tmean') {
      parr_full = "mean temperature"
    } else if (parr == 'prcp') {
      parr_full = "total precipitation"
    }
    parr_full

    if (monn == 'annual') {
      monn_full = "annual"
    } else if (monn == 'spring') {
      monn_full = "spring"
    } else if (monn == 'summer') {
      monn_full = "summer"
    } else if (monn == 'fall') {
      monn_full = "fall"
    } else if (monn == 'winter') {
      monn_full = "winter"
    } else if (monn == 'Jan') {
      monn_full = "January"
    } else if (monn == 'Feb') {
      monn_full = "February"
    } else if (monn == 'Mar') {
      monn_full = "March"
    } else if (monn == 'Apr') {
      monn_full = "April"
    } else if (monn == 'May') {
      monn_full = "May"
    } else if (monn == 'Jun') {
      monn_full = "June"
    } else if (monn == 'Jul') {
      monn_full = "July"
    } else if (monn == 'Aug') {
      monn_full = "August"
    } else if (monn == 'Sep') {
      monn_full = "September"
    } else if (monn == 'Oct') {
      monn_full = "October"
    } else if (monn == 'Nov') {
      monn_full = "November"
    } else if (monn == 'Dec') {
      monn_full = "December"
    }
    monn_full

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
    plt_wtrmrk <-
      "Created by Aseem Sharma, BC Ministry of Forests using ERA5-Land hourly data\nContact: Aseem.Sharma@gov.bc.ca"
    plt_wtrmrk

    # Climate plot title ( use log for prcp)
    if (parr == "prcp") {
      par_title <-  paste0(region, " ",
                           parr_full, " anomaly  (% of normal)",
                           " : ",
                           monn_full)
    } else {
      par_title <-  paste0(region, " ",
                           parr_full, " anomaly (", unt,")",
                           " : ",
                           monn_full)
    }

    spatial_ano_plt <-  ggplot() +
      geom_spatraster(data = ano_dt_rast) +
      scale_fill_gradientn(
        name = paste0(parr, " anomaly ", unt),
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
      scale_x_continuous(
        name =  "Longitude (°W) ",
        breaks = seq(xmi - 5, xmx + 5, 10),
        labels = abs,
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        name = "Latitude (°N) ",
        breaks = seq(ymi - 1, ymx + 1, 6),
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
        element_line(colour = "black", linewidth =  1),
        axis.title.y = element_text(
          angle = 90,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 14,
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
          size = 14,
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
        legend.position = 'right',
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, -5, -5, -5),
        legend.title = element_text(size = 15),
        legend.text = element_text(margin = margin(t = -5), size = 16),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = -5),
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

    if (parr == "prcp" & maxval > 200) {
     spatial_ano_plt <- spatial_ano_plt +
        scale_fill_gradientn(
          name = paste0(parr, " anomaly ", unt),
          colours = cpt(pal = "cmocean_curl",
                        n = 100,
                        rev = T),
          na.value = "transparent",
          limits = c(minval, maxval),
          breaks = brks_seq,
          labels = labels_val
        )
    } else if (parr == "prcp") {
      spatial_ano_plt <- spatial_ano_plt +
        scale_fill_gradientn(
          name = paste0(parr_pr, "  anomaly (%) "),
          colours = cpt(pal = "cmocean_curl",
                        n = 100,
                        rev = T),
          na.value = "transparent",
          limits = c(minval, maxval),
          breaks = brks_seq
        )
    }
    spatial_ano_plt <- spatial_ano_plt +
      labs(tag = plt_wtrmrk, title = par_title,subtitle = "Baseline: 1981-2010") +
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
  })

  # Anomaly map plot display ----
  output$ano_map <- renderPlot({
    sp_ano_plt_rct()
  })

  # Location map plot ----
  output$loc_map <- renderLeaflet({
    req(input$ecoprov_area, input$wtrshd_area, input$major_area)

    # Shape file for location
    sel_area_shpfl <- get_shapefile()

    if (input$wtrshd_area == "All_watersheds" &&
        input$major_area == "Watersheds") {
      sel_area_shpfl <- bc_wtrshd_shp['MJR_WTRSHM']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ MJR_WTRSHM,
          popup = ~ MJR_WTRSHM,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )

    } else if (input$ecoprov_area == "All_ecoregions" &&
               input$major_area == "Ecoregions") {
      sel_area_shpfl <- bc_ecoprv_shp['name']

      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~ name,
          popup = ~ name,
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    } else{
      leaflet(sel_area_shpfl) %>%
        addTiles() %>%
        addPolygons(
          color = "Red",
          weight = 1,
          opacity = 1,
          fill = TRUE,
          fillOpacity = 0
        )
    }

  })

  observeEvent(input$loc_map_shape_click, {
    nm <- input$loc_map_shape_click$id
    print(nm)
    if (input$major_area == "Ecoregions") {
      updatePickerInput(session, "ecoprov_area", selected = nm)
    } else if (input$major_area == "Watersheds") {
      updateSelectInput(session, "wtrshd_area", selected = nm)
    }
  })

  # Climate normal plot ----
   ## Climate normal map title ----
  clm_plot_title_info <- reactive({
    req(input$par_picker)
    req(input$month_picker)

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- " "
    }
    unt

    sel_area_shpfl <- get_shapefile()

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region


    if (parr == 'tmin') {
      parr_full = "minimum temperature"
    } else if (parr == 'tmax') {
      parr_full = "maximum temperature"
    } else if (parr == 'tmean') {
      parr_full = "mean temperature"
    } else if (parr == 'prcp') {
      parr_full = "total precipitation"
    }
    parr_full

    if (monn == 'annual') {
      monn_full = "annual"
    } else if (monn == 'spring') {
      monn_full = "spring"
    } else if (monn == 'summer') {
      monn_full = "summer"
    } else if (monn == 'fall') {
      monn_full = "fall"
    } else if (monn == 'winter') {
      monn_full = "winter"
    } else if (monn == 'Jan') {
      monn_full = "January"
    } else if (monn == 'Feb') {
      monn_full = "February"
    } else if (monn == 'Mar') {
      monn_full = "March"
    } else if (monn == 'Apr') {
      monn_full = "April"
    } else if (monn == 'May') {
      monn_full = "May"
    } else if (monn == 'Jun') {
      monn_full = "June"
    } else if (monn == 'Jul') {
      monn_full = "July"
    } else if (monn == 'Aug') {
      monn_full = "August"
    } else if (monn == 'Sep') {
      monn_full = "September"
    } else if (monn == 'Oct') {
      monn_full = "October"
    } else if (monn == 'Nov') {
      monn_full = "November"
    } else if (monn == 'Dec') {
      monn_full = "December"
    }
    monn_full

    if (parr == "prcp") {
      clm_nor_title_txt <-
        # Climate plot title ( use log for prcp)
        paste0(region, " mean ",
               parr_full," (average of  1981-2010)","(", unt,")" ," (log-scale)",
               "  : ",
               monn_full)
    } else{
      clm_nor_title_txt <-  paste0(region, " ",
                                   parr_full, " (average of  1981-2010) ", "(", unt,")" ,
                                   " : ",
                                   monn_full)
    }
    clm_nor_title_txt
  })

  output$clm_nor_title <- renderText({
    clm_plot_title_info()
  })

  # Climate normal data plot : for normal plot
  reactive_clm_dt_plt <- reactive({
    req(input$major_area)
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- " "
    }
    unt

    sel_area_shpfl <- get_shapefile()
    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    clm_dt_fl %>%
      filter(par == input$par_picker) -> clm_dt_fl_par
    clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # plot(clm_dt_sel_rast)

    # clm_dt_fl %>%
    #   filter(par == "tmean") -> clm_dt_fl_par
    # clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # clm_dt_sel_rast
    # sel_area_shpfl <- bc_shp

    names(clm_dt_sel_rast) <- months_nam

    # Select for input month
    clm_dt_sel_rast_mon <-
      subset(clm_dt_sel_rast, which(names(clm_dt_sel_rast) %in% monn))
    clm_dt_sel_rast <- clm_dt_sel_rast_mon
    rm()

    # Clip by shape file of the selected area
    #browser()
    sel_area_shpfl <- get_shapefile()
    #browser()

    clm_dt_sel_rast <-
      terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # plot(clm_dt_sel_rast)

    mn_clm_val <-
      round(global(clm_dt_sel_rast, 'mean', na.rm = T), digits = 2)
    mi_clm_val <-
      round(global(clm_dt_sel_rast, 'min', na.rm = T), digits = 2)
    mx_clm_val <-
      round(global(clm_dt_sel_rast, 'max', na.rm = T), digits = 2)

    # Plot using terra rast

    #     plt_wtrmrk <-
    #       "Created by Aseem Sharma BC Ministry of Forests using ERA5-Land hourly data\nContact: Aseem.Sharma@gov.bc.ca"
    #     plt_wtrmrk

    if (parr == "prcp") {
      clm_dt_sel_rast1 <- log(clm_dt_sel_rast)
    } else{
      clm_dt_sel_rast1 <- clm_dt_sel_rast
    }

    spatial_clm_plt <-  ggplot() +
      geom_spatraster(data = clm_dt_sel_rast1) +
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
        breaks = seq(xmi - 5, xmx + 5, 10),
        labels = abs,
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        name = "Latitude (°N) ",
        breaks = seq(ymi - 1, ymx + 1, 6),
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
        element_line(colour = "black", linewidth =  1),
        axis.title.y = element_text(
          angle = 90,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.title.x = element_text(
          angle = 0,
          face = "plain",
          size = 15,
          colour = "Black",
          margin = unit(c(-1, -1, -1, -1), "mm")
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          colour = "black",
          size = 14,
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
          size = 14,
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
          size = 15,
          colour = "Black"
        ),
        legend.position = 'right',
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, -5, -5, -5),
        legend.title = element_text(size = 15),
        legend.text = element_text(margin = margin(t = -5), size = 16),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = -5),
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

    if (parr == "prcp") {
      spatial_clm_plt <- spatial_clm_plt +
        scale_fill_continuous(
          type = "viridis",
          name = " ",
          option = "viridis",
          direction = -1,
          na.value = "transparent"
        )
    }

    # Climate plot title ( use log for prcp)
    # if (parr == "prcp") {
    #   par_title <-  paste0(region, " mean ",
    #                        parr_full,"\n ","(", unt,")" ," (log-scale)",
    #                        " : ",
    #                        monn_full)
    # } else{
    #   par_title <-  paste0(region, " ",
    #                        parr_full, " ", "(", unt,")" ,
    #                        " : ",
    #                        monn_full)
    # }
    spatial_clm_plt <- spatial_clm_plt +
      # labs(tag = plt_wtrmrk) +
      # theme(
      #   plot.tag.position = "bottom",
      #   plot.tag = element_text(
      #     color = 'gray50',
      #     hjust = 1,
      #     size = 8
      #   )
      # ) +
      labs(
        # title = par_title,
        subtitle = paste0(
          'Mean = ',
          mn_clm_val[[1]]," ",
          "(", unt,")" ,
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
  })

  output$clm_nor_map <- renderPlot({
    reactive_clm_dt_plt()
  })
  # Climate normal data : for download
  reactive_clm_dt_fl <- reactive({
    req(input$major_area)
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- " "
    }
    unt

    sel_area_shpfl <- get_shapefile()
    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    clm_dt_fl %>%
      filter(par == input$par_picker) -> clm_dt_fl_par
    clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # plot(clm_dt_sel_rast)

    # clm_dt_fl %>%
    #   filter(par == "tmean") -> clm_dt_fl_par
    # clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    # clm_dt_sel_rast
    # sel_area_shpfl <- bc_shp

    names(clm_dt_sel_rast) <- months_nam

    # Select for input month
    clm_dt_sel_rast_mon <-
      subset(clm_dt_sel_rast, which(names(clm_dt_sel_rast) %in% monn))
    clm_dt_sel_rast <- clm_dt_sel_rast_mon
    rm()

    # Clip by shape file of the selected area
    #browser()
    sel_area_shpfl <- get_shapefile()
    #browser()

    clm_dt_sel_rast <-
      terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # plot(clm_dt_sel_rast)
    clm_dt_sel_rast
  })


  # Anomaly overview table caption text ----
  ano_overview_info <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    sel_area_shpfl <- get_shapefile()

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region
    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    if (parr == 'tmin') {
      parr_full = "minimum temperature"
    } else if (parr == 'tmax') {
      parr_full = "maximum temperature"
    } else if (parr == 'tmean') {
      parr_full = "mean temperature"
    } else if (parr == 'prcp') {
      parr_full = "total precipitation"
    }
    parr_full

    if (monn == 'annual') {
      monn_full = "annual"
    } else if (monn == 'spring') {
      monn_full = "spring"
    } else if (monn == 'summer') {
      monn_full = "summer"
    } else if (monn == 'fall') {
      monn_full = "fall"
    } else if (monn == 'winter') {
      monn_full = "winter"
    } else if (monn == 'Jan') {
      monn_full = "January"
    } else if (monn == 'Feb') {
      monn_full = "February"
    } else if (monn == 'Mar') {
      monn_full = "March"
    } else if (monn == 'Apr') {
      monn_full = "April"
    } else if (monn == 'May') {
      monn_full = "May"
    } else if (monn == 'Jun') {
      monn_full = "June"
    } else if (monn == 'Jul') {
      monn_full = "July"
    } else if (monn == 'Aug') {
      monn_full = "August"
    } else if (monn == 'Sep') {
      monn_full = "September"
    } else if (monn == 'Oct') {
      monn_full = "October"
    } else if (monn == 'Nov') {
      monn_full = "November"
    } else if (monn == 'Dec') {
      monn_full = "December"
    }
    monn_full

    # Years
    # Filter for selected year (s)
    if (input$ab_years_choose %% 2 == 0) {
      sel_yrs <- seq(input$year_range[1], input$year_range[2], 1)
    } else{
      sel_yrs <- input$year_specific
    }

    minyr <- min(sel_yrs)
    maxyr <- max(sel_yrs)

    ano_overview <-
      paste0(
        "The spatially averaged anomaly overview of ",
        region,
        "'s ",
        monn_full,
        " ",
        parr_full,
        " ",
        minyr,
        "—",
        maxyr,
        "."
      )
    ano_overview
  })

  output$anomaly_overview <- renderText({
    ano_overview_info()
  })

  # Anomaly overview  table ----
  output$ano_ovr_tbl <- renderDT({
    reactive_ano_dt_fl() -> ano_dt_rast
    sel_area_shpfl <- get_shapefile()

    # Background requirements
    monn = input$month_picker
    parr = input$par_picker

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- ""
    }
    unt

    #For percentage of precipitation anomaly
    names(ano_dt_rast)

    clm_dt_fl %>%
      filter(par == parr) -> clm_dt_fl_par
    clm_dt_sel_rast <- rast(clm_dt_fl_par$dt_pth)
    names(clm_dt_sel_rast) <- months_nam

    # Select for input month
    clm_dt_sel_rast_mon <-
      subset(clm_dt_sel_rast, which(names(clm_dt_sel_rast) %in% monn))
    clm_dt_sel_rast <- clm_dt_sel_rast_mon
    rm(clm_dt_sel_rast_mon)
    # Crop to shpfile
    clm_dt_rast <-
      terra::crop(clm_dt_sel_rast, sel_area_shpfl, mask = T)
    # plot(clm_dt_rast)
    # plot(ano_dt_rast)

    if (parr == 'prcp') {
      ano_dt_rast_per1 <- (ano_dt_rast / clm_dt_rast) * 100
      #If prcp anomalies are very hihgn ( > 200 %) then convert and limit to 200.
      ano_dt_rast_per2 <-
        ifel(ano_dt_rast_per1 > 201, 200, ano_dt_rast_per1)
      ano_dt_rast_per3 <-
        ifel(ano_dt_rast_per2 < -201, -200, ano_dt_rast_per2)
      ano_dt_rast_per <- ano_dt_rast_per3
    } else{
      ano_dt_rast_per <- ano_dt_rast
    }
    # plot(ano_dt_rast_per,1)
    ano_dt_rast_per

    mn_ano <-
      terra::global(ano_dt_rast, fun = "mean", na.rm = T)
    mn_ano
    mn_ano <- round(mean(mn_ano$mean, na.rm = T), 2)
    mi_ano <- terra::global(ano_dt_rast, fun = "min", na.rm = T)
    mi_ano <- round(min(mi_ano$min, na.rm = T), 2)
    mx_ano <- terra::global(ano_dt_rast, fun = "max", na.rm = T)
    mx_ano <- round(max(mx_ano$max, na.rm = T), 2)


    #Percentage
    mn_ano_per <-
      terra::global(ano_dt_rast_per, fun = "mean", na.rm = T)
    mn_ano_per <- round(mean(mn_ano_per$mean, na.rm = T), 2)
    mi_ano_per <-
      terra::global(ano_dt_rast_per, fun = "min", na.rm = T)
    mi_ano_per <- round(min(mi_ano_per$min, na.rm = T), 2)
    mx_ano_per <-
      terra::global(ano_dt_rast_per, fun = "max", na.rm = T)
    mx_ano_per <- round(max(mx_ano_per$max, na.rm = T), 2)

    #Combine for a display table
    if (parr == 'prcp') {
      mi_ano_val = paste0(mi_ano_per, "% of normal (", mi_ano, unt, ")")
      mn_ano_val = paste0(mn_ano_per, "% of normal (", mn_ano, unt, ")")
      mx_ano_val = paste0(mx_ano_per, "% of normal (", mx_ano, unt, ")")
    } else {
      mi_ano_val = paste0(mi_ano, unt)
      mn_ano_val = paste0(mn_ano, unt)
      mx_ano_val = paste0(mx_ano, unt)
    }

    # Create a table
    ano_ovr_dt <-
      data.frame(
        "Anomaly" = c("Minimum", "Mean", "Maximum"),
        "Value" = c(mi_ano_val, mn_ano_val, mx_ano_val)
      )
    ano_ovr_dt
    datatable(
      ano_ovr_dt,
      rownames = FALSE,
      options = list(
        lengthChange = FALSE,
        info = FALSE,
        paging = FALSE,
        searching = FALSE
      )
    )

  })

  # Spatially averaged anomaly trend plot ----
  # clip the anomalies to selected shape file area and calculate spatially average values and trend
  # Reactive data
  reactive_ano_dt_fl_sp <- reactive({
    req(input$month_picker)
    req(input$par_picker)
    req(input$major_area)

    ano_dt_fl %>%
      filter(mon == input$month_picker &
               par == input$par_picker) -> ano_dt_fl_mon
    ano_dt_sel_rast <- rast(ano_dt_fl_mon$dt_pth)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast)

    # ano_dt_sel_rast <- rast(ano_dt_fl_mon$dt_pth)

    # Clip by shapefile of the selected area
    sel_area_shpfl <- get_shapefile()

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

      # other requirements
    monn = input$month_picker
    parr = input$par_picker

   ano_dt_shp_rast <-
      terra::crop(ano_dt_sel_rast, sel_area_shpfl, mask = T)
    ano_dt_shp_rast
    # plot(ano_dt_shp_rast,74)
    yr_df <- tibble(paryr = names(ano_dt_shp_rast))
    yr_df %<>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    names(ano_dt_shp_rast) <- yr_df$yr

    # Shapefile spatial average anomalies by year
    ano_shp_av_dt <-
      tibble(rownames_to_column(global(
        ano_dt_shp_rast, fun = "mean", na.rm = T
      ), "yr")) %>%
      dplyr::select(yr, ano = mean)
    ano_shp_av_dt %<>%
      drop_na()
    ano_shp_av_dt$yr <-
      as.numeric(str_extract(ano_shp_av_dt$yr, "[0-9]+"))
    ano_shp_av_dt$par <- unique(ano_dt_fl_mon$par)
    ano_shp_av_dt$mon <- unique(ano_dt_fl_mon$mon)
    ano_shp_av_dt$region <- region
    ano_shp_av_dt
  })

  # ggplot2 trend plot  ----
  spatial_av_trnd_plt_rct <- reactive({
    reactive_ano_dt_fl_sp() -> ano_shp_dt
    ano_shp_dt
    # ano_shp_dt <-  ano_shp_av_dt
    # parr <- "tmean"
    # mon <- "summer"
    # region <- "BC"

    # Background requirements for plots
    parr <- unique(ano_shp_dt$par)
    monn <- unique(ano_shp_dt$mon)
    region <- unique(ano_shp_dt$region)

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- ""
    }
    unt

    if (parr == 'tmin') {
      parr_full = "minimum temperature"
    } else if (parr == 'tmax') {
      parr_full = "maximum temperature"
    } else if (parr == 'tmean') {
      parr_full = "mean temperature"
    } else if (parr == 'prcp') {
      parr_full = "total precipitation"
    }
    parr_full

    if (monn == 'annual') {
      monn_full = "annual"
    } else if (monn == 'spring') {
      monn_full = "spring"
    } else if (monn == 'summer') {
      monn_full = "summer"
    } else if (monn == 'fall') {
      monn_full = "fall"
    } else if (monn == 'winter') {
      monn_full = "winter"
    } else if (monn == 'Jan') {
      monn_full = "January"
    } else if (monn == 'Feb') {
      monn_full = "February"
    } else if (monn == 'Mar') {
      monn_full = "March"
    } else if (monn == 'Apr') {
      monn_full = "April"
    } else if (monn == 'May') {
      monn_full = "May"
    } else if (monn == 'Jun') {
      monn_full = "June"
    } else if (monn == 'Jul') {
      monn_full = "July"
    } else if (monn == 'Aug') {
      monn_full = "August"
    } else if (monn == 'Sep') {
      monn_full = "September"
    } else if (monn == 'Oct') {
      monn_full = "October"
    } else if (monn == 'Nov') {
      monn_full = "November"
    } else if (monn == 'Dec') {
      monn_full = "December"
    }
    monn_full

    # Trend on average anomaly 1950 - now
    ano_shp_dt %<>%
      filter(yr > 1950) %<>%
      mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
        # incpt =zyp.trend.vector(ano)[["intercept"]],
        #sig = zyp.trend.vector(ano)[["sig"]])
        sig = round(MannKendall(ano)[[2]], digits = 2))
    ano_shp_dt

    ano_mk_trnd <-
      zyp.sen(ano ~ yr, ano_shp_dt)##Give the trend###
    ano_mk_trnd$coefficients
    ano_shp_dt$trn <-  ano_mk_trnd$coeff[[2]]
    ano_shp_dt$incpt <-  ano_mk_trnd$coeff[[1]]

    xs = c(min(ano_shp_dt$yr), max(ano_shp_dt$yr))
    trn_slp = c(unique(ano_shp_dt$incpt), unique(ano_shp_dt$trn))
    ys = cbind(1, xs) %*% trn_slp
    ano_shp_dt$trn_lab = paste(
      "italic(1951-trend)==",
      round(ano_shp_dt$trn, 2),
      "~','~italic(p)==",
      round(ano_shp_dt$sig, 2)
    )

    # Trend on average anomaly 1980 - now
    ano_shp_dt %>%
      filter(yr > 1979) %>%
      mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
        # incpt =zyp.trend.vector(ano)[["intercept"]],
        #sig = zyp.trend.vector(ano)[["sig"]])
        sig = round(MannKendall(ano)[[2]], digits = 2)) -> ano_shp_dt80
    ano_shp_dt80

    ano_mk_trnd80 <-
      zyp.sen(ano ~ yr, ano_shp_dt80)##Give the trend###
    ano_mk_trnd80$coefficients
    ano_shp_dt80$trn <-  ano_mk_trnd80$coeff[[2]]
    ano_shp_dt80$incpt <-  ano_mk_trnd80$coeff[[1]]

    xs80 = c(min(ano_shp_dt80$yr), max(ano_shp_dt80$yr))
    trn_slp80 = c(unique(ano_shp_dt80$incpt), unique(ano_shp_dt80$trn))
    ys80 = cbind(1, xs80) %*% trn_slp80
    ano_shp_dt80$trn_lab = paste(
      "italic(1980-trend)==",
      round(ano_shp_dt80$trn, 2),
      "~','~italic(p)==",
      round(ano_shp_dt80$sig, 2)
    )

    # anomaly plot
    ymin <- (-1) * (max(abs(ano_shp_dt$ano)))
    ymax <- (1) * (max(abs(ano_shp_dt$ano)))
    minyr <- min(ano_shp_dt$yr)
    maxyr <- max(ano_shp_dt$yr)

    ybrk_neg <-
      ceiling(c(seq((-1) * (max(
        abs(ano_shp_dt$ano)
      )), 0, length.out = 4)))
    ybrk_neg
    ybrk_pos <-
      floor(c(seq(0, (1) * (max(
        abs(ano_shp_dt$ano)
      )), length.out = 4)))[-1]
    ybrk_pos

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

    ybrks_seq <- c(ybrk_negn, ybrk_posp)

    # Positive and negative anomalies and 3 years moving average to create bar plot
    ano_shp_dt %<>%
      mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
      mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
    ano_shp_dt
    tail(ano_shp_dt)

    plt_wtrmrk <-
      "Created by Aseem Sharma, BC Ministry of Forests using ERA5-Land hourly data\nContact: Aseem.Sharma@gov.bc.ca"
    plt_wtrmrk

    if (parr == "prcp") {
      par_title <-  paste0(region, " ",
                           parr_full, " ", "anomaly", " (% of normal)",
                           " : ",
                           monn_full)
    } else{
      par_title <-  paste0(region, " ",
                           parr_full, " ", "anomaly"," (", unt,")",
                           " : ",
                           monn_full)
    }

    if (parr == "prcp") {
      y_axis_lab <- paste0(parr, " average anomaly (% of normal)")
    } else{
      y_axis_lab <- paste0(parr, " average anomaly ", "(", unt, ")")
    }

    ano_shp_trn_plt <-
      ggplot(data = ano_shp_dt, aes(x = yr, y = ano)) +
      annotate(
        geom = 'text',
        label = plt_wtrmrk,
        x = Inf,
        y = -Inf,
        hjust = 1,
        vjust = -0.3,
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
        name = paste0(parr, " anomaly ", unt),
        colours = cpt(pal = "ncl_BlWhRe",
                      n = 100,
                      rev = F),
        limits = c(ymin, ymax),
        breaks = ybrks_seq
      ) +
      geom_line(
        aes(y = ano_mv, color = "3 years moving average"),
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
          color = "1950-trend line"
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
        parse = TRUE
      ) +
      # add 80s trend
      geom_segment(
        aes(
          x = xs80[[1]],
          xend = xs80[[2]],
          y = ys80[[1]],
          yend = ys80[[2]],
          color = "1980-trend line"
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
      scale_y_continuous(name = y_axis_lab,
                         limits = c(ymin, ymax),
                         breaks = ybrks_seq) +
      labs(title = par_title, subtitle = "Baseline: 1981-2010") +
      scale_color_manual(
        " ",
        values = c(
          "3 years moving average" = "green",
          "1950-trend line" = "black",
          "1980-trend line" = "deepskyblue2"
        ),
        labels =  c(
          "3 years moving average" = "3-years moving \n mean",
          "1950-trend line" = "1950-trend",
          "1980-trend line" = "1980-trend"
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
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size = 13),
        legend.text = element_text(margin = margin(t = -5), size = 12),
        strip.text.x = element_text(size = 12, angle = 0),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(margin = -5),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = 'Black')
      )
    ano_shp_trn_plt
    if (parr == "prcp") {
      ano_shp_trn_plt <- ano_shp_trn_plt +
        scale_fill_gradientn(
          name = paste0(parr_pr, "  anomaly ", unt),
          colours = cpt(pal = "cmocean_curl",
                        n = 100,
                        rev = T),
          limits = c(ymin, ymax),
          breaks = ybrks_seq
        )
    }
    ano_shp_trn_plt

  })
  #Display bar trend plot
  output$spatial_trn_plt <- renderPlot({
    spatial_av_trnd_plt_rct()
  })

  # Time series line plot ----

  output$echarts_trn_plt <- renderEcharts4r({
    reactive_ano_dt_fl_sp() -> ano_shp_dt
    ano_shp_dt %<>%
      filter(yr > 1950)

    # Background requirements for plots
    parr <- unique(ano_shp_dt$par)
    monn <- unique(ano_shp_dt$mon)
    region <- unique(ano_shp_dt$region)

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- ""
    }
    unt

    if (parr == 'tmin') {
      parr_full = "minimum temperature"
    } else if (parr == 'tmax') {
      parr_full = "maximum temperature"
    } else if (parr == 'tmean') {
      parr_full = "mean temperature"
    } else if (parr == 'prcp') {
      parr_full = "total precipitation"
    }
    parr_full

    if (monn == 'annual') {
      monn_full = "annual"
    } else if (monn == 'spring') {
      monn_full = "spring"
    } else if (monn == 'summer') {
      monn_full = "summer"
    } else if (monn == 'fall') {
      monn_full = "fall"
    } else if (monn == 'winter') {
      monn_full = "winter"
    } else if (monn == 'Jan') {
      monn_full = "January"
    } else if (monn == 'Feb') {
      monn_full = "February"
    } else if (monn == 'Mar') {
      monn_full = "March"
    } else if (monn == 'Apr') {
      monn_full = "April"
    } else if (monn == 'May') {
      monn_full = "May"
    } else if (monn == 'Jun') {
      monn_full = "June"
    } else if (monn == 'Jul') {
      monn_full = "July"
    } else if (monn == 'Aug') {
      monn_full = "August"
    } else if (monn == 'Sep') {
      monn_full = "September"
    } else if (monn == 'Oct') {
      monn_full = "October"
    } else if (monn == 'Nov') {
      monn_full = "November"
    } else if (monn == 'Dec') {
      monn_full = "December"
    }
    monn_full

    # Trend on average anomaly
    ano_shp_dt %<>%
      mutate(# trnd =zyp.trend.vector(ano)[["trend"]],
        # incpt =zyp.trend.vector(ano)[["intercept"]],
        #sig = zyp.trend.vector(ano)[["sig"]])
        sig = round(MannKendall(ano)[[2]], digits = 2))
    ano_shp_dt

    ano_mk_trnd <-
      zyp.sen(ano ~ yr, ano_shp_dt)##Give the trend###
    ano_mk_trnd$coefficients
    ano_shp_dt$trn <-  ano_mk_trnd$coeff[[2]]
    ano_shp_dt$incpt <-  ano_mk_trnd$coeff[[1]]

    xs = c(min(ano_shp_dt$yr), max(ano_shp_dt$yr))
    trn_slp = c(unique(ano_shp_dt$incpt), unique(ano_shp_dt$trn))
    ys = cbind(1, xs) %*% trn_slp
    ano_shp_dt$trn_lab = paste(
      "italic(trend)==",
      round(ano_shp_dt$trn, 2),
      "~','~italic(p)==",
      round(ano_shp_dt$sig, 2)
    )

    # anomaly plot
    ymin <- (-1) * (max(abs(ano_shp_dt$ano)))
    ymax <- (1) * (max(abs(ano_shp_dt$ano)))
    minyr <- min(ano_shp_dt$yr)
    maxyr <- max(ano_shp_dt$yr)

    ybrk_neg <-
      ceiling(c(seq((-1) * (max(
        abs(ano_shp_dt$ano)
      )), 0, length.out = 4)))
    ybrk_neg
    ybrk_pos <-
      floor(c(seq(0, (1) * (max(
        abs(ano_shp_dt$ano)
      )), length.out = 4)))[-1]
    ybrk_pos

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

    ybrks_seq <- c(ybrk_negn, ybrk_posp)

    # Positive and negative anomalies and 3 years moving average to create bar plot
    ano_shp_dt %<>%
      mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
      mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
    # echarts4R trend plot
    ano_shp_dt$yr_chr <- as.character(ano_shp_dt$yr)
    yymin <- ceiling(ymin)
    yymax <- floor(ymax)

    ano_shp_dt$trendline <-
      ano_mk_trnd$coeff[[1]] + ano_mk_trnd$coeff[[2]] * ano_shp_dt$yr

    if (parr == "prcp") {
      par_title <-  paste0(region, " ",
                           parr_full, " anomaly (% of normal)",
                           " : ",
                           monn_full)
    } else{
      par_title <-  paste0(region, " ",
                           parr_full, " anomaly", " (", unt,")",
                           " : ",
                           monn_full)
    }

    chart_title <- par_title

    ano_shp_dt %>%
      e_charts(x = yr_chr) %>%
      e_line(serie = ano, color = "blue", ) %>% # add a line
      e_scatter(serie = ano, color = 'blue') %>%
      e_line(serie = trendline,
             name = "Trendline",
             smooth = F) %>%
      # e_lm(ano ~ yr, name = "Linear model", color="black") %>%
      # e_y_axis(min = yymin, max = yymax) |>
      e_legend(
        show = F,
        orient = 'vertical',
        left = 100,
        top = 50
      ) %>%
      e_tooltip(axisPointer = list(type = "cross")) %>%
      e_theme("infographic") %>%
      e_title(text = chart_title)
  })

  # Download data save plots ---------------

   ## Spatial anomaly map and raster data download ----
  file_nam_info <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)


    sel_area_shpfl <- get_shapefile()
    # plot(st_geometry(sel_area_shpfl))

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    fl_nam <-
      paste0(region,
             "_",
             parr,"_anomaly",
             "_",
             monn,
             "_",
             input$year_range[1],
             "_",
             input$year_range[2])
    fl_nam
  })

  # Spatial anomaly data download as raster (tif )
  output$download_ano_data <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info(), "_data.tif")
    },
    content = function(file) {
      writeRaster(reactive_ano_dt_fl(),
                  file,
                  filetype = "GTiff",
                  overwrite = TRUE)
    }
  )

  # Spatial anomaly map save
  output$download_ano_plt <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info(), "_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = sp_ano_plt_rct(),
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

  ## Anomaly time series and plot download ----
  file_nam_info_sp_av <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    sel_area_shpfl <- get_shapefile()

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    # Year range
    if (monn != "annual") {
      mx_yr = max_year
    } else {
      mx_yr = max_year - 1
    }

    fl_nam <-
      paste0(region,
             "_",
             parr,"_anomaly_timeseries",
             "_",
             monn,
             "_",
             min_year,
             "_",
             mx_yr)
    fl_nam
  })

  # Spatial anomaly time series data download (.csv)
  output$download_ano_ts_data <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_sp_av(),
             "_data.csv")
    },
    content = function(file) {
      write_csv(reactive_ano_dt_fl_sp(),
                file, append = FALSE)
    }
  )

  #Spatially averaged trend plot save
  output$download_avtrn_plt <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_sp_av(), "_trend_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = spatial_av_trnd_plt_rct(),
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
  ## Climate normal data and plot download  ----
  file_nam_info_clm_nor <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- ""
    }
    unt

    sel_area_shpfl <- get_shapefile()

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    if (parr == 'tmin') {
      parr_full = "minimum temperature"
    } else if (parr == 'tmax') {
      parr_full = "maximum temperature"
    } else if (parr == 'tmean') {
      parr_full = "mean temperature"
    } else if (parr == 'prcp') {
      parr_full = "total precipitation"
    }
    parr_full

    if (monn == 'annual') {
      monn_full = "annual"
    } else if (monn == 'spring') {
      monn_full = "spring"
    } else if (monn == 'summer') {
      monn_full = "summer"
    } else if (monn == 'fall') {
      monn_full = "fall"
    } else if (monn == 'winter') {
      monn_full = "winter"
    } else if (monn == 'Jan') {
      monn_full = "January"
    } else if (monn == 'Feb') {
      monn_full = "February"
    } else if (monn == 'Mar') {
      monn_full = "March"
    } else if (monn == 'Apr') {
      monn_full = "April"
    } else if (monn == 'May') {
      monn_full = "May"
    } else if (monn == 'Jun') {
      monn_full = "June"
    } else if (monn == 'Jul') {
      monn_full = "July"
    } else if (monn == 'Aug') {
      monn_full = "August"
    } else if (monn == 'Sep') {
      monn_full = "September"
    } else if (monn == 'Oct') {
      monn_full = "October"
    } else if (monn == 'Nov') {
      monn_full = "November"
    } else if (monn == 'Dec') {
      monn_full = "December"
    }
    monn_full

    fl_nam <-
      paste0(region,
             "_",
             parr_full,"_climate_normal_1981_2010",
             "_",
             monn_full)
    fl_nam
  })

  dwnlnd_clm_plt <- reactive({
    req(input$par_picker)
    req(input$month_picker)
    req(input$year_range)

    # other requirements
    monn = input$month_picker
    parr = input$par_picker

    sel_area_shpfl <- get_shapefile()

    if (input$major_area == "BC") {
      region = "BC"
    } else if (input$major_area == "Western North America") {
      region = "Western North America"
    } else if (input$major_area == "Ecoregions") {
      region = input$ecoprov_area
    } else if (input$major_area == "Watersheds") {
      region = region = input$wtrshd_area
    }
    region

    # units
    if (parr == "tmax" | parr == "tmin" | parr == "tmean") {
      unt <- "°C"
    } else if (parr == "prcp") {
      unt <- "mm"
      parr_pr <- "prcp"
    } else if (parr == "RH") {
      unt <- "%"
    } else {
      unt <- ""
    }
    unt

      plt_wtrmrk <-
        "Created by Aseem Sharma BC Ministry of Forests using ERA5-Land hourly data\nContact: Aseem.Sharma@gov.bc.ca"
        plt_wtrmrk
      # Climate plot title ( use log for prcp)
      if (parr == "prcp") {
        par_title <-  paste0(region, " mean ",
                             parr,"","(", unt,")" ," (log-scale)",
                             " : ",
                             monn)
      } else{
        par_title <-  paste0(region, " ",
                             parr, " ", "(", unt,")" ,
                             " : ",
                             monn)
      }

    dwn_clm_plt <- reactive_clm_dt_plt() +
    labs(tag = plt_wtrmrk,title = par_title) +
    theme(
      plot.tag.position = 'bottom',
      plot.tag = element_text(
        color = 'gray50',
        hjust = 1,
        size = 8
      )
    ) +
    theme(
      plot.title = element_text(size = 12, face = 'plain'),
      plot.subtitle = element_text(size = 10)
    )
    dwn_clm_plt
  })

  # Climatological normal plot save
  output$download_clm_nor_plt <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_clm_nor(), "_plot.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot =  dwnlnd_clm_plt(),
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

  # Download climate normal data in tiff
  output$download_clm_nor_data <- downloadHandler(
    filename = function(file) {
      paste0(file_nam_info_clm_nor(), "_data.tif")
    },
    content = function(file) {
      writeRaster(reactive_clm_dt_fl(),
                  file,
                  filetype = "GTiff",
                  overwrite = TRUE)
    }
  )

    # Reset  selection /filters -----
  observeEvent(input$reset_input, {
    shinyjs::reset("selection-panel")
  })

  # observeEvent(input$reset_input, {
  #   shinyjs::reset("sel_yrs")
  # })

  # Feedback text -------
  output$feedback_text <- renderText({
    HTML(
      "<p>We used <a href='https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview'>
      ERA5-Land hourly data</a> to calculate the anomalies and climatology.
      Anomalies are calculated as the measure of departure from the climatological averages spanning from 1981 to 2010.
      Should you have any inquiries or wish to provide feedback, please do not hesitate to use
      <a href=https://forms.office.com/r/wN0QYAvSTZ'> this feedback form </a> or write to Aseem Sharma @ <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> . </p>"
    )

  })
  # Reports ----

  # pdf_file_name = paste0("BC_climate_anomaly_",
  #                        update_month,
  #                        "_",
  #                        update_year,
  #                        ".pdf")
  html_file_name = paste0("BC_climate_anomaly_",
                          update_month,
                          "_",
                          update_year,
                          ".html")

  # HTML in the shiny www folder
  output$doc_html <- renderUI({
    a(
      "BC climate anomaly report:",
      html_file_name,
      target = "_blank",
      style = "font-size:20px;",
      href = html_file_name,
      img(
        src = "html_logo.png",
        height = "3%",
        width = "3%",
        align = "center"
      )
    )
  })

  # # pdf in the shiny www folder
  #   output$doc_pdf <- renderUI({
  #     a(
  #       "BC climate anomaly report:",
  #       pdf_file_name,
  #       target = "_blank",
  #       style = "font-size:20px;",
  #       href = pdf_file_name,
  #       img(
  #         src = "pdf_logo.png",
  #         height = "3%",
  #         width = "3%",
  #         align = "center"
  #       )
  #     )
  #   })

  # App deployment date ----
  output$deploymentDate <- renderText({
    paste0("This app was last updated on ",
           readLines("deployment_history.txt"),
           ".")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
