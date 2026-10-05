## BC Climate anomaly app: An app to visualize monthly and annual temperature and precipitation anomalies in BC and its sub regions or user defined area along with their trends.
## author: Aseem Raj Sharma, PhD. E-mail: aseem.sharma@gov.bc.ca
# Copyright 2023 Province of British Columbia
# Licensed under the Apache License, Version 2.0 (the "License");
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This script need to be run after running trend calculation, bc monthly report quarto and upload history scripts.
# Required libraries -------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
library(plotly)

library(markdown)
library(rmarkdown)

library(terra)
library(tidyterra)
library(leaflet)

library(tidyverse)
library(magrittr)
library(lubridate)

library(zoo)
library(zyp)
library(colorspace)
library(cptcity)

# Paths -----------------------------------------------------------------------
shp_fls_pth <- './shapefiles/'
ano_dt_pth <- './ano_clm_trn_data/'

# Global Metadata -------------------------------------------------------------
plt_wtrmrk <- "@Aseem R. Sharma, BC Ministry of Forests. Data credit: ERA5land/C3S/ECMWF."
app_deployment_date <- format(Sys.Date(), "%d %B, %Y")

# Extent Domain (Western North America) ----------------------------------------
xmi <- -140
xmx <- -108
ymi <- 39
ymx <- 60

# Update month, season, year ------------------------------
min_year <- 1951
max_year <- 2026

update_month <- "August"
update_year <- "2026"

# Load Shape files -------------------------------------------------------
shp_fls_lst <- list.files(
  path = shp_fls_pth,
  pattern = "\\.(shp|gpkg)$",
  full.names = TRUE,
  ignore.case = TRUE
)

get_shp <- function(lst, pattern) {
  match_file <- lst[str_detect(lst, pattern)]
  if (length(match_file) == 0) {
    stop(paste("Shapefile matching pattern '", pattern, "' not found."))
  }
  vect(match_file[1])
}

target_crs <- "EPSG:4326"

# User-supplied location helpers -------------------------------------------------
# The app works internally in WGS84 (EPSG:4326).
# Uploaded shapefiles are validated and reprojected to WGS84.
# Check that the ENTIRE uploaded geometry is inside the Western North America
# climate-data domain. Because the domain is a rectangle, checking the complete
# bounding extent is sufficient to guarantee that the whole geometry is inside.

check_user_location_domain <- function(shp) {
  # Coordinates must be in WGS84 before checking the domain.
  shp <- terra::project(shp, "EPSG:4326")

  bb <- terra::ext(shp)

  # Require the complete shapefile extent to be within the domain.
  outside_domain <-
    bb$xmin < xmi ||
    bb$xmax > xmx ||
    bb$ymin < ymi ||
    bb$ymax > ymx

  if (outside_domain) {
    stop(
      paste0(
        "The entire uploaded shapefile must be within the Western North America ",
        "domain. Allowed longitude: ",
        xmi,
        " to ",
        xmx,
        " degrees; allowed latitude: ",
        ymi,
        " to ",
        ymx,
        " degrees. The uploaded geometry extends outside this domain."
      )
    )
  }

  shp
}

# Common validation for both uploaded shapefiles and point location
validate_user_shapefile <- function(shp, target_crs = "EPSG:4326") {
  if (is.null(shp) || nrow(shp) == 0) {
    stop("The shapefile contains no features.")
  }

  # A valid CRS is required so the geometry can be correctly transformed.
  shp_crs <- terra::crs(shp, proj = TRUE)
  if (is.na(shp_crs) || shp_crs == "") {
    stop(
      "The shapefile has no coordinate reference system (.prj missing). Please provide a shapefile with a valid projection."
    )
  }

  # Validate geometry before analysis.
  valid <- tryCatch(
    terra::is.valid(shp),
    error = function(e) rep(FALSE, nrow(shp))
  )

  if (any(!valid)) {
    stop(
      "The shapefile contains invalid geometries. Please repair the geometry and upload it again."
    )
  }

  # Reproject to the app's working CRS.
  shp <- tryCatch(
    terra::project(shp, target_crs),
    error = function(e) {
      stop("The shapefile projection could not be converted to EPSG:4326.")
    }
  )

  # IMPORTANT: the ENTIRE geometry must be inside the Western North America
  # domain, not merely intersect it.
  shp <- check_user_location_domain(shp)

  # Check supported geometry types.
  geom_types <- unique(terra::geomtype(shp))

  if (!all(geom_types %in% c("polygons", "points", "lines"))) {
    stop("The uploaded shapefile contains unsupported geometry.")
  }

  # A point shapefile must contain exactly one point.
  if (all(terra::geomtype(shp) == "points") && nrow(shp) != 1) {
    stop(
      "A point shapefile must contain exactly one point. For multiple locations, upload a polygon shapefile instead."
    )
  }

  # The app supports polygon shapefiles or exactly one point.
  if (!all(terra::geomtype(shp) %in% c("polygons", "points"))) {
    stop(
      "The custom shapefile must contain polygon features or exactly one point."
    )
  }

  if (
    length(geom_types) > 1 ||
      (!all(terra::geomtype(shp) == "points") &&
        !all(terra::geomtype(shp) %in% c("polygons", "multipolygons")))
  ) {
    # Keep mixed geometry handling conservative.
    if (!all(terra::geomtype(shp) %in% c("polygons", "multipolygons"))) {
      stop(
        "The custom shapefile must contain polygon features or exactly one point."
      )
    }
  }

  shp
}

# Read and validate an uploaded shapefile -------------------------------------

read_user_shapefile <- function(uploaded_files, target_crs = "EPSG:4326") {
  ## 1. Check upload ------------------

  if (is.null(uploaded_files) || nrow(uploaded_files) == 0) {
    stop("No shapefile was uploaded.")
  }

  ## 2. Create temporary folder --------------

  shp_dir <- tempfile("user_shapefile_")
  dir.create(
    shp_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ## 3. Copy uploaded shapefile components using original filenames -------------

  for (i in seq_len(nrow(uploaded_files))) {
    file_name <- basename(uploaded_files$name[i])

    file.copy(
      uploaded_files$datapath[i],
      file.path(shp_dir, file_name),
      overwrite = TRUE
    )
  }

  ## 4. Find the .shp file -----------------------
  shp_files <- list.files(
    shp_dir,
    pattern = "\\.shp$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(shp_files) == 0) {
    stop(
      "No .shp file was found. Please upload the complete shapefile."
    )
  }

  if (length(shp_files) > 1) {
    stop(
      "More than one .shp file was uploaded. Please upload only one shapefile."
    )
  }

  shp_path <- shp_files[1]

  ## 5. Check required shapefile components --------------

  shp_base <- tools::file_path_sans_ext(shp_path)

  required_files <- c(
    paste0(shp_base, ".shp"),
    paste0(shp_base, ".shx"),
    paste0(shp_base, ".dbf")
  )

  if (!all(file.exists(required_files))) {
    stop(
      "The shapefile is incomplete. Please upload the .shp, .shx and .dbf files together."
    )
  }

  ## 6. Read shapefile using its ORIGINAL CRS ------------------
  shp <- tryCatch(
    {
      terra::vect(shp_path)
    },
    error = function(e) {
      stop(
        paste0(
          "The shapefile could not be read. ",
          "Please make sure the .shp, .shx and .dbf files belong to the same shapefile."
        )
      )
    }
  )

  if (is.null(shp) || nrow(shp) == 0) {
    stop("The uploaded shapefile contains no features.")
  }

  ## 7. Make sure the original CRS is defined -----------------

  original_crs <- terra::crs(shp)

  if (
    is.na(original_crs) ||
      !nzchar(original_crs)
  ) {
    stop(
      paste0(
        "The uploaded shapefile does not have a valid CRS. ",
        "Please include the correct .prj file."
      )
    )
  }

  ## 8. Convert to WGS84 FIRST -------------------------

  shp_wgs84 <- tryCatch(
    {
      terra::project(
        shp,
        target_crs
      )
    },
    error = function(e) {
      stop(
        paste0(
          "The shapefile could not be converted to WGS84 (EPSG:4326). ",
          "Please check that the .prj file correctly describes the original CRS."
        )
      )
    }
  )

  ## 9. Confirm that the result is WGS84 -------------
  if (
    !grepl(
      "4326|WGS.?84|longlat",
      terra::crs(shp_wgs84),
      ignore.case = TRUE
    )
  ) {
    stop(
      "The uploaded shapefile could not be confirmed as WGS84 (EPSG:4326)."
    )
  }

  # 10. Create the allowed WGS84 analysis domain and check

  domain_poly <- terra::as.polygons(
    terra::ext(
      xmi,
      xmx,
      ymi,
      ymx
    ),
    crs = target_crs
  )

  # Check ACTUAL geometry overlap with the domain
  # A shapefile is accepted if at least part of its geometry
  # overlaps the allowed domain.
  has_overlap <- tryCatch(
    {
      test_intersection <- terra::intersect(
        shp_wgs84,
        domain_poly
      )

      nrow(test_intersection) > 0 &&
        any(!terra::is.empty(test_intersection))
    },
    error = function(e) {
      stop(
        paste0(
          "Could not determine whether the shapefile overlaps ",
          "the Western North America domain."
        )
      )
    }
  )

  if (!has_overlap) {
    stop(
      paste0(
        "The uploaded shapefile does not overlap the Western North America domain. ",
        "Allowed longitude: ",
        xmi,
        " to ",
        xmx,
        " and latitude: ",
        ymi,
        " to ",
        ymx,
        "."
      )
    )
  }

  # 12. Return the shapefile in WGS84
  shp_wgs84
}

# Apply selected location to a raster ------------------------------------------
# User points extract the raster cell containing the uploaded point
# and return a raster with values only in that cell.

apply_location_to_raster <- function(r, location) {
  ## USER-PROVIDED POINT ------------------------------

  if (location$type == "point") {
    # Make sure the uploaded location is a SpatVector

    if (!inherits(location$data, "SpatVector")) {
      stop(
        "The uploaded point is not a valid terra spatial point."
      )
    }
    # Make sure it contains exactly one point

    if (
      terra::geomtype(location$data)[1] != "points" ||
        nrow(location$data) != 1
    ) {
      stop(
        "The uploaded location must contain exactly one point."
      )
    }
    # Make sure point and raster use the same CRS

    point_crs <- terra::crs(location$data)
    raster_crs <- terra::crs(r)

    if (
      is.na(point_crs) ||
        point_crs == ""
    ) {
      stop(
        "The uploaded point does not have a valid coordinate reference system."
      )
    }

    # Convert point to raster CRS if necessary
    point_for_raster <- location$data

    if (
      !terra::same.crs(
        location$data,
        r
      )
    ) {
      point_for_raster <- tryCatch(
        terra::project(
          location$data,
          raster_crs
        ),

        error = function(e) {
          stop(
            "The uploaded point could not be converted to the climate raster projection."
          )
        }
      )
    }

    # Extract raster value AND cell number
    ext_val <- tryCatch(
      terra::extract(
        r,
        point_for_raster,
        cells = TRUE,
        ID = FALSE
      ),

      error = function(e) {
        stop(
          paste0(
            "The uploaded point could not be extracted ",
            "from the climate raster."
          )
        )
      }
    )

    ## Check extraction result ------------------

    if (
      is.null(ext_val) ||
        nrow(ext_val) != 1
    ) {
      stop(
        "No climate-data cell could be found at the uploaded point."
      )
    }

    # ----------------------------------------------------------
    # Get raster cell number

    cell <- suppressWarnings(
      as.integer(ext_val$cell[1])
    )

    if (
      is.na(cell) ||
        cell < 1 ||
        cell > terra::ncell(r)
    ) {
      stop(
        "The uploaded point does not correspond to a valid raster cell."
      )
    }

    # Get climate values

    vals <- ext_val[
      1,
      setdiff(
        names(ext_val),
        "cell"
      ),
      drop = FALSE
    ]

    # Check for missing climate data

    if (
      ncol(vals) == 0 ||
        all(
          is.na(
            as.numeric(vals[1, ])
          )
        )
    ) {
      stop(
        "No climate-data value is available at the uploaded point."
      )
    }

    # Create output raster

    out <- r

    # Get all raster values as matrix
    out_vals <- terra::values(
      out,
      mat = TRUE
    )

    # Set all cells to NA
    out_vals[,] <- NA_real_

    # Put extracted values into selected cell
    out_vals[
      cell,
      seq_len(ncol(vals))
    ] <- as.numeric(
      vals[1, ]
    )

    # Put values back into raster
    terra::values(out) <- out_vals

    return(out)
  }

  # ============================================================
  # EXISTING POLYGON WORKFLOW
  # ============================================================

  r |>
    terra::crop(
      location$data,
      snap = "out"
    ) |>
    terra::mask(
      location$data,
      touches = TRUE
    )
}

add_user_point_marker <- function(p, location) {
  if (location$type == "point") {
    xy <- terra::crds(location$data)
    p +
      ggplot2::geom_point(
        data = data.frame(x = xy[1, 1], y = xy[1, 2]),
        ggplot2::aes(x = x, y = y),
        colour = "red",
        size = 3.5,
        shape = 21,
        fill = "white",
        stroke = 1.2,
        inherit.aes = FALSE
      )
  } else {
    p
  }
}

location_label <- function(location) {
  if (location$type == "point") {
    xy <- terra::crds(location$data)[1, ]
    return(paste0(
      "User point (",
      round(abs(xy[1]), 4),
      "°W, ",
      round(xy[2], 4),
      "°N)"
    ))
  }
  "User shapefile"
}

# Existing shape files
na_shp <- get_shp(shp_fls_lst, "north_america")
wna_shp <- project(crop(na_shp, ext(xmi, xmx, ymi, ymx)), target_crs)

bc_shp <- project(get_shp(shp_fls_lst, "bc_shapefile"), target_crs)

bc_ecoprv_shp <- get_shp(shp_fls_lst, "bc_ecoprovince") %>%
  filter(code != 'NEP') %>%
  project(target_crs)

bc_ecorgn_shp <- project(get_shp(shp_fls_lst, "bc_ecoregions"), target_crs)
bc_ecosec_shp <- project(get_shp(shp_fls_lst, "bc_ecosections"), target_crs)

bc_flp_shp <- get_shp(shp_fls_lst, "flp")
bc_flp_shp$flp_unit_nam <- paste0('FLP- ', bc_flp_shp$ORG_UNIT)
bc_flp_shp <- project(bc_flp_shp, target_crs)

bc_wtrshd_shp <- project(get_shp(shp_fls_lst, "bc_watersheds"), target_crs)
bc_fwa_shp <- project(get_shp(shp_fls_lst, "fwa_watersheds"), target_crs)
bc_muni_shp <- project(get_shp(shp_fls_lst, "bc_municipalities"), target_crs)

# Parameters & Date Range Settings report ------------------------------------------
months_nam <- c(
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

parameters <- c("tmean", "tmax", "tmin", "prcp", "vpd", "rh", "soil_moisture")

years <- seq(min_year, max_year, 1)
yr_choices <- sort(years, decreasing = TRUE)

report_years <- seq(2023, max_year, 1)

# Anomaly & Climatology Data Cataloging -------------------------------------
list.files(
  path = ano_dt_pth,
  pattern = ".nc",
  full.names = T
) -> ano_clm_trn_dt_fls
ano_clm_trn_dt_fls

ano_clm_trn_dt_fl <- tibble(dt_pth = ano_clm_trn_dt_fls) %>%
  mutate(fl_nam = basename(dt_pth)) %>%
  mutate(
    par = str_extract(fl_nam, paste(parameters, collapse = "|")),
    dt_type = str_extract(fl_nam, "(ano|clm|spatial_trend)"), # ano, clm, spatial_trend
    mon = str_extract(
      fl_nam,
      "(annual|fall|summer|winter|spring|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
    ),
    start_year = str_extract(fl_nam, "(19|20)\\d{2}") # 1950 or 1980
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

# Report Suffix Construction ------------------------------------------------
month_labs <- c(
  "January" = "jan",
  "February" = "feb",
  "March" = "mar",
  "April" = "apr",
  "May" = "may",
  "June" = "jun",
  "July" = "jul",
  "August" = "aug",
  "September" = "sep",
  "October" = "oct",
  "November" = "nov",
  "December" = "dec"
)

upd_mon_lab <- month_labs[[update_month]]
upd_year <- as.integer(update_year)

build_monthly_suffixes <- function(start_year, end_year, start_month) {
  months_order <- names(month_labs)
  suffixes <- c()

  for (yr in seq(end_year, start_year, by = -1)) {
    mons <- if (yr == end_year) {
      months_order[1:match(start_month, months_order)]
    } else {
      months_order
    }

    for (m in rev(mons)) {
      suffixes <- c(suffixes, paste0(month_labs[[m]], yr))
    }
  }
  suffixes
}

monthly_suffixes <- build_monthly_suffixes(
  start_year = 2023,
  end_year = upd_year,
  start_month = update_month
)

start_ann_yr <- 2024
end_ann_yr <- upd_year - 1

annual_suffixes <- if (end_ann_yr >= start_ann_yr) {
  paste0("ann", seq(end_ann_yr, start_ann_yr, by = -1))
} else {
  character(0)
}

report_suffixes <- c(
  monthly_suffixes,
  annual_suffixes,
  "longterm"
)

# UI  --------------------------------------
# Reusable Footer Module for BC Gov styling
bcgov_footer <- function() {
  column(
    width = 12,
    style = "background-color:#003366; border-top:2px solid #fcba19; position:relative; margin-top:20px;",
    tags$footer(
      class = "footer",
      tags$div(
        class = "container",
        style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
        tags$ul(
          style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
          tags$li(a(
            href = "https://www2.gov.bc.ca/gov/content/home",
            "Home",
            style = "font-size:1em; font-weight:normal; color:white; padding:0 5px; border-right:1px solid #4b5e7e;"
          )),
          tags$li(a(
            href = "https://www2.gov.bc.ca/gov/content/home/disclaimer",
            "Disclaimer",
            style = "font-size:1em; font-weight:normal; color:white; padding:0 5px; border-right:1px solid #4b5e7e;"
          )),
          tags$li(a(
            href = "https://www2.gov.bc.ca/gov/content/home/privacy",
            "Privacy",
            style = "font-size:1em; font-weight:normal; color:white; padding:0 5px; border-right:1px solid #4b5e7e;"
          )),
          tags$li(a(
            href = "https://www2.gov.bc.ca/gov/content/home/accessibility",
            "Accessibility",
            style = "font-size:1em; font-weight:normal; color:white; padding:0 5px; border-right:1px solid #4b5e7e;"
          )),
          tags$li(a(
            href = "https://www2.gov.bc.ca/gov/content/home/copyright",
            "Copyright",
            style = "font-size:1em; font-weight:normal; color:white; padding:0 5px; border-right:1px solid #4b5e7e;"
          )),
          tags$li(a(
            href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html",
            "Contact",
            style = "font-size:1em; font-weight:normal; color:white; padding:0 5px;"
          ))
        )
      )
    )
  )
}

# UI
ui <- fluidPage(
  useShinyjs(),
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
            "<h3><b>BC climate anomaly app</b>: Visualizing Climate Anomalies in British Columbia (BC)</h3>"
          )
        ),
        includeMarkdown("intro_bc_climate_anomaly_app.Rmd"),
        column(
          width = 12,
          HTML(
            "<h4><b>Citation</b></h4>
             <h5><u>Please cite the contents of this app as:</u><br>
             Sharma, A.R. 2023. BC climate anomaly app: Visualizing monthly, seasonal, and annual climate anomalies in British Columbia (BC).
             British Columbia Ministry of Forests.
             <a href='https://bcgov-env.shinyapps.io/bc_climate_anomaly/' target='_blank'>https://bcgov-env.shinyapps.io/bc_climate_anomaly/</a></h5>"
          )
        ),
        column(
          width = 12,
          HTML(
            "<h5><u>App created by:</u><br>
             <b>Aseem R. Sharma, PhD</b><br>
             Research Climatologist<br>
             FFEC, FEA, OCF, BC Ministry of Forests<br>
             <a href='mailto:Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a><br><br>
             <h4><b>Code</b></h4>
             <h5>The code and data of this app are available through GitHub at <a href='https://github.com/bcgov/bc_climate_anomaly.git' target='_blank'>https://github.com/bcgov/bc_climate_anomaly</a>.</h5>"
          )
        ),
        column(
          width = 12,
          HTML(
            "<h5><b>Disclaimer</b></h5>
             <p>This app and the climate reports here have been prepared using <a href='https://www.ecmwf.int/en/era5-land' target='_blank'>ERA5-Land</a> data
             from the European Centre for Medium-Range Weather Forecasts (ECMWF), as available at the time of preparation.
             Please note that the original data may be subject to updates or revisions. Any modifications to the original data may result in adjustments to the findings presented in this report.</p>"
          )
        ),
        column(width = 12, textOutput("deploymentDate")),
        bcgov_footer()
      )
    ),

    ## About page ---------------------
    tabPanel(
      title = "About",
      value = "about",
      withMathJax(includeMarkdown("about_bc_climate_anomaly_app.Rmd")),
      bcgov_footer()
    ),

    ## Anomaly App page ----------------------------
    tabPanel(
      title = "Anomaly app",
      value = "ano_app",
      sidebarLayout(
        sidebarPanel(
          id = "selection-panel",
          width = 3,

          # =========================================================
          # COMPACT SIDEBAR CSS
          # =========================================================
          tags$head(
            tags$style(HTML(
              "
        #selection-panel {
          padding-top: 5px;
          padding-bottom: 5px;
        }

        #selection-panel .form-group {
          margin-top: 0px;
          margin-bottom: 5px;
        }

        #selection-panel .help-block {
          margin-top: 2px;
          margin-bottom: 3px;
          line-height: 1.15;
        }

        #selection-panel p {
          margin-top: 2px;
          margin-bottom: 4px;
          line-height: 1.2;
        }

        /* All four selection headings at the same level */
        #selection-panel h4 {
          margin-top: 5px;
          margin-bottom: 4px;
          line-height: 1.1;
        }

        #selection-panel h5 {
          margin-top: 3px;
          margin-bottom: 3px;
          line-height: 1.1;
        }

        #selection-panel hr {
          margin-top: 5px;
          margin-bottom: 5px;
        }

        #selection-panel .btn {
          margin-top: 0px;
          margin-bottom: 2px;
          padding-top: 5px;
          padding-bottom: 5px;
        }

        #selection-panel .radio-inline {
          margin-top: 0px;
          margin-bottom: 0px;
        }

        #selection-panel .radio {
          margin-top: 2px;
          margin-bottom: 2px;
        }

        #selection-panel .form-control {
          padding-top: 4px;
          padding-bottom: 4px;
          height: 32px;
        }

        #selection-panel .row {
          margin-bottom: 0px;
        }

        select.form-control {
          transition: background-color 0.3s ease;
        }

        select.form-control:focus {
          background-color: #d4edda !important;
        }

        .selectize-dropdown .option {
          border-bottom: 1px solid #ccc;
          padding: 5px 8px;
        }

        .flash-text {
          color: red;
          animation: flash 1s infinite;
        }

        @keyframes flash {
          0%   { opacity: 1; }
          50%  { opacity: 0; }
          100% { opacity: 1; }
        }

        #selection-panel #loc_map {
          margin-top: 0px;
        }

        #selection-panel .well {
          padding: 5px 8px;
          margin-bottom: 0px;
        }
        "
            ))
          ),

          # =========================================================
          # MAIN HEADING
          # =========================================================

          helpText(
            HTML("<h4><b>Filter/Selections</b></h4>")
          ),

          helpText(
            HTML(
              '<p>Select an area, climate variable, and period, then click <b><i class="flash-text">Run Analysis</i></b>.</p>'
            )
          ),

          tags$hr(),

          # =========================================================
          # 1. AREA
          # =========================================================
          helpText(
            HTML(
              "<h4><b>1. Area of Interest</b></h4>"
            )
          ),

          # Select an area
          helpText(
            HTML("<h5><b>Select an area OR</b></h5>")
          ),
          HTML(
            "<small>(Western North America, BC, Eco-provinces/regions/sections, Major Watersheds, FWA watersheds, FLP boundaries, Municipalities)</small>"
          ),

          pickerInput(
            "major_area",
            NULL,
            choices = c(
              "Western North America",
              "BC",
              "Ecoprovinces",
              "Ecoregions",
              "Ecosections",
              "Major watersheds",
              "FWA watersheds",
              "FLP boundaries",
              "Municipalities"
            ),
            selected = "BC"
          ),

          hidden(
            pickerInput(
              "ecoprov_area",
              "Ecoprovinces",
              choices = c(
                "Ecoprovinces (select one)",
                bc_ecoprv_shp$name
              ),
              multiple = FALSE
            )
          ),

          hidden(
            pickerInput(
              "ecorgn_area",
              "Ecoregions",
              choices = c(
                "Ecoregions (select one)",
                bc_ecorgn_shp$CRGNNM
              ),
              multiple = FALSE
            )
          ),

          hidden(
            pickerInput(
              "ecosec_area",
              "Ecosections",
              choices = c(
                "Ecosections (select one)",
                bc_ecosec_shp$ECOSEC_NM
              ),
              multiple = FALSE
            )
          ),

          hidden(
            selectInput(
              "wtrshd_area",
              "Watershed",
              choices = c(
                "Major watersheds (select one)",
                bc_wtrshd_shp$MJR_WTRSHM
              ),
              multiple = FALSE
            )
          ),

          hidden(
            selectInput(
              "fwa_area",
              "FWA watersheds",
              choices = c(
                "FWA watersheds (select one)",
                bc_fwa_shp$WATERSHE_2
              ),
              multiple = FALSE
            )
          ),

          hidden(
            selectInput(
              "flp_area",
              "FLP boundaries",
              choices = c(
                "FLP boundaries (select one)",
                bc_flp_shp$flp_unit_nam
              ),
              multiple = FALSE
            )
          ),

          hidden(
            pickerInput(
              "muni_area",
              "Municipalities",
              choices = c(
                "Municipalities (select one)",
                bc_muni_shp$ABRVN
              ),
              multiple = FALSE
            )
          ),
          # User input
          helpText(
            HTML("<h5><b>Provide your own location</b></h5>")
          ),

          radioButtons(
            "user_location_type",
            NULL,
            choices = c(
              "None" = "none",
              "Shapefile" = "shp",
              "Point" = "point"
            ),
            selected = "none",
            inline = TRUE
          ),

          conditionalPanel(
            condition = "input.user_location_type == 'shp'",
            fileInput(
              "user_shp",
              "Upload shapefile",
              multiple = TRUE,
              accept = c(
                ".zip",
                ".shp",
                ".shx",
                ".dbf",
                ".prj",
                ".cpg"
              )
            ),
            helpText(
              "Upload a ZIP or complete shapefile (.shp, .shx, .dbf, .prj)."
            )
          ),

          conditionalPanel(
            condition = "input.user_location_type == 'point'",
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "user_lon",
                  "Longitude",
                  value = NULL,
                  min = -180,
                  max = 180,
                  step = 0.0001,
                  width = "100%"
                )
              ),
              column(
                width = 6,
                numericInput(
                  "user_lat",
                  "Latitude",
                  value = NULL,
                  min = -90,
                  max = 90,
                  step = 0.0001,
                  width = "100%"
                )
              )
            ),
            helpText(
              "Point must be within -140 to -108°W, 39 to 60°N."
            )
          ),

          uiOutput("user_location_status"),

          tags$hr(),

          # Section 2
          helpText(
            HTML("<h4><b>2. Climate variable</b></h4>")
          ),

          uiOutput("par_picker"),

          HTML(
            "<small>
    (Temperature, VPD, Precipitation, RH, Soil moisture)
  </small>"
          ),

          tags$hr(),

          # Section 3
          helpText(
            HTML("<h4><b>3. Month, season, or annual</b></h4>")
          ),

          uiOutput("month_picker"),

          tags$hr(),

          # Section 4
          helpText(
            HTML("<h4><b>4. Range of years or specific year(s)</b></h4>")
          ),

          actionButton("rng_years_choose", "Range of years"),
          actionButton("ab_years_choose", "Specific year(s)"),

          sliderInput(
            "year_range",
            "Year range",
            min_year,
            max_year,
            value = c((max_year - 5), max_year),
            sep = ""
          ),

          chooseSliderSkin(skin = "Shiny"),

          hidden(
            selectInput(
              "year_specific",
              "Year(s)",
              choices = yr_choices,
              multiple = TRUE,
              selected = max_year
            )
          ),

          tags$hr(),

          # =========================================================
          # RUN ANALYSIS + RESET
          # =========================================================

          fluidRow(
            column(
              width = 8,

              actionButton(
                "run_ana_button",
                tags$b(
                  tags$span(
                    style = "color: red;",
                    "Run Analysis"
                  )
                ),
                width = "100%"
              )
            ),

            column(
              width = 4,

              actionButton(
                "reset_input",
                "Reset",
                width = "100%"
              )
            )
          ),

          # =========================================================
          # LOCATION MAP
          # =========================================================

          fluidRow(
            column(
              width = 12,

              HTML(
                "<h4><b>Location Map</b></h4>"
              ),

              withSpinner(
                leafletOutput(
                  "loc_map",
                  height = "20vh"
                ),
                type = 6
              )
            )
          ),

          # =========================================================
          # CEI LINK
          # =========================================================

          fluidRow(
            column(
              width = 12,

              wellPanel(
                style = "
            background-color: white;
            padding: 5px 8px;
            margin-bottom: 0px;
          ",

                HTML(
                  '<h4 style="margin: 2px 0;">
              For climate extreme indices (CEI) refer to
              <a href="https://bcgov-env.shinyapps.io/bc_climate_extremes_app/" target="_blank">
                <b>bc_climate_extremes_app</b>
              </a>
            </h4>'
                )
              )
            )
          )
        ),
        mainPanel(
          width = 9,
          column(
            width = 10,
            wellPanel(HTML(
              "<h4><b>Time series, linear trends and spatial anomaly maps</b></h4>"
            ))
          ),
          fluidRow(
            column(
              width = 12,
              tabBox(
                width = 12,
                tabPanel(
                  title = "Time-series plot",
                  status = "primary",
                  withSpinner(
                    plotlyOutput("lnr_trn_plt", height = "60vh"),
                    type = 6
                  ),
                  downloadButton("download_lnr_trn_plt", "Download plot"),
                  downloadButton(
                    "download_ano_ts_data",
                    "Download anomaly time series data"
                  )
                ),
                tabPanel(
                  title = "Spatial anomaly maps",
                  status = "primary",
                  withSpinner(
                    plotOutput("sptl_ano_map", height = "70vh"),
                    type = 6
                  ),
                  downloadButton("download_sptl_ano_plt", "Download plot"),
                  downloadButton(
                    "download_sptl_ano_data",
                    "Download raster data"
                  )
                )
              )
            )
          ),

          ## Climate normals & spatial trends
          fluidRow(
            box(
              width = 4,
              align = "left",
              wellPanel(HTML("<h5><b>Climate Normal (1981-2010)</b></h5>")),
              uiOutput("clm_nor_title", height = "30vh"),
              withSpinner(
                plotOutput("clm_nor_map", width = "100%", height = "30vh"),
                type = 6
              ),
              downloadButton("download_clm_nor_plt", "Download plot"),
              downloadButton("download_clm_nor_data", "Download raster data")
            ),
            box(
              width = 4,
              align = "left",
              wellPanel(HTML("<h5><b>Spatial trends since 1950</b></h5>")),
              uiOutput("clm_trn50_title", height = "30vh"),
              withSpinner(
                plotOutput("clm_trn50_map", width = "100%", height = "30vh"),
                type = 6
              ),
              downloadButton("download_clm_trn50_plt", "Download plot"),
              downloadButton("download_clm_trn50_data", "Download raster data")
            ),
            box(
              width = 4,
              align = "left",
              wellPanel(HTML("<h5><b>Spatial trends since 1980</b></h5>")),
              uiOutput("clm_trn80_title", height = "30vh"),
              withSpinner(
                plotOutput("clm_trn80_map", width = "100%", height = "30vh"),
                type = 6
              ),
              downloadButton("download_clm_trn80_plt", "Download plot"),
              downloadButton("download_clm_trn80_data", "Download raster data")
            )
          ),

          column(
            width = 12,
            HTML(
              "<h5><b>Disclaimer:</b></h5><p>This analysis utilizes ERA5-Land data. Any modifications to the dataset or discrepancies in the results due to data changes should be carefully considered by users.</p>"
            )
          )
        )
      ),
      bcgov_footer()
    ),

    ## Reports page -----------------------------
    tabPanel(
      title = "Reports",
      value = "report",
      column(
        width = 12,
        wellPanel(
          HTML(
            "<h3><b>BC climate summary and anomaly reports</b></h3>
                <h4>Monthly summaries, annual reports, and long-term trends (HTML)</h4>"
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary",
            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 24px;",
              lapply(unique(report_years), function(yr) {
                tags$div(
                  style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px; background-color: #fafafa;",
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
      bcgov_footer()
    ),

    ## Climate Stripes -----------------------
    tabPanel(
      title = "Climate stripes",
      value = "clm_stripes",
      column(
        width = 12,
        wellPanel(HTML("<h3><b>BC climate stripes</b></h3>")),
        HTML(
          "<h5>Inspired by the work of British climate scientist <a href='https://showyourstripes.info/' target='_blank'>Prof. Ed Hawkins</a>,
           the climate stripes (also known as warming stripes) visually represent changes in annual temperatures relative to the long-term average.<br>
           Below are the 'climate stripes' plots for British Columbia (BC) since 1950. Each stripe corresponds to a single year's temperature compared to the 1981–2010 average.
           Red stripes indicate warmer-than-average years, while blue stripes represent cooler-than-average years. The intensity of the color reflects the magnitude of the difference from the average.<br><br>
           Feel free to download and use these visuals!<br><br></h5>"
        ),
        fluidRow(
          wellPanel(HTML(
            "<h3><b>BC climate stripes (mean temperature): with title</b></h3>"
          )),
          box(
            width = 12,
            height = "100vh",
            status = "primary",
            downloadButton(
              "clm_strp_plt_ttl_dnwld",
              "Download climate stripe plot with title"
            ),
            imageOutput("bc_clm_strp_withtitle")
          )
        ),
        fluidRow(
          wellPanel(HTML(
            "<h3><b>BC climate stripes (mean temperature): without title</b></h3>"
          )),
          box(
            width = 12,
            height = "100vh",
            status = "primary",
            downloadButton(
              "clm_strp_plt_wttl_dnwld",
              "Download climate stripe plot without title"
            ),
            imageOutput("bc_clm_strp_withouttitle")
          )
        )
      ),
      bcgov_footer()
    ),

    ## Feedback and links --------------------------
    tabPanel(
      title = "Feedback & Links",
      value = "feed_link",
      column(
        width = 12,
        wellPanel(HTML("<h3><b>Feedback</b></h3>")),
        fluidRow(
          box(width = 12, status = "primary", uiOutput("feedback_text"))
        )
      ),
      column(
        width = 12,
        wellPanel(HTML("<h4><b>Links to other apps</b></h4>")),
        HTML(
          "<h5><b>Here are the links to other apps developed in FFEC:</b></h5>
           <a href='https://bcgov-env.shinyapps.io/cmip6-BC/' target='_blank'>CMIP6-BC</a><br>
           <a href='https://bcgov-env.shinyapps.io/bc_climate_extremes_app/' target='_blank'>BC_climate_extremes_app</a><br><br>"
        )
      ),
      bcgov_footer()
    )
  )
)

# Server ----
server <- function(session, input, output) {
  options(warn = -1)

  ## User-defined location validation ------------------------------------------

  user_location <- reactiveVal(NULL)
  user_location_error <- reactiveVal(NULL)

  observeEvent(
    list(
      input$user_location_type,
      input$user_shp,
      input$user_lon,
      input$user_lat
    ),
    {
      user_location(NULL)
      user_location_error(NULL)

      # Do nothing until a location type has been selected
      if (
        is.null(input$user_location_type) ||
          input$user_location_type == "none"
      ) {
        return()
      }

      # =============================================================
      # SHAPEFILE
      # =============================================================

      # If Shapefile is selected, wait until a file is uploaded
      if (
        input$user_location_type == "shp" &&
          (is.null(input$user_shp) || nrow(input$user_shp) == 0)
      ) {
        return()
      }

      ## ENTER POINT --------------------

      if (input$user_location_type == "point") {
        # IMPORTANT:
        # Wait until BOTH longitude and latitude have been entered.
        # Do not show an error while the user is still entering them.

        if (
          is.null(input$user_lon) ||
            is.null(input$user_lat) ||
            is.na(input$user_lon) ||
            is.na(input$user_lat)
        ) {
          return()
        }

        # Get coordinates
        lon <- as.numeric(input$user_lon)
        lat <- as.numeric(input$user_lat)

        # Check that coordinates are valid numbers
        if (
          !is.finite(lon) ||
            !is.finite(lat)
        ) {
          user_location_error(
            "Longitude and latitude must be valid numeric values."
          )

          showNotification(
            "Longitude and latitude must be valid numeric values.",
            type = "warning",
            duration = 8
          )

          return()
        }

        # Check WNA domain
        if (
          lon < -140 ||
            lon > -108 ||
            lat < 39 ||
            lat > 60
        ) {
          msg <- paste0(
            "The point is outside the analysis domain. ",
            "Longitude must be between -140 and -108°W, ",
            "and latitude must be between 39 and 60°N."
          )

          user_location_error(msg)

          showNotification(
            msg,
            type = "warning",
            duration = 8
          )

          return()
        }

        # -----------------------------------------------------------
        # Create spatial point
        # -----------------------------------------------------------
        result <- tryCatch(
          {
            point <- terra::vect(
              data.frame(
                longitude = lon,
                latitude = lat
              ),
              geom = c("longitude", "latitude"),
              crs = "EPSG:4326"
            )

            list(
              type = "point",
              data = point,
              label = paste0(
                "Point (",
                round(lon, 4),
                ", ",
                round(lat, 4),
                ")"
              ),
              message = paste0(
                "Valid point: longitude ",
                round(lon, 4),
                ", latitude ",
                round(lat, 4)
              )
            )
          },

          error = function(e) {
            list(
              error = conditionMessage(e)
            )
          }
        )

        # -----------------------------------------------------------
        # Store point validation result
        # -----------------------------------------------------------
        if (!is.null(result$error)) {
          user_location_error(result$error)

          showNotification(
            result$error,
            type = "warning",
            duration = 8
          )
        } else {
          user_location(result)

          showNotification(
            result$message,
            type = "message",
            duration = 6
          )
        }

        return()
      }

      # =============================================================
      # SHAPEFILE PROCESSING
      # =============================================================

      result <- tryCatch(
        {
          if (input$user_location_type == "shp") {
            shp <- read_user_shapefile(
              input$user_shp,
              target_crs = "EPSG:4326"
            )

            geom_types <- unique(
              terra::geomtype(shp)
            )

            if (
              !all(
                geom_types %in%
                  c(
                    "polygons",
                    "multipolygons",
                    "points",
                    "lines"
                  )
              )
            ) {
              stop(
                "The uploaded shapefile contains unsupported geometry."
              )
            }

            # -------------------------------------------------------
            # Point shapefile
            # -------------------------------------------------------
            if (all(terra::geomtype(shp) == "points")) {
              if (nrow(shp) != 1) {
                stop(
                  "The custom point shapefile must contain exactly one point."
                )
              }

              loc <- list(
                type = "point",
                data = shp
              )

              list(
                type = "point",
                data = shp,
                label = location_label(loc),
                message = paste0(
                  "Valid one-point shapefile. ",
                  location_label(loc)
                )
              )

              # -------------------------------------------------------
              # Polygon shapefile
              # -------------------------------------------------------
            } else if (
              all(
                terra::geomtype(shp) %in%
                  c(
                    "polygons",
                    "multipolygons"
                  )
              )
            ) {
              # -----------------------------------------------------
              # Get uploaded shapefile name without .shp extension
              # -----------------------------------------------------
              shp_name <- input$user_shp$name[
                tolower(
                  tools::file_ext(input$user_shp$name)
                ) ==
                  "shp"
              ][1]

              shp_name <- tools::file_path_sans_ext(
                basename(shp_name)
              )

              # -----------------------------------------------------
              # Return validated polygon shapefile
              # -----------------------------------------------------
              list(
                type = "polygon",
                data = shp,
                label = shp_name,
                message = paste0(
                  "Valid shapefile: ",
                  shp_name
                )
              )
            } else {
              stop(
                paste0(
                  "The custom shapefile must contain polygon ",
                  "features or exactly one point."
                )
              )
            }
          } else {
            stop(
              "Unknown user location type."
            )
          }
        },

        error = function(e) {
          list(
            error = conditionMessage(e)
          )
        }
      )

      # -------------------------------------------------------------
      # Show validation result
      # -------------------------------------------------------------
      if (!is.null(result$error)) {
        user_location_error(
          result$error
        )

        showNotification(
          result$error,
          type = "warning",
          duration = 8
        )
      } else {
        user_location(
          result
        )

        showNotification(
          result$message,
          type = "message",
          duration = 6
        )
      }
    },

    ignoreInit = TRUE
  )

  observeEvent(
    input$user_location_type,
    {
      if (
        is.null(input$user_location_type) || input$user_location_type == "none"
      ) {
        shinyjs::enable("major_area")
        shinyjs::enable("ecoprov_area")
        shinyjs::enable("ecorgn_area")
        shinyjs::enable("ecosec_area")
        shinyjs::enable("wtrshd_area")
        shinyjs::enable("fwa_area")
        shinyjs::enable("flp_area")
        shinyjs::enable("muni_area")
      } else {
        shinyjs::disable("major_area")
        shinyjs::disable("ecoprov_area")
        shinyjs::disable("ecorgn_area")
        shinyjs::disable("ecosec_area")
        shinyjs::disable("wtrshd_area")
        shinyjs::disable("fwa_area")
        shinyjs::disable("flp_area")
        shinyjs::disable("muni_area")
      }
    },
    ignoreInit = FALSE
  )

  output$user_location_status <- renderUI({
    err <- user_location_error()
    loc <- user_location()

    if (!is.null(err)) {
      div(
        style = "color:#a94442; background:#f2dede; border:1px solid #ebccd1; padding:8px; border-radius:4px;",
        tags$b("Invalid user location: "),
        err
      )
    } else if (!is.null(loc)) {
      div(
        style = "color:#155724; background:#d4edda; border:1px solid #c3e6cb; padding:8px; border-radius:4px;",
        tags$b("Location accepted: "),
        loc$message
      )
    } else if (
      !is.null(input$user_location_type) &&
        input$user_location_type == "shp"
    ) {
      div(
        style = "color:#856404; background:#fff3cd; border:1px solid #ffeeba; padding:8px; border-radius:4px;",
        "Upload the selected location file to validate it."
      )
    } else if (
      !is.null(input$user_location_type) &&
        input$user_location_type == "point"
    ) {
      div(
        style = "color:#856404; background:#fff3cd; border:1px solid #ffeeba; padding:8px; border-radius:4px;",
        "Enter longitude and latitude of the point of interest."
      )
    }
  })

  ## Resolve either the original region selection or the validated user location
  get_analysis_location <- reactive({
    if (
      !is.null(input$user_location_type) &&
        input$user_location_type != "none"
    ) {
      req(user_location())
      return(user_location())
    }

    req(input$major_area)
    list(type = "polygon", data = get_shapefile(), label = get_region())
  })

  # Maps and plots tab --------------------------------------------------------

  ## Dynamic UI Visibility: Region Selectors ----------------------------------
  observeEvent(input$major_area, {
    area_map <- list(
      "Ecoprovinces" = "ecoprov_area",
      "Ecoregions" = "ecorgn_area",
      "Ecosections" = "ecosec_area",
      "Major watersheds" = "wtrshd_area",
      "FWA watersheds" = "fwa_area",
      "FLP boundaries" = "flp_area",
      "Municipalities" = "muni_area"
    )

    target_id <- area_map[[input$major_area]]

    # Hide all regional pickers first
    lapply(area_map, hideElement)

    # Show selected regional picker if applicable
    if (!is.null(target_id)) {
      showElement(target_id)
    }
  })

  ## Shapefile Filtering Reactive --------------------------------------------
  get_shapefile <- reactive({
    req(input$major_area)

    switch(
      input$major_area,
      "Western North America" = wna_shp,
      "BC" = bc_shp,
      "Ecoprovinces" = if (input$ecoprov_area == "Ecoprovinces (select one)") {
        bc_shp
      } else {
        filter(bc_ecoprv_shp, name == input$ecoprov_area)
      },
      "Ecoregions" = if (input$ecorgn_area == "Ecoregions (select one)") {
        bc_shp
      } else {
        filter(bc_ecorgn_shp, CRGNNM == input$ecorgn_area)
      },
      "Ecosections" = if (input$ecosec_area == "Ecosections (select one)") {
        bc_shp
      } else {
        filter(bc_ecosec_shp, ECOSEC_NM == input$ecosec_area)
      },
      "Major watersheds" = if (
        input$wtrshd_area == "Major watersheds (select one)"
      ) {
        bc_shp
      } else {
        filter(bc_wtrshd_shp, MJR_WTRSHM == input$wtrshd_area)
      },
      "FWA watersheds" = if (input$fwa_area == "FWA watersheds (select one)") {
        bc_shp
      } else {
        filter(bc_fwa_shp, WATERSHE_2 == input$fwa_area)
      },
      "FLP boundaries" = if (input$flp_area == "FLP boundaries (select one)") {
        bc_shp
      } else {
        filter(bc_flp_shp, flp_unit_nam == input$flp_area)
      },
      "Municipalities" = if (input$muni_area == "Municipalities (select one)") {
        bc_shp
      } else {
        filter(bc_muni_shp, ABRVN == input$muni_area)
      },
      bc_shp
    )
  })

  ## Region Name Extraction ---------------------------------------------------
  get_region <- reactive({
    if (
      !is.null(input$user_location_type) &&
        input$user_location_type != "none"
    ) {
      req(user_location())
      return(user_location()$label)
    }

    req(input$major_area)

    switch(
      input$major_area,
      "BC" = "BC",
      "Western North America" = "Western North America",
      "Ecoprovinces" = input$ecoprov_area,
      "Ecoregions" = input$ecorgn_area,
      "Ecosections" = input$ecosec_area,
      "Municipalities" = input$muni_area,
      "Major watersheds" = input$wtrshd_area,
      "FWA watersheds" = input$fwa_area,
      "FLP boundaries" = input$flp_area,
      NULL
    )
  })

  ## Parameter & UI Selection Renderers ---------------------------------------
  output$par_picker <- renderUI({
    par_choices <- c(
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
      NULL,
      choices = par_choices,
      selected = "tmean"
    )
  })

  output$month_picker <- renderUI({
    mon_choices <- c(
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
      NULL,
      choices = mon_choices,
      selected = "annual"
    )
  })

  ## Interactive Year Selection State ---------------------------------------
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

  ## Helper Reactives for Variable Metadata ---------------------------------
  get_years <- reactive({
    if (whichInput$type == "specific") {
      input$year_specific
    } else {
      seq(input$year_range[1], input$year_range[2], 1)
    }
  })

  get_par_full <- reactive({
    req(input$par_picker)
    switch(
      input$par_picker,
      'tmin' = "minimum temperature",
      'tmax' = "maximum temperature",
      'tmean' = "mean temperature",
      'prcp' = "total precipitation",
      'rh' = "relative humidity (RH)",
      'vpd' = "vapor pressure deficit (VPD)",
      'soil_moisture' = "volumetric soil moisture (0-1m)",
      "unknown variable"
    )
  })

  get_unit <- reactive({
    req(input$par_picker)
    switch(
      input$par_picker,
      "tmax" = ,
      "tmin" = ,
      "tmean" = "°C",
      "prcp" = "mm",
      "rh" = "%",
      "vpd" = "kPa",
      "soil_moisture" = "m\U00B3/m",
      " "
    )
  })

  get_mon_full <- reactive({
    req(input$month_picker)
    month_lookup <- c(
      annual = "Annual",
      spring = "Spring",
      summer = "Summer",
      fall = "Fall",
      winter = "Winter",
      Jan = "January",
      Feb = "February",
      Mar = "March",
      Apr = "April",
      May = "May",
      Jun = "June",
      Jul = "July",
      Aug = "August",
      Sep = "September",
      Oct = "October",
      Nov = "November",
      Dec = "December"
    )
    month_lookup[[input$month_picker]] %||% "Unknown"
  })

  ## Reset Form ---------------------------------------------------------------
  observeEvent(input$reset_input, {
    shinyjs::reset("selection-panel")
    updateRadioButtons(session, "user_location_type", selected = "none")
    user_location(NULL)
    user_location_error(NULL)
  })

  ## Interactive Location Map ------------------------------------------------
  output$loc_map <- renderLeaflet({
    req(input$major_area)

    loc <- get_analysis_location()
    sel_area_shpfl <- loc$data
    lyr_id <- NULL

    if (loc$type == "point") {
      xy <- terra::crds(sel_area_shpfl)
      return(
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            lng = xy[1, 1],
            lat = xy[1, 2],
            radius = 7,
            color = "red",
            fillColor = "red",
            fillOpacity = 0.8,
            popup = location_label(loc)
          )
      )
    }

    if (
      input$major_area == "Major watersheds" &&
        input$wtrshd_area == "Major watersheds (select one)"
    ) {
      sel_area_shpfl <- bc_wtrshd_shp['MJR_WTRSHM']
      lyr_id <- "MJR_WTRSHM"
    } else if (
      input$major_area == "Ecoprovinces" &&
        input$ecoprov_area == "Ecoprovinces (select one)"
    ) {
      sel_area_shpfl <- bc_ecoprv_shp['name']
      lyr_id <- "name"
    } else if (
      input$major_area == "Ecoregions" &&
        input$ecorgn_area == "Ecoregions (select one)"
    ) {
      sel_area_shpfl <- bc_ecorgn_shp['CRGNNM']
      lyr_id <- "CRGNNM"
    } else if (
      input$major_area == "Ecosections" &&
        input$ecosec_area == "Ecosections (select one)"
    ) {
      sel_area_shpfl <- bc_ecosec_shp['ECOSEC_NM']
      lyr_id <- "ECOSEC_NM"
    } else if (
      input$major_area == "Municipalities" &&
        input$muni_area == "Municipalities (select one)"
    ) {
      sel_area_shpfl <- bc_muni_shp['ABRVN']
      lyr_id <- "ABRVN"
    } else if (
      input$major_area == "FLP boundaries" &&
        input$flp_area == "FLP boundaries (select one)"
    ) {
      sel_area_shpfl <- bc_flp_shp['flp_unit_nam']
      lyr_id <- "flp_unit_nam"
    }

    map_form <- if (!is.null(lyr_id)) as.formula(paste0("~", lyr_id)) else NULL

    leaflet(sel_area_shpfl) %>%
      addTiles() %>%
      addPolygons(
        layerId = map_form,
        popup = map_form,
        color = "Red",
        weight = 1,
        opacity = 1,
        fill = TRUE,
        fillOpacity = 0
      )
  })

  observeEvent(input$loc_map_shape_click, {
    req(is.null(input$user_location_type) || input$user_location_type == "none")
    nm <- input$loc_map_shape_click$id
    req(nm)

    switch(
      input$major_area,
      "Ecoprovinces" = updatePickerInput(
        session,
        "ecoprov_area",
        selected = nm
      ),
      "Ecoregions" = updateSelectInput(session, "ecorgn_area", selected = nm),
      "Ecosections" = updateSelectInput(session, "ecosec_area", selected = nm),
      "Major watersheds" = updateSelectInput(
        session,
        "wtrshd_area",
        selected = nm
      ),
      "FLP boundaries" = updateSelectInput(session, "flp_area", selected = nm),
      "Municipalities" = updateSelectInput(session, "muni_area", selected = nm)
    )
  })

  ## Main Calculation Reactive Trigger -------------------------------------
  ano_clm_trn_sel_dt_rct <- eventReactive(input$run_ana_button, {
    req(input$month_picker, input$par_picker, input$major_area)

    # # For sample run -----------------
    #     monn = "Jun"
    #     parr = "tmean"
    #     sel_yrs <- seq(1951, 2026, 1)
    #     sel_yrs
    #     sel_area_shpfl <- bc_shp
    #     sel_area_shpfl
    #     region = "BC"
    #     ano_clm_trn_dt_fl %>%
    #       filter(
    #         mon == monn &
    #           par == parr
    #       ) -> ano_clm_trn_dt_fl_mon
    #     ano_clm_trn_dt_fl_mon
    #     ano_dt_sel_rast <- rast(ano_clm_trn_dt_fl_mon$dt_pth[[1]])
    #     ano_dt_sel_rast
    #     terra::plot(ano_dt_sel_rast, 70:nlyr(ano_dt_sel_rast))
    #
    #     ano_dt_sel_rast_trn <- rast(ano_clm_trn_dt_fl_mon$dt_pth[[3]])
    #     plot(ano_dt_sel_rast_trn)
    #
    #     # end of sample run

    ano_clm_trn_dt_fl %>%
      filter(
        mon == input$month_picker &
          par == input$par_picker
      ) -> ano_clm_trn_dt_fl_mon

    # Apply either the selected built-in region or validated user location
    location <- get_analysis_location()
    # location <- bc_shp
    sel_area_shpfl <- location$data

    # other requirements
    monn = unique(ano_clm_trn_dt_fl_mon$mon)
    parr = unique(ano_clm_trn_dt_fl_mon$par)

    # Anomaly
    ano_clm_trn_dt_fl_mon %>%
      filter(dt_type == 'ano') -> ano_dt_fl_mon

    ano_dt_sel_rast <- rast(ano_dt_fl_mon$dt_pth)
    ano_dt_sel_rast
    # plot(ano_dt_sel_rast,1)
    yr_df <- tibble(paryr = names(ano_dt_sel_rast))
    yr_df %<>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    names(ano_dt_sel_rast) <- yr_df$yr
    terra::time(ano_dt_sel_rast) <- yr_df$yr

    # crop mask for selected area
    ano_dt_shp_rast <- apply_location_to_raster(ano_dt_sel_rast, location)
    # plot(ano_dt_shp_rast, 77)

    # Climatology
    ano_clm_trn_dt_fl_mon %>%
      filter(dt_type == 'clm') -> clm_dt_fl_mon

    clm_dt_sel_rast <- rast(clm_dt_fl_mon$dt_pth)
    clm_dt_sel_rast

    #crop for selected area
    clm_dt_shp_rast <- apply_location_to_raster(clm_dt_sel_rast, location)

    #calculate percentage for prcp and soil-moisture
    if (parr == 'prcp' | parr == 'soil_moisture') {
      ano_dt_shp_rast1 <- (ano_dt_shp_rast / clm_dt_shp_rast) * 100
      #If prcp anomalies are very high ( > 200 %) then convert and limit to 200.
      ano_dt_shp_rast2 <-
        ifel(ano_dt_shp_rast1 > 201, 200, ano_dt_shp_rast1)
      ano_dt_shp_rast3 <-
        ifel(ano_dt_shp_rast2 < -201, -200, ano_dt_shp_rast2)
      ano_dt_shp_rast <- ano_dt_shp_rast3
    } else {
      ano_dt_shp_rast <- ano_dt_shp_rast
    }
    # plot(aano_dt_shp_rast,40:44)
    ano_dt_shp_rast

    # Spatial trends
    # trends50
    ano_clm_trn_dt_fl_mon %>%
      filter(dt_type == 'trend' & start_year == '1950') -> trend_dt_fl_mon50

    trn_dt_sel_rast50 <- rast(trend_dt_fl_mon50$dt_pth)
    trn_dt_sel_rast50
    # plot(trn_dt_sel_rast50)

    # crop for selected area
    trn_dt_shp_rast50 <- apply_location_to_raster(trn_dt_sel_rast50, location)
    # plot(trn_dt_shp_rast50)

    # trends80
    ano_clm_trn_dt_fl_mon %>%
      filter(dt_type == 'trend' & start_year == '1980') -> trend_dt_fl_mon80

    trn_dt_sel_rast80 <- rast(trend_dt_fl_mon80$dt_pth)
    trn_dt_sel_rast80
    # plot(trn_dt_sel_rast80)

    #crop for selected area
    trn_dt_shp_rast80 <- apply_location_to_raster(trn_dt_sel_rast80, location)

    # Final return list
    result_lst <- return(list(
      fltr_ano_dt = ano_dt_shp_rast,
      fltr_clm_dt = clm_dt_shp_rast,
      fltr_trn50_dt = trn_dt_shp_rast50,
      fltr_trn80_dt = trn_dt_shp_rast80,
      fltr_mtdt_fl = ano_clm_trn_dt_fl_mon
    ))

    return(result_lst)
  })

  # Time-series and linear trend -------------------------
  time_series_trnd_rct <- eventReactive(input$run_ana_button, {
    withProgress(message = 'Calculating linear trends', value = 0, {
      incProgress(0.02, detail = "Filtering data...")
      ## time series data generate -----------
      # Filtered reactive data
      ano_clm_trn_sel_dt_rct()[[1]] -> ano_dt_shp_rast

      ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt

      # sel_dt_mtdt <- ano_clm_trn_dt_fl_mon

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

      ano_shp_av_dt$ano <- round(ano_shp_av_dt$ano, digits = 4)

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
        dplyr::select(yr, ano, par, mon, region) -> av_ano_ts

      ## Trend calculation and plot ------------

      # Background requirements for plots
      parr <- unique(ano_shp_av_dt$par)
      monn <- unique(ano_shp_av_dt$mon)
      region <- unique(ano_shp_av_dt$region)

      # Trend on average anomaly 1950 - now
      ano_shp_av_dt %<>%
        filter(yr > 1950) %>%
        mutate(
          # trnd =zyp.trend.vector(ano)[["trend"]],
          # incpt =zyp.trend.vector(ano)[["intercept"]],
          #sig = zyp.trend.vector(ano)[["sig"]])
          sig = round(MannKendall(ano)[[2]], digits = 4)
        )
      ano_shp_av_dt

      ano_mk_trnd <-
        zyp.sen(ano ~ yr, ano_shp_av_dt) ##Give the trend###
      ano_mk_trnd$coefficients
      ano_shp_av_dt$trn <- ano_mk_trnd$coeff[[2]]
      ano_shp_av_dt$incpt <- ano_mk_trnd$coeff[[1]]

      xs = c(min(ano_shp_av_dt$yr), max(ano_shp_av_dt$yr))
      trn_slp = c(unique(ano_shp_av_dt$incpt), unique(ano_shp_av_dt$trn))
      ys = cbind(1, xs) %*% trn_slp
      ano_shp_av_dt$trn_lab = paste(
        "italic(1950-~trend)==",
        round(ano_shp_av_dt$trn, 2),
        "~yr^{-1}~','~italic(p)==",
        round(ano_shp_av_dt$sig, 2)
      )

      #     mag_trnd_lab=paste("italic(t)==",round(ano_shp_av_dt$trn,2),get_unit(),
      #                        "~mm~yr^{-1}~','~italic(p)==",round(ano_shp_av_dt$sig,2))

      # Trend on average anomaly 1980 - now
      ano_shp_av_dt %>%
        filter(yr > 1979) %>%
        mutate(
          # trnd =zyp.trend.vector(ano)[["trend"]],
          # incpt =zyp.trend.vector(ano)[["intercept"]],
          #sig = zyp.trend.vector(ano)[["sig"]])
          sig = round(MannKendall(ano)[[2]], digits = 2)
        ) -> ano_shp_av_dt80
      ano_shp_av_dt80

      ano_mk_trnd80 <-
        zyp.sen(ano ~ yr, ano_shp_av_dt80) ##Give the trend###
      ano_mk_trnd80$coefficients
      ano_shp_av_dt80$trn <- ano_mk_trnd80$coeff[[2]]
      ano_shp_av_dt80$incpt <- ano_mk_trnd80$coeff[[1]]

      xs80 = c(min(ano_shp_av_dt80$yr), max(ano_shp_av_dt80$yr))
      trn_slp80 = c(unique(ano_shp_av_dt80$incpt), unique(ano_shp_av_dt80$trn))
      ys80 = cbind(1, xs80) %*% trn_slp80
      ano_shp_av_dt80$trn_lab = paste(
        "italic(1980-~trend)==",
        round(ano_shp_av_dt80$trn, 2),
        "~yr^{-1}~','~italic(p)==",
        round(ano_shp_av_dt80$sig, 2)
      )

      incProgress(0.02, detail = "Plotting linear trend ...")

      # anomaly plot
      ymin <- (-1) * (max(abs(ano_shp_av_dt$ano)))
      ymax <- (1) * (max(abs(ano_shp_av_dt$ano)))
      minyr <- min(ano_shp_av_dt$yr)
      maxyr <- max(ano_shp_av_dt$yr)

      if (ymax < 1) {
        ybrk_neg <-
          round(
            c(seq(
              (-1) *
                (max(
                  abs(ano_shp_av_dt$ano)
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
                  abs(ano_shp_av_dt$ano)
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
                abs(ano_shp_av_dt$ano)
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
                abs(ano_shp_av_dt$ano)
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
      ano_shp_av_dt %<>%
        mutate(pos_neg = if_else(ano <= 0, "neg", "pos")) %>%
        mutate(ano_mv = rollmean(ano, 3, fill = list(NA, NULL, NA)))
      ano_shp_av_dt
      tail(ano_shp_av_dt)

      if (parr == "prcp" | parr == "soil_moisture") {
        par_title <- paste0(
          get_region(),
          " ",
          get_par_full(),
          " ",
          "anomaly",
          " (% of normal)",
          " : ",
          get_mon_full()
        )
      } else {
        par_title <- paste0(
          get_region(),
          " ",
          get_par_full(),
          " ",
          "anomaly",
          " (",
          get_unit(),
          ")",
          " : ",
          get_mon_full()
        )
      }

      if (parr == "prcp" | parr == "soil_moisture") {
        y_axis_lab <- paste0(parr, " average anomaly (% of normal)")
      } else {
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
          label = ano_shp_av_dt$trn_lab[[1]],
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
          label = ano_shp_av_dt80$trn_lab[[1]],
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
        labs(title = par_title, subtitle = "Baseline: 1981-2010") +
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
          # panel.spacing=unit(0.1,"lines"),
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
          axis.text = element_text(
            margin = margin(t = -5, r = -5, b = -5, l = -5)
          ),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(colour = 'Black')
        )
      ano_shp_trn_plt

      if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
        ano_shp_trn_plt <- ano_shp_trn_plt +
          scale_fill_gradientn(
            name = paste0(parr, "  anomaly ", get_unit()),
            colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
            limits = c(ymin, ymax),
            breaks = ybrks_seq
          )
      }
      ano_shp_trn_plt <- ano_shp_trn_plt +
        theme(axis.title.y = element_blank())
      ano_shp_trn_plt

      # plotly display

      trn1980_lab <-
        paste0(
          '1980-trend = ',
          round(ano_shp_av_dt80$trn[[1]], 2),
          'yr<sup>-1</sup>',
          '<span>&#44;</span> ',
          ' <i>p<i>=',
          round(ano_shp_av_dt80$sig[[1]], 2)
        )
      trn1980_lab
      trn1950_lab <-
        paste0(
          '1950-trend = ',
          round(ano_shp_av_dt$trn[[1]], 2),
          'yr<sup>-1</sup>',
          '<span>&#44;</span> ',
          ' <i>p<i>=',
          round(ano_shp_av_dt$sig[[1]], 2)
        )
      trn1950_lab

      #Convert to plotly
      ano_shp_trn_plty <- ggplotly(ano_shp_trn_plt) %>%
        layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.6, y = 1.0)
        ) %>%
        layout(
          margin = list(l = 0, r = 0, b = 10, t = 80),
          title = list(
            x = 0.001,
            y = 0.92,
            text = paste0(
              par_title,
              '<br>',
              '<sup>',
              'Baseline: 1981-2010',
              '</sup>'
            )
          )
        ) %>%
        layout(
          annotations = list(
            list(
              x = 1,
              y = 0.0,
              text = plt_wtrmrk,
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'auto',
              xshift = 0,
              yshift = 0,
              font = list(size = 9, color = '#e5e5e5')
            )
          )
        ) %>%
        layout(
          annotations = list(
            list(
              x = 0.30,
              y = 0.97,
              text = trn1950_lab,
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'auto',
              xshift = 0,
              yshift = 0,
              font = list(size = 15, color = "black")
            )
          )
        ) %>%
        layout(
          annotations = list(
            list(
              x = 0.30,
              y = 0.93,
              text = trn1980_lab,
              showarrow = F,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'auto',
              xshift = 0,
              yshift = 0,
              font = list(size = 15, color = '#00bfff')
            )
          )
        ) %>%
        layout(xaxis = list(showgrid = FALSE), yaxis = list(showgrid = FALSE))
      ano_shp_trn_plty

      ### File name for download -----
      # Year range
      if (monn != "annual") {
        mx_yr = max_year
      } else {
        mx_yr = max_year - 1
      }

      fl_nam <-
        paste0(
          get_region(),
          "_",
          parr,
          "_anomaly_timeseries",
          "_",
          monn,
          "_",
          min_year,
          "_",
          mx_yr
        )
      fl_nam
      incProgress(0.05, detail = "Finalizing linear trend ...")
      # Final return list
      return(list(
        lnr_trn_ptly_plt = ano_shp_trn_plty,
        fl_nam_dwnld = fl_nam,
        lnr_trn_plt_dwnld = ano_shp_trn_plt,
        ts_data_csv = av_ano_ts
      ))
    })
  })

  ## display linear trend  ---------------
  output$lnr_trn_plt <- renderPlotly({
    time_series_trnd_rct()[[1]]
  })

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
      paste0(time_series_trnd_rct()[[2]], "_data.csv")
    },
    content = function(file) {
      write_csv(time_series_trnd_rct()[[4]], file, append = FALSE)
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

    location <- get_analysis_location()
    sel_area_shpfl <- location$data

    ano_dt_sel_rast <- ano_dt_shp_rast
    names(ano_dt_sel_rast)

    # Filter for selected year (s)
    sel_yrs <- get_years()

    if (length(sel_yrs) > 50) {
      sel_yrs <- sel_yrs[1:50]
      shinyalert(
        html = T,
        text = tagList(h3(
          "Too many years selected, maximum 50 allowed."
        )),
        showCancelButton = T
      )
    }

    yr_df <- tibble(paryr = names(ano_dt_sel_rast))
    yr_df %<>%
      mutate(yr = as.numeric(str_extract(paryr, "[0-9]+")))
    names(ano_dt_sel_rast) <- yr_df$yr

    ano_dt_rast <- subset(
      ano_dt_sel_rast,
      which(names(ano_dt_sel_rast) %in% sel_yrs)
    )
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
    if (parr == "prcp" | parr == "soil_moisture") {
      par_title <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " anomaly (% of normal)",
        ": ",
        get_mon_full()
      )
    } else {
      par_title <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " anomaly (",
        get_unit(),
        ")",
        ": ",
        get_mon_full()
      )
    }

    xlim <- c(-140, -113.0)
    ylim <- c(45, 61)

    ### plot to display ----

    spatial_ano_plt <- ggplot() +
      geom_spatraster(data = ano_dt_rast) +
      scale_fill_gradientn(
        name = paste0(parr, " anomaly ", get_unit()),
        colours = cpt(pal = "ncl_BlWhRe", n = 100, rev = F),
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
        name = "Longitude (°W) ",
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
        axis.text = element_text(
          margin = margin(t = -5, r = -5, b = -5, l = -5)
        ),
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

    if (
      parr == "prcp" &
        maxval > 200 |
        parr == "soil_moisture" & maxval > 200 |
        parr == "rh" & maxval > 200
    ) {
      spatial_ano_plt <- spatial_ano_plt +
        scale_fill_gradientn(
          name = paste0(parr, " anomaly ", get_unit()),
          colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
          na.value = "transparent",
          limits = c(minval, maxval),
          breaks = brks_seq,
          labels = labels_val
        )
    } else if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
      spatial_ano_plt <- spatial_ano_plt +
        scale_fill_gradientn(
          name = paste0(parr, "  anomaly (%) "),
          colours = cpt(pal = "cmocean_curl", n = 100, rev = T),
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
          '[',
          get_region(),
          ' anomaly over ',
          minyr,
          '-',
          maxyr,
          ': Mean = ',
          ano_ovr_dt[2, 2],
          ' ,',
          ' Range = ',
          ano_ovr_dt[1, 2],
          ' - ',
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
    spatial_ano_plt <- add_user_point_marker(spatial_ano_plt, location)

    ### File name for download ------------
    fl_nam <-
      paste0(
        get_region(),
        "_",
        parr,
        "_anomaly",
        "_",
        monn,
        "_",
        input$year_range[1],
        "_",
        input$year_range[2]
      )
    fl_nam

    ## final reactive output list  -------------------

    return(list(
      sptl_ano_data = ano_dt_rast,
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
      writeRaster(
        spatial_ano_dt_plt_rct()[[1]],
        file,
        filetype = "GTiff",
        overwrite = TRUE
      )
    }
  )

  # Climate normal plot ----------------------------------------------------------------------------
  clm_nor_plt_rct <- eventReactive(input$run_ana_button, {
    ano_clm_trn_sel_dt_rct()[[2]] -> clm_dt_shp_rast

    ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt
    parr <- unique(sel_dt_mtdt$par)
    monn <- unique(sel_dt_mtdt$mon)

    location <- get_analysis_location()
    sel_area_shpfl <- location$data

    ## Climate normal plot title -----
    if (parr == "prcp") {
      clm_nor_title_txt <-
        # Climate plot title ( use log for prcp)
        paste0(
          get_region(),
          " mean ",
          get_par_full(),
          " (average of  1981-2010)",
          "(",
          get_unit(),
          ")",
          " (log-scale)",
          "  : ",
          get_mon_full()
        )
    } else {
      clm_nor_title_txt <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " (average of  1981-2010) ",
        "(",
        get_unit(),
        ")",
        " : ",
        get_mon_full()
      )
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
    } else {
      clm_dt_shp_rast1 <- clm_dt_shp_rast
    }

    spatial_clm_plt <- ggplot() +
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
        name = "Longitude (°W) ",
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
        axis.text = element_text(
          margin = margin(t = -5, r = -5, b = -5, l = -5)
        ),
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

    if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
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
          mn_clm_val[[1]],
          " ",
          "(",
          get_unit(),
          ")",
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
    spatial_clm_plt <- add_user_point_marker(spatial_clm_plt, location)

    ## Climate normal data and plot download ---------
    fl_nam <-
      paste0(
        get_region(),
        "_",
        get_par_full(),
        "_climate_normal_1981_2010",
        "_",
        get_mon_full()
      )
    fl_nam

    # Plot with title for download

    # Climate plot title ( use log for prcp)
    if (parr == "prcp") {
      par_title <- paste0(
        get_region(),
        " ",
        get_par_full(),
        "",
        "(",
        get_unit(),
        ")",
        " (log-scale)",
        " : ",
        get_mon_full(),
        " (average 1981-2010)"
      )
    } else {
      par_title <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " ",
        "(",
        get_unit(),
        ")",
        " : ",
        get_mon_full(),
        " (average 1981-2010)"
      )
    }

    spatial_clm_plt_dnwld <- spatial_clm_plt +
      labs(title = par_title)
    spatial_clm_plt_dnwld

    ### Final reactive output list --------------

    return(list(
      clm_nor_title_txt = clm_nor_title_txt,
      clm_nor_plt = spatial_clm_plt,
      clm_nor_plt_dnwld = spatial_clm_plt_dnwld,
      clm_nor_data = clm_dt_shp_rast1,
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
        plot = clm_nor_plt_rct()[[3]],
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
      writeRaster(
        clm_nor_plt_rct()[[4]],
        file,
        filetype = "GTiff",
        overwrite = TRUE
      )
    }
  )

  # Spatial anomaly trends for 1950s and 1980s ---------------------------------------------------------

  spatial_ano_trnd_rct <- eventReactive(input$run_ana_button, {
    withProgress(message = 'Calculating spatial trends', value = 0, {
      incProgress(0.1, detail = "Extracting data ...")

      ano_clm_trn_sel_dt_rct()[[5]] -> sel_dt_mtdt
      parr <- unique(sel_dt_mtdt$par)
      monn <- unique(sel_dt_mtdt$mon)

      location <- get_analysis_location()
      sel_area_shpfl <- location$data

      # 1950s spatial trend ----------
      ano_clm_trn_sel_dt_rct()[[3]] -> ano_trn_mag_sig50
      # ano_trn_mag_sig50 <- trn_dt_shp_rast50

      nm <- names(ano_trn_mag_sig50)

      # Standardize layer names
      names(ano_trn_mag_sig50) <- dplyr::case_when(
        nm %in% c("trend_mag", "trnmag") ~ "trnmag",
        nm == "pval" ~ "pval",
        TRUE ~ nm
      )

      # Check that both required layers exist
      stopifnot(all(c("trnmag", "pval") %in% names(ano_trn_mag_sig50)))

      # Force consistent layer order
      ano_trn_mag_sig50 <- ano_trn_mag_sig50[[c("trnmag", "pval")]]

      # Now layer 1 is ALWAYS trnmag
      mn_trn_val50 <- round(
        global(ano_trn_mag_sig50[["trnmag"]], "mean", na.rm = TRUE),
        3
      )
      mi_trn_val50 <- round(
        global(ano_trn_mag_sig50[["trnmag"]], "min", na.rm = TRUE),
        3
      )
      mx_trn_val50 <- round(
        global(ano_trn_mag_sig50[["trnmag"]], "max", na.rm = TRUE),
        3
      )

      # Convert to point data
      ano_sp_mk_trn_sig_dt50 <- as_tibble(
        ano_trn_mag_sig50,
        xy = TRUE,
        na.rm = TRUE
      ) %>%
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
        geom_tile(
          data = ano_sp_mk_trn_sig_dt50,
          aes(x = x, y = y, fill = trnmag),
          alpha = 1
        ) +
        scale_fill_continuous_diverging(
          palette = "Blue-Red",
          n_interp = 21,
          limits = c(-mxtrn50, mxtrn50),
          # breaks=seq(-1.2, 1.2,0.3),
          # labels=seq(-0.8, 0.8,0.2),
          # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
          name = bquote(
            ~"trend" ~ yr^{
              -1
            }
          )
        ) +
        geom_point(
          data = ano_dt_sig_trn50,
          aes(x = x, y = y),
          color = "Black",
          fill = "Gray10",
          alpha = 0.4,
          size = 0.3,
          shape = 3
        ) +
        geom_sf(
          data = sel_area_shpfl,
          colour = "black",
          size = 1,
          fill = NA,
          alpha = 0.8
        ) +
        scale_x_continuous(
          name = "Longitude (°W) ",
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
          axis.text = element_text(
            margin = margin(t = -5, r = -5, b = -5, l = -5)
          ),
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
      ano_dt_sp_trn_sig_plt50

      if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
        ano_dt_sp_trn_sig_plt50 <- ano_dt_sp_trn_sig_plt50 +
          scale_fill_continuous_diverging(
            palette = "green-brown",
            n_interp = 21,
            rev = T,
            limits = c(-mxtrn50, mxtrn50),
            # breaks=seq(-1.2, 1.2,0.3),
            # labels=seq(-0.8, 0.8,0.2),
            # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
            name = bquote(
              ~"trend" ~ yr^{
                -1
              }
            )
          )
      }
      ano_dt_sp_trn_sig_plt50

      ano_dt_sp_trn_sig_plt50 <- ano_dt_sp_trn_sig_plt50 +
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
            mn_trn_val50[[1]],
            " ",
            "(",
            trn_unt,
            " yr",
            "\u207B",
            "\u00B9)",
            "  ",
            "Range = ",
            "[",
            mi_trn_val50[[1]],
            " - ",
            mx_trn_val50[[1]],
            "]. "
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

      nm <- names(ano_trn_mag_sig80)

      # Standardize layer names
      names(ano_trn_mag_sig80) <- dplyr::case_when(
        nm %in% c("trend_mag", "trnmag") ~ "trnmag",
        nm == "pval" ~ "pval",
        TRUE ~ nm
      )

      # Check that both required layers exist
      stopifnot(all(c("trnmag", "pval") %in% names(ano_trn_mag_sig80)))

      # Force consistent layer order
      ano_trn_mag_sig80 <- ano_trn_mag_sig80[[c("trnmag", "pval")]]

      # Now layer 1 is ALWAYS trnmag
      mn_trn_val80 <- round(
        global(ano_trn_mag_sig80[["trnmag"]], "mean", na.rm = TRUE),
        3
      )
      mi_trn_val80 <- round(
        global(ano_trn_mag_sig80[["trnmag"]], "min", na.rm = TRUE),
        3
      )
      mx_trn_val80 <- round(
        global(ano_trn_mag_sig80[["trnmag"]], "max", na.rm = TRUE),
        3
      )
      # plot (1980-now)
      ano_sp_mk_trn_sig_dt80 <- as_tibble(
        ano_trn_mag_sig80,
        xy = TRUE,
        na.rm = TRUE
      ) %>%
        mutate(trnmag = round(trnmag, 3))

      #### Plot trend maps (1980-now)
      incProgress(0.1, detail = "Plotting spatial trend (1980-now)...")

      ano_dt_sig_trn80 <- ano_sp_mk_trn_sig_dt80 %>%
        dplyr::filter(pval <= 0.1)
      ano_dt_sig_trn80

      mxtrn80 <- max(abs(ano_sp_mk_trn_sig_dt80$trnmag), na.rm = T)
      mxtrn80

      ano_dt_sp_trn_sig_plt80 <- ggplot() +
        geom_tile(
          data = ano_sp_mk_trn_sig_dt80,
          aes(x = x, y = y, fill = trnmag),
          alpha = 1
        ) +
        scale_fill_continuous_diverging(
          palette = "Blue-Red",
          n_interp = 21,
          limits = c(-mxtrn80, mxtrn80),
          # breaks=seq(-1.2, 1.2,0.3),
          # labels=seq(-0.8, 0.8,0.2),
          # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
          name = bquote(
            ~"trend" ~ yr^{
              -1
            }
          )
        ) +
        geom_point(
          data = ano_dt_sig_trn80,
          aes(x = x, y = y),
          color = "Black",
          fill = "Gray10",
          alpha = 0.4,
          size = 0.3,
          shape = 3
        ) +
        geom_sf(
          data = sel_area_shpfl,
          colour = "black",
          size = 1,
          fill = NA,
          alpha = 0.8
        ) +
        scale_x_continuous(
          name = "Longitude (°W) ",
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
          axis.text = element_text(
            margin = margin(t = -5, r = -5, b = -5, l = -5)
          ),
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

      if (parr == "prcp" | parr == "soil_moisture" | parr == "rh") {
        ano_dt_sp_trn_sig_plt80 <- ano_dt_sp_trn_sig_plt80 +
          scale_fill_continuous_diverging(
            palette = "green-brown",
            n_interp = 21,
            rev = T,
            limits = c(-mxtrn80, mxtrn80),
            # breaks=seq(-1.2, 1.2,0.3),
            # labels=seq(-0.8, 0.8,0.2),
            # name=expression(paste0(parr," trend ", unt, " yr \U2212 \U00B9")))+
            name = bquote(
              ~"trend" ~ yr^{
                -1
              }
            )
          )
      }
      ano_dt_sp_trn_sig_plt80

      ano_dt_sp_trn_sig_plt80 <- ano_dt_sp_trn_sig_plt80 +
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
            mn_trn_val80[[1]],
            " ",
            "(",
            trn_unt,
            " yr",
            "\u207B",
            "\u00B9)",
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
      ano_dt_sp_trn_sig_plt80 <- add_user_point_marker(
        ano_dt_sp_trn_sig_plt80,
        location
      )
      ano_dt_sp_trn_sig_plt50 <- add_user_point_marker(
        ano_dt_sp_trn_sig_plt50,
        location
      )

      ### Plots titles --------------
      spl_trn_title_txt50 <- paste0(
        get_region(),
        " ",
        get_mon_full(),
        ' ',
        get_par_full(),
        " anomlay trend",
        " (",
        trn_unt,
        " yr",
        "\u207B",
        "\u00B9) since 1950: ",
        get_mon_full(),
        ". Black dots indicate cells with significant trends."
      )

      spl_trn_title_txt80 <- paste0(
        get_region(),
        " ",
        get_mon_full(),
        ' ',
        get_par_full(),
        " anomlay trend",
        " (",
        trn_unt,
        " yr",
        "\u207B",
        "\u00B9) since 1980: ",
        get_mon_full(),
        ". Black dots indicate cells with significant trends."
      )

      ##  For plot and data downloads ---------

      trnd_fl_nam50 <-
        paste0(
          get_region(),
          "_",
          get_par_full(),
          "_spatial_trend_1950_present",
          "_",
          get_mon_full()
        )
      trnd_fl_nam50
      trnd_fl_nam80 <-
        paste0(
          get_region(),
          "_",
          get_par_full(),
          "_spatial_trend_1980_present",
          "_",
          get_mon_full()
        )
      trnd_fl_nam80

      # Plot with title for download

      par_title50 <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " anomaly trend (",
        trn_unt,
        " yr",
        "\u207B",
        "\u00B9): ",
        get_mon_full(),
        "1950-present.
                             Black dots indicate cells with significant trends."
      )
      par_title80 <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " anomaly trend (",
        trn_unt,
        " yr",
        "\u207B",
        "\u00B9): ",
        get_mon_full(),
        "1980-present.
                             Black dots indicate cells with significant trends."
      )

      par_title50 <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " anomaly trend (",
        trn_unt,
        " yr",
        "\u207B",
        "\u00B9): ",
        get_mon_full(),
        " 1950-present.
                             Black dots indicate cells with significant trends."
      )
      par_title80 <- paste0(
        get_region(),
        " ",
        get_par_full(),
        " anomaly trend (",
        trn_unt,
        " yr",
        "\u207B",
        "\u00B9): ",
        get_mon_full(),
        " 1980-present.
                             Black dots indicate cells with significant trends."
      )

      # Plot download
      ano_dt_sp_trn_sig_plt50_dnwld <- ano_dt_sp_trn_sig_plt50 +
        labs(title = par_title50)
      ano_dt_sp_trn_sig_plt50_dnwld

      ano_dt_sp_trn_sig_plt80_dnwld <- ano_dt_sp_trn_sig_plt80 +
        labs(title = par_title80)
      ano_dt_sp_trn_sig_plt80_dnwld

      incProgress(0.02, detail = "Finalizing spatial trends ...")
      # return plot or data here
      return(list(
        plt_title_1950 = spl_trn_title_txt50,
        trn_plt_1950 = ano_dt_sp_trn_sig_plt50,
        plt_title_1980 = spl_trn_title_txt80,
        trn_plt_1980 = ano_dt_sp_trn_sig_plt80,

        dnwld_fl_nam50 = trnd_fl_nam50,
        dnwld_trn_plt50 = ano_dt_sp_trn_sig_plt50_dnwld,
        dnwld_trn_dt50 = ano_trn_mag_sig50,

        download_fl_nam80 = trnd_fl_nam80,
        downalod_trn_plt80 = ano_dt_sp_trn_sig_plt80_dnwld,
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
        plot = spatial_ano_trnd_rct()[[6]],
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
      writeRaster(
        spatial_ano_trnd_rct()[[7]],
        file,
        filetype = "GTiff",
        overwrite = TRUE
      )
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
        plot = spatial_ano_trnd_rct()[[9]],
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
      writeRaster(
        spatial_ano_trnd_rct()[[10]],
        file,
        filetype = "GTiff",
        overwrite = TRUE
      )
    }
  )

  # Feedback text -------
  output$feedback_text <- renderText({
    HTML(
      "<p>We used <a href='https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview' target='_blank'>
ERA5-Land hourly data</a> to calculate the anomalies and climatology.
Anomalies are calculated as the measure of departure from the climatological averages spanning from 1981 to 2010.
Should you have any inquiries or wish to provide feedback, please do not hesitate to use
<a href='https://forms.office.com/r/wN0QYAvSTZ' target='_blank'>this feedback form</a> or write to
<a href='mailto:Aseem.Sharma@gov.bc.ca'><b>Aseem Sharma</b></a>.</p>"
    )
  })

  # Reports --------------------------------------

  ## Years present in reports ----
  report_years <- sort(unique(
    as.numeric(substr(
      report_suffixes[grepl("^[A-Za-z]{3}[0-9]{4}$", report_suffixes)],
      4,
      7
    )),
    decreasing = TRUE
  ))

  ## Helper: resolve report filename ----
  get_report_filename <- function(suffix) {
    switch(
      suffix,
      "ann2025" = "bc_annual_climate_summary_2025.html",
      "ann2024" = "bc_annual_climate_summary_2024.html",
      "ann2023" = "bc_annual_climate_summary_2023.html",
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

          file1 <- paste0(
            "bc_monthly_climate_summary_",
            month_full,
            "_",
            year,
            ".html"
          )
          file2 <- paste0(
            month_full,
            "_",
            year,
            "_bc_mon_sea_ann_climate_summary.html"
          )

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
      "monthly" = "#007ACC",
      "annual" = "#1B7F3B",
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
            if (!grepl("^[A-Za-z]{3}[0-9]{4}$", suffix)) {
              return(NULL)
            }

            year <- substr(suffix, 4, 7)
            if (as.numeric(year) != yr) {
              return(NULL)
            }

            month_abbr <- toupper(substr(suffix, 1, 3))
            month_num <- match(month_abbr, toupper(month.abb))
            if (is.na(month_num)) {
              return(NULL)
            }

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
      height = 700,
      align = 'center'
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
      height = 700,
      align = 'center'
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
    }
  )

  # App deployment date ----
  output$deploymentDate <- renderText({
    paste0(
      "This app was last updated on ",
      readLines("deployment_history.txt"),
      '.'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
