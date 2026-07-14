rm(list = ls(all = TRUE))

library(quarto)
library(tictoc)

#--- User Inputs ----
update_month <- "June"
update_year <- "2026"

# Modify and run the R script for monthly plots --- ----
# Modify R script and createa and save plots for the given months in the folder mon_2mon_results_plots
r_script_path <- './bc_mon_climate_report/1_bc_mon_sea_ann_clmt_summary_4quarto_report.R'
r_script_updated_path <- "./bc_mon_climate_report/1_bc_mon_sea_ann_clmt_summary_4quarto_report_UPDATED.R"

# Read and modify R script
r_lines <- readLines(r_script_path)
r_lines <- gsub(
  '^update_month\\s*<-\\s*\\".*\\"',
  paste0('update_month <- "', update_month, '"'),
  r_lines
)
r_lines <- gsub(
  '^update_year\\s*<-\\s*\\".*\\"',
  paste0('update_year <- "', update_year, '"'),
  r_lines
)

# r_lines <- gsub(
#   '^(min_year\\s*<-\\s*)[0-9]+(.*)$',
#   paste0('\\1', min_year, '\\2'),
#   r_lines
# )

# r_lines <- gsub(
#   '^(max_year\\s*<-\\s*)[0-9]+(.*)$',
#   paste0('\\1', max_year, '\\2'),
#   r_lines
# )

# Write modified R script to temporary file
writeLines(r_lines, r_script_updated_path)

# Run the updated R script
message("Running summary data preparation script...")

tic("Run R script")
source(r_script_updated_path, echo = TRUE, max.deparse.length = Inf)
toc()

# Modify and render the Quarto report -----------
qmd_file_path <- "./bc_mon_climate_report/2_bc_mon_sea_ann_clmt_summary_quarto_report.qmd"
updated_qmd_file <- "./bc_mon_climate_report/2_bc_mon_sea_ann_clmt_summary_quarto_report_UPDATED.qmd"

# Read and modify QMD
qmd_lines <- readLines(qmd_file_path)
qmd_lines <- gsub(
  '^update_month\\s*<-\\s*\\".*\\"',
  paste0('update_month <- "', update_month, '"'),
  qmd_lines
)
qmd_lines <- gsub(
  '^update_year\\s*<-\\s*\\".*\\"',
  paste0('update_year <- "', update_year, '"'),
  qmd_lines
)

# Write modified QMD to file
writeLines(qmd_lines, updated_qmd_file)

# Render the updated QMD file
html_file_name <- paste0(
  update_month,
  "_",
  update_year,
  "_bc_mon_sea_ann_climate_summary.html"
)

message("Rendering HTML report...")
tic("Render Quarto HTML")
quarto::quarto_render(
  input = updated_qmd_file,
  output_file = html_file_name,
  output_format = "html"
)
toc()

# Copy to shiny www folder if needed
html_output_file_name <- basename(html_file_name)

html_output_file_name <- paste0(
  './bc_mon_climate_report/',
  html_output_file_name
)
html_output_file_name

file.copy(from = html_output_file_name, to = "./www/", overwrite = T)

# Remove files and folders from original
file.remove(html_output_file_name)
unlink(
  "2_bc_mon_sea_ann_clmt_summary_quarto_report_UPDATED_cache",
  recursive = TRUE
)
