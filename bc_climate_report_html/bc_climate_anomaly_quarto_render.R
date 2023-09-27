library(quarto)
library(tictoc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# tic()

# Render the anomaly quarto file in the www app folder of the shiny app with date.

update_month <- "August"
update_year <- "2023"

# pdf_file_name = paste0("BC_climate_anomaly_",update_month,"_",update_year,".pdf")
html_file_name = paste0("BC_climate_anomaly_",update_month,"_",update_year,".html")


# html_render
quarto::quarto_render(
  input="bc_eral_climate_anomaly_report_html.qmd",
  output_file = html_file_name, output_format = "html"
)

# # pdf_render
#  quarto::quarto_render(
#    input="bc_eral_climate_anomaly_report_html_pdf.qmd",
#   output_file = pdf_file_name, output_format = "pdf",
# )

 # Copy files from regular folder to www for shiny upload and remove from orginal folder
 html_output_file_name <- basename(html_file_name)
 # pdf_output_file_name <- basename(pdf_file_name)

 file.copy(from = html_output_file_name, to = "../www/", overwrite = T)
 # file.copy(from = pdf_output_file_name, to = "../www/", overwrite = T)

 #Remove files and folders from original
 file.remove(html_output_file_name)
 # file.remove(pdf_output_file_name)
 # file.remove('bc_eral_climate_anomaly_report_html_pdf.xdv')
 # file.remove('bc_eral_climate_anomaly_report_html_pdf.fdb_latexmk')

 # unlink("bc_eral_climate_anomaly_report_html_pdf_files", recursive = TRUE)

toc()