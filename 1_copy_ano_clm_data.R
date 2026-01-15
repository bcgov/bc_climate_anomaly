org_ano_clm_dt_pth <- "../data_database/data_eral/eral_ano_clm_dt/"
copy_ano_clm_dt_pth <- "./ano_clm_trn_data/"

# list all nc files
ano_clm_dt_fls <- list.files(
  path = org_ano_clm_dt_pth,
  pattern = "\\.nc$",
  full.names = TRUE
)

# filter out _res01.nc
ano_clm_dt_fls_to_copy <- ano_clm_dt_fls[!grepl("_res01\\.nc$", ano_clm_dt_fls)]

# copy them
file.copy(
  from = ano_clm_dt_fls_to_copy,
  to = copy_ano_clm_dt_pth,
  overwrite = TRUE
)
