source("data-raw/variables.R")
source("data-raw/citable_file_variables.R")
source("data-raw/lite_download_file_variables.R")
source("data-raw/filters.R")

save(
  var_names_df,
  has_value_df,
  cite_file_vars_df,
  lite_download_file_vars_df,
  filter_names_df,
  file = "R/sysdata.rda",
  compress = "bzip2"
)
