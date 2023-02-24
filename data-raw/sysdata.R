source("data-raw/variables.R")
source("data-raw/citable_file_variables.R")
source("data-raw/lite_download_file_variables.R")
source("data-raw/filters.R")
source("data-raw/informal_groups.R")
source("data-raw/location_tag.R")

var_names_df <- var_names
has_value_df <- has_value
cite_file_vars_df <- cite_file_vars
lite_download_file_vars_df <- lite_download_file_vars
filter_names_df <- filter_names
informal_groups_df <- informal_groups
location_tag_df <- location_tag

supported_langs <- c(English = "en", Finnish = "fi", Swedish = "sv")

usethis::use_data(
  var_names_df,
  has_value_df,
  cite_file_vars_df,
  lite_download_file_vars_df,
  filter_names_df,
  informal_groups_df,
  location_tag_df,
  supported_langs,
  internal = TRUE,
  overwrite = TRUE
)

# exported data
source("data-raw/finland_map.R")
