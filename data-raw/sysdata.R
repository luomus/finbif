source("data-raw/variables.R")
source("data-raw/citable_file_variables.R")
source("data-raw/lite_download_file_variables.R")
source("data-raw/filters.R")
source("data-raw/informal_groups.R")
source("data-raw/sources.R")
source("data-raw/misc.R")
source("data-raw/location_tag.R")
source("data-raw/complete_list_type.R")
source("data-raw/enums.R")

var_names_df <- var_names
has_value_df <- has_value
cite_file_vars_df <- cite_file_vars
lite_download_file_vars_df <- lite_download_file_vars
filter_names_df <- filter_names
informal_groups_df <- informal_groups
source_df <- source
superrecord_basis_df <- superrecord_basis
sex_df <- sex
quality_issues_df <- quality_issues
record_reliability_df <- record_reliability
complete_list_type_df <- complete_list_type
location_tag_df <- location_tag

supported_langs <- c(English = "en", Finnish = "fi", Swedish = "sv")

usethis::use_data(
  var_names_df,
  has_value_df,
  cite_file_vars_df,
  lite_download_file_vars_df,
  filter_names_df,
  informal_groups_df,
  source_df,
  superrecord_basis_df,
  sex_df,
  quality_issues_df,
  record_reliability_df,
  complete_list_type_df,
  location_tag_df,
  supported_langs,
  internal = TRUE,
  overwrite = TRUE
)

# exported data
source("data-raw/finland_map.R")
