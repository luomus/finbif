source("data-raw/field_translations.R")
source("data-raw/filter_translations.R")
source("data-raw/status_translations.R")
usethis::use_data(
  field_translations,
  filter_translations,
  admin_status_translations,
  redlist_status,
  internal = TRUE, overwrite = TRUE
)
