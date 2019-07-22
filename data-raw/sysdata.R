source("data-raw/field_translations.R")
source("data-raw/filter_translations.R")
source("data-raw/status_translations.R")
source("data-raw/informal_groups.R")
usethis::use_data(
  field_translations,
  filter_translations,
  admin_status_translations,
  redlist_status_translations,
  informal_groups,
  internal = TRUE,
  overwrite = TRUE
)
