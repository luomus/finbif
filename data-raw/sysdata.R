source("data-raw/var_translations.R")
source("data-raw/filter_translations.R")
source("data-raw/status_translations.R")
source("data-raw/informal_groups.R")
source("data-raw/habitat_types.R")
source("data-raw/taxon_ranks.R")
source("data-raw/countries.R")
source("data-raw/provinces.R")
source("data-raw/municipalities.R")
source("data-raw/bird_association_area.R")
usethis::use_data(
  var_names,
  filter_names,
  administrative_status,
  red_list_status,
  informal_group,
  informal_group_reported,
  primary_habitat,
  primary_secondary_habitat,
  taxon_rank,
  country,
  province,
  municipality,
  bird_assoc_area,
  internal = TRUE,
  overwrite = TRUE
)
