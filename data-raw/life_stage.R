life_stage <- c(
  "adult", "immature", "egg", "tadpole", "pupa", "nymph", "subimago", "larva",
  "snag", "embryo", "subadult", "mature", "sterile", "fertile", "sprout",
  "dead_sprout", "bud", "flower", "withered_flower", "seed", "ripening_fruit",
  "ripe_fruit", "subterranean"
)
life_stage <- data.frame(
  stage = life_stage, row.names = life_stage, stringsAsFactors = FALSE
)
class(life_stage[["stage"]]) <- "translation"
