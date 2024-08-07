path <- "/master/projects/laji/src/i18n/"

if (identical(Sys.getenv("BRANCH"), "dev")) {

  path <- "/development/projects/laji/src/i18n/"

}

locales <- c("en.json", "fi.json", "sv.json")

locales <- Map(
  \(x) sprintf("https://raw.githubusercontent.com/luomus/laji%s%s", path, x),
  locales
)

locales <- Map(jsonlite::fromJSON, locales)

locales <- Map(unlist, locales)

nms <- Map(names, locales)

nms <- Reduce(intersect, nms)

locales <- lapply(locales, `[`, nms)

locales <- as.data.frame(locales)

names(locales) <- gsub(".json", "", names(locales))

lite_download_file_vars_df <- read.csv(
  "data-raw/lite_download_file_variables.csv", stringsAsFactors = FALSE,
  strip.white = TRUE, row.names = 1L, comment.char = "#", na.strings = ""
)

locales <- locales[rownames(lite_download_file_vars_df), ]

for (l in names(locales)) {

  locales[[l]] <- ifelse(
    is.na(locales[[l]]),
    lite_download_file_vars_df[["translated_var"]],
    locales[[l]]
  )

}

locales <- Map(make.names, locales)

lite_download_file_vars_df <- cbind(lite_download_file_vars_df, locales)
