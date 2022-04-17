path <- "/development/projects/laji/src/i18n/"

locales <- sprintf("https://github.com/luomus/laji.git/branches%s", path)

locales <- system2("svn", c("ls", locales), TRUE)

locales <- Map(
  \(x) sprintf("https://raw.githubusercontent.com/luomus/laji%s%s", path, x),
  locales
)

locales <- Map(jsonlite::fromJSON, locales)

locales <- Map(unlist, locales)

locales <- as.data.frame(locales)

names(locales) <- gsub(".json", "", names(locales))

lite_download_file_vars_test <- read.csv(
  "data-raw/lite_download_file_variables.csv", stringsAsFactors = FALSE,
  strip.white = TRUE, row.names = 1L, comment.char = "#", na.strings = ""
)

locales <- locales[rownames(lite_download_file_vars_test), ]

for (l in names(locales)) {

  locales[[l]] <- ifelse(
    is.na(locales[[l]]),
    lite_download_file_vars_test[["translated_var"]],
    locales[[l]]
  )

  locales[[l]] <- ifelse(
    is.na(lite_download_file_vars_test[["combine_with"]]),
    locales[[l]],
    paste(
      locales[[l]],
      locales[lite_download_file_vars_test[["combine_with"]], l],
      sep = " + "
    )
  )

}

locales <- Map(make.names, locales)

lite_download_file_vars_test <- cbind(lite_download_file_vars_test, locales)
