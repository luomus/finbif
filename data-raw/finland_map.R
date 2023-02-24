client <- ows4R::WFSClient$new(
  "http://geo.stat.fi/geoserver/wfs", serviceVersion =  "1.0.0"
)
caps <- client$getCapabilities()
ft <-
  caps$findFeatureTypeByName("tilastointialueet:maakunta1000k", exact = TRUE)
finland_map <- ft$getFeatures()
sf::st_crs(finland_map) <- 3067
finland_map <- sf::st_union(finland_map)
finland_map <- sf::st_simplify(finland_map, FALSE, 1000)
finland_map <- sf::st_cast(finland_map, "POLYGON")
finland_map <- sf::st_transform(finland_map, 4326)
finland_map <- unlist(finland_map, FALSE)
finland_map <- lapply(finland_map, rbind, NA)
finland_map <-
  list(vertices = do.call(rbind, finland_map), bbox = c(19, 59, 32, 71))
