p_needed <-
  c(
    "stringi",
    "keras",
    "tidyverse",
    "MASS",
    "future.apply",
    "rgdal",
    "geojsonio",
    "rmapshaper",
    "abind",
    "maptools",
    "cartogram"
  )

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
print(sapply(p_needed, require, character.only = TRUE))

plan(multiprocess)