GetINEGIAGEBS2018 <- function( write.shapes = FALSE) {
  all.mexico.shape.files <- list.files("downloadAGEBS/geo_data_2018", "^[[:digit:]]+{2}a[r]*\\.shp$", recursive = T ,full.names = T)
  list.all.shp.mexico <- lapply(all.mexico.shape.files, readShapePoly)
  all.shp.mexico <- do.call(bind, list.all.shp.mexico)
  if (write.shapes) {
    writeSpatialShape(all.shp.mexico, "downloadAGEBS/all.shp")
  }
  return(all.shp.mexico)
}
