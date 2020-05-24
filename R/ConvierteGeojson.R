#' Estadísticas de Poligonos
#'
#' Permite revisar las estadísticas del tamaño del polygono, tanto en el tamñano y número de puntos que lo
#' conforman.
#'
#' @param polygon.to.check Objeto sf con poligonos georeferenciados
#'
#' @return Texto con la estadística de tamaño y número de puntos
#' @export
#'
#' @examples \dontrun{EstadisticasPoligono(PoligonoLocalidad)}
EstadisticasPoligono <- function(polygon.to.check) {
  size.in.memory <- object_size(polygon.to.check)
  pts <- st_cast(polygon.to.check$geometry, "MULTIPOINT")
  number.points <- sum(sapply(pts, length))
  sprintf("Tamaño del poligono: %s Mb Numero de Puntos: %d", round(size.in.memory / 1000000, 1),  number.points)
}

read.locality.rur <- st_read("national_loc_rur.shp") %>%
  select("CVEGEO", "NOM_ENT", "NOM_MUN", "NOM_LOC") %>%
  rename( cvegeo = CVEGEO, nom_ent = NOM_ENT, nom_mun = NOM_MUN, nom_loc = NOM_LOC) %>%
  as.data.frame() %>%
  select(-geometry)

read.locality.urb <- st_read("national_loc_urb.shp") %>%
  select("CVEGEO", "NOM_ENT", "NOM_MUN", "NOM_LOC") %>%
  rename(cvegeo = CVEGEO, nom_ent = NOM_ENT, nom_mun = NOM_MUN, nom_loc = NOM_LOC) %>%
  as.data.frame() %>%
  select(-geometry)
read.locality <- data.table::rbindlist(list(read.locality.rur,read.locality.urb))

read.country <- st_read("national_estatal.shp") %>%
  select("CVEGEO", "NOMBRE", "OID") %>%
  rename(id = OID, cvegeo = CVEGEO, nom_ent = NOMBRE)
PolygonStats(read.country)
read.country.opti <- st_simplify(read.country, preserveTopology = TRUE, dTolerance = 0.02)
read.country.opti$geometry[!st_is_valid(read.country.opti)] <- st_make_valid(read.country.opti$geometry[!st_is_valid(read.country.opti)])
PolygonStats(read.country.opti)
topojson_write(read.country.opti, file = "mexico_estados.json")

read.municipality <- st_read("national_municipal.shp") %>%
  select("CVEGEO", "NOM_ENT", "NOM_MUN", "OID") %>%
  rename(id = OID, cvegeo = CVEGEO, nom_ent = NOM_ENT, nom_mun = NOM_MUN)
PolygonStats(read.municipality)
read.municipality.opti <- st_simplify(read.municipality, preserveTopology = TRUE, dTolerance = 0.02)
read.municipality.opti$geometry[!st_is_valid(read.municipality.opti)] <- st_make_valid(read.municipality.opti$geometry[!st_is_valid(read.municipality.opti)])
PolygonStats(read.country.opti)
topojson_write(read.municipality.opti, file = "mexico_municipios.json")

shapes.ageb.files <- list.files(
  path = "shps", recursive = T, pattern = "ageb_urb.shp", full.names = T
)

all.agebs <- purrr::map(shapes.ageb.files, st_read) %>%
  purrr::map(select, CVEGEO, OID) %>%
  purrr::map(rename, id = OID, cvegeo = CVEGEO)
all.agebs <- st_as_sf(data.table::rbindlist(all.agebs))
all.agebs <- mutate(all.agebs, semicvegeo = stringr::str_sub(cvegeo, 1,9)) %>%
  left_join(read.locality, by = c("semicvegeo" = "cvegeo")) %>%
  select(-semicvegeo)
all.agebs.opti <- st_simplify(all.agebs, preserveTopology = TRUE, dTolerance = 0.02)
all.agebs.opti$geometry[!st_is_valid(all.agebs.opti)] <- st_make_valid(all.agebs.opti$geometry[!st_is_valid(all.agebs.opti)])
all.agebs.opti <- topojson_write(all.agebs.opti, file = "mexico_agebs.json")
