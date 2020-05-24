#' Extrae poligonos AGEB para el MG 2010
#'
#' Extrae los archivos  SHP, DBF, SHX y PRJ para crear la base de poligonos del Marco Geoestadístico 2010.
#' Esta función lee una serie de archivos comprimidos en formato zip y extrae los poligonos. Puede aplicarse a uno
#' o varios archivos de estados.
#'
#' @param archivos.estados Archivos descargados de la página de Inegi en formato zip
#' @param ruta.guardar Ruta donde se guardaran los arcivos comrimidos.
#'
#' @return devuelve un objeto sf con la clave CVEGEO y el poligono.
#' @export
#'
#' @examples \dontrun{ExtraeAGEBMG2010(c("01_SCINCE_zip.zip"), "agebs/)}
ExtraeAGEBMG2010 <- function(archivos.estados, ruta.guardar) {
  todos.shp.agebs <- archivos.estados %>%
    purrr::map( ~ unzip(
      zipfile = paste0(ruta.guardar, .x),
      files = paste0(substr(.x, 1, 2), c("/ageb_urb.shp", "/ageb_urb.dbf", "/ageb_urb.shx", "/ageb_urb.prj")),
      exdir = paste0(ruta.guardar, "agebs/")
    )) %>%
  purrr::map( ~ sf::read_sf(.x[1])) %>%
    purrr::map( ~ dplyr::select(.x, CVEGEO)) %>%
    purrr::reduce(base::rbind) %>%
    dplyr::mutate(
      id_state = as.numeric(str_sub(CVEGEO, 1, 2)),
      id_mun = as.numeric(str_sub(CVEGEO, 3, 5)),
      id_loc = as.numeric(str_sub(CVEGEO, 6, 9)),
      clave_ageb = stringr::str_sub(CVEGEO, 10, 13)
    ) %>%
    dplyr::rename(ageb_clave = CVEGEO)
  return(todos.shp.agebs)
}

#' Extrae poligonos Localidad para el MG 2010
#'
#' Extrae los archivos  SHP, DBF, SHX y PRJ para crear la base de poligonos del Marco Geoestadístico 2010.
#' Esta función lee una serie de archivos comprimidos en formato zip y extrae los poligonos. Puede aplicarse a uno
#' o varios archivos de estados.
#'
#' @param localities.file Vector de archivos comprimidos en formato zip con la localidad
#' @param files.path Ruta donde se encuentran los archivos que seran leídos
#' @param save.path Ruta donde se salvaran los archivos descomprimidos
#'
#' @return Objeto sf con todas las localidades para todos los archivos zip procesados
#' @export
#'
#' @examples \dontrun{ExtraeLocalidadesMG2010(c("01_SCINCE_zip.zip"), "localidades/)}
ExtraeLocalidadesMG2010 <- function(localities.file, files.path, save.path) {
  unzip(
    zipfile = paste0(files.path, localities.file),
    files = c(
      paste0(substr(localities.file, 1, 2), c("/Loc_rur.shp", "/Loc_rur.shx", "/loc_rur.dbf", "/loc_rur.prj")),
      paste0(substr(localities.file, 1, 2), c("/loc_urb.shp", "/loc_urb.shx", "/loc_urb.dbf", "/loc_urb.prj"))
    ),
    exdir = paste0(save.path, "localidades/")
  )
  upper.case.pattern <- list.files(
    path = paste0(files.path, "localidades/", substr(localities.file, 1, 2)),
    pattern = "[L|l]oc_[rur|urb].[prj|shx|dbf|shp]",
    full.names = T
  )
  fix.lower.case.pattern <- gsub("/Loc", "/loc", upper.case.pattern)
  file.rename(upper.case.pattern, fix.lower.case.pattern)
  localities.all.shp <- fix.lower.case.pattern %>%
    purrr::keep(~ grepl(".shp", .x)) %>%
    purrr::map(read_sf) %>%
    purrr::reduce(plyr::rbind.fill) %>%
    dplyr::select(CVEGEO, NOM_ENT, NOM_MUN, NOM_LOC, CVE_AGEB, geometry) %>%
    purrr::mutate(
      id_state = as.numeric(str_sub(CVEGEO, 1, 2)),
      id_mun = as.numeric(str_sub(CVEGEO, 3, 5)),
      id_loc = as.numeric(str_sub(CVEGEO, 6, 9)),
      clave_ageb = str_sub(CVEGEO, 10, 13),
      NOM_LOC = iconv(NOM_LOC, 'latin1', 'UTF-8'),
      NOM_ENT = iconv(NOM_ENT, 'latin1', 'UTF-8'),
      NOM_MUN = iconv(NOM_MUN, 'latin1', 'UTF-8')
    ) %>%
    dplyr::rename(ageb_clave = CVEGEO, name_state = NOM_ENT, name_mun = NOM_MUN, name_loc = NOM_LOC, ageb_id = CVE_AGEB)
  return(localities.all.shp)
}
