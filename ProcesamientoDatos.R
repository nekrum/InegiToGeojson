library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(tictoc)


DescargaArchivosINEGI <- function(ruta.web, archivos.scince, ruta.guardar) {
  tic("Descarga de archivos INEGI")
  archivos.existentes <- list.files(ruta.guardar)
  archivos.necesarios <- archivos.scince[!archivos.scince %in% archivos.existentes]
  if(length(archivos.necesarios) > 0) {
    download.file(paste0(ruta.web, archivos.necesarios), paste0(ruta.guardar, archivos.necesarios), method = 'libcurl')
  }
  toc()
}

ExtraerPoligonosAGEB <- function(archivos.estados, ruta.guardar) {
  tic("Extraer y crear tabla de AGEBS")
  todos.shp.agebs <- archivos.estados %>%
    map( ~ unzip(
      zipfile = paste0(ruta.guardar, .x),
      files = paste0(substr(.x, 1, 2), c("/ageb_urb.shp", "/ageb_urb.dbf", "/ageb_urb.shx", "/ageb_urb.prj")),
      exdir = paste0(ruta.guardar, "agebs/")
    )) %>%
    map( ~ read_sf(.x[1])) %>%
    map( ~ select(.x, CVEGEO)) %>%
    reduce(rbind) %>%
    mutate(
      id_state = as.numeric(str_sub(CVEGEO, 1, 2)),
      id_mun = as.numeric(str_sub(CVEGEO, 3, 5)),
      id_loc = as.numeric(str_sub(CVEGEO, 6, 9)),
      clave_ageb = str_sub(CVEGEO, 10, 13)
    ) %>%
    dplyr::rename(ageb_clave = CVEGEO)
  toc()
  return(todos.shp.agebs)
}

ExtractLocalitiesINEGI <- function(localities.file, files.path, ) {
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
    keep(~ grepl(".shp", .x)) %>%
    map(read_sf) %>%
    reduce(rbind.fill) %>%
    select(CVEGEO, NOM_ENT, NOM_MUN, NOM_LOC, CVE_AGEB, geometry) %>%
    mutate(
      id_state = as.numeric(str_sub(CVEGEO, 1, 2)),
      id_mun = as.numeric(str_sub(CVEGEO, 3, 5)),
      id_loc = as.numeric(str_sub(CVEGEO, 6, 9)),
      clave_ageb = str_sub(CVEGEO, 10, 13)
    ) %>%
    dplyr::rename(ageb_clave = CVEGEO, name_state = NOM_ENT, name_mun = NOM_MUN, name_loc = NOM_LOC, ageb_id = CVE_AGEB)
}

