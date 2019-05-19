library(sf)
library(foreign)
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
      files = c(
        paste0(substr(.x, 1, 2), "/ageb_urb.shp"),
        paste0(substr(.x, 1, 2), "/ageb_urb.dbf"),
        paste0(substr(.x, 1, 2), "/ageb_urb.shx"),
        paste0(substr(.x, 1, 2), "/ageb_urb.prj")
      ),
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
    rename(ageb_clave = CVEGEO)
  toc()
  return(todos.shp.agebs)
}
