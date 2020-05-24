#' Descarga Marco Geostadístico
#'
#' Descarga el marco Geoestadístico del INEGI, puede hacerlo sin repetir la descarga
#' de archivos.
#'
#' Existen varios marcos geostadísticos en el INEGI. Esta función
#' permite descargar los diferentes marcos a partir de una url y una lista de los
#' nombres de los mismos.
#'
#' Por ejemplo para el marco SCIENCE 2010 la url base es
#' https://www.inegi.org.mx/contenidos/masiva/indicadores/inv/
#'
#' y los nombres de los archivos para los 32 estados se pueden generar:
#'
#' paste0(sprintf("%02d",0:32), "_SCINCE_zip.zip")
#'
#' Para el marco 2019 se puede usar la url
#'
#' http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463776079/
#'
#' Y generar los archvivos se puede hacer a partir de este ejemplo:
#'
#' 01_aguascalientes.zip
#'
#' Suplantando el estado por el estado que nos interesa.
#'
#'
#' @param ruta.web
#' @param archivos.geoinfo
#' @param ruta.guardar
#'
#' @return Un directorio con todos los archivos descargados del portal INEGI
#' @export
#'
#' @examples DescargaArchivosINEGI(
#'  ruta.web = 'https://www.inegi.org.mx/contenidos/masiva/indicadores/inv/',
#'  archivos.geoinfo = '01_SCIENCE_zip.zip',
#'  ruta.guardar = '/tmp/'
#')
DescargaArchivosINEGI <- function(ruta.web, archivos.geoinfo, ruta.guardar) {
  archivos.existentes <- list.files(ruta.guardar)
  archivos.necesarios <- archivos.geoinfo[!archivos.geoinfo %in% archivos.existentes]
  if(length(archivos.necesarios) > 0) {
    download.file(
      paste0(ruta.web, archivos.necesarios),
      paste0(ruta.guardar, archivos.necesarios),
      method = 'libcurl'
    )
  }
}

#' Descarga el Catálogo de Claves
#'
#' El Catálogo Único de Claves de Áreas Geoestadísticas Estatales, Municipales y Localidades
#' es la referencia para la extructura  nombres de las claves geoestadísticas del Inegi.
#'
#' Contiene la clave individual y general para cada localidad, municipio y estado. Ademas
#' de su coordenada geográfica y nombre con acentos.
#'
#' @return Data frame con todos los datos de las claves únicas de acceso al INEGI
#' @export
#'
#' @examples DescargaCatalogo("/tmp/")
DescargaCatalogo <- function(ruta.guardar = "descargas/") {
  catalogo.url <- "https://www.inegi.org.mx/app/api/ageeml/Catuni/ObtenerExpLOC/?fecha=01/04/2020&tipo=2&union=0&formato=03&cod=1"
  if(dir.exists("descargas")) {
    download.file(catalogo.url, paste0(ruta.guardar, "catalogo.csv"))
  } else {
    stop("El directorio no existe")
  }
  catalogo.geo <- data.table::fread(paste0(ruta.guardar, "/catalogo.csv"))
  return(catalogo.geo[])
}



