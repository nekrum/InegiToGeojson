library(RFrog)
source("~/analisis_aislados/INEGI_agebs/ProcessData.R")
SetUser("calejero", "Lattakia_83", T)


QUERY_AGEBS <- c("SELECT * FROM data.ageb_target_distribution")
web.path <- "https://www.inegi.org.mx/contenidos/masiva/indicadores/inv/"
scince.files <- paste0(sprintf("%02d",0:32), "_SCINCE_zip.zip")
save.path <- "~/analisis_aislados/INEGI_agebs/SCIENCE/"
states.files <- scince.files[-1]
DescargaArchivosINEGI(web.path, scince.files, save.path)
agebs.all.shp <- ExtraerPoligonosAGEB(states.files, save.path)
ageb.target.distribution <- Select(QUERY_AGEBS)
setDT(ageb.target.distribution)
ageb.target.distribution[, ageb.construido := paste0(sprintf("%02d",id_state), sprintf("%03d",id_mun), sprintf("%04d",id_loc), sprintf("%04s", ageb_id))]
ageb.target.distribution[ageb_clave != ageb.construido]
ageb.target.distribution[, .(
  N.orig.in.inegi = .SD[ageb_clave %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.orig.not.inegi = .SD[!ageb_clave %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.newageb.in.inegi = .SD[ageb.construido %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.newageb.not.inegi = .SD[!ageb.construido %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.orig.in.inegi.notcoincidence = .SD[ageb_clave != ageb.construido & ageb_clave %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.orig.not.inegi.notcoincidence = .SD[ageb_clave != ageb.construido & !ageb_clave %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.newageb.in.inegi.notcoincidence = .SD[ageb_clave != ageb.construido & ageb.construido %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.newageb.not.inegi.notcoincidence = .SD[ageb_clave != ageb.construido & !ageb.construido %in% unique(agebs.all.shp$ageb_clave), uniqueN(ageb_clave)],
  N.orig. = uniqueN(ageb_clave),
  N.newageb = uniqueN(ageb.construido),
  N.diff.orig.new = .SD[ageb_clave != ageb.construido, .N]
)]

ageb.target.distribution[!ageb_clave %in% unique(agebs.all.shp$ageb_clave),]

localidades.rurales <- read.dbf("analisis_aislados/INEGI_agebs")
setDT(localidades.rurales)
localidades.rurales[, NOM_LOC := iconv(NOM_LOC, 'latin1', 'UTF-8')]
localidades.rurales[, OID := NULL]
localidades.urbanas <- read.dbf("analisis_aislados/INEGI_chekcs/Localidades_urbanas_2010_5.dbf")
setDT(localidades.urbanas)
localidades.urbanas[, NOM_LOC := iconv(NOM_LOC, 'latin1', 'UTF-8')]
localidades.urbanas[, OID := NULL]
localidades.inegi <- rbindlist(list(rurares = localidades.rurales, urbanas = localidades.urbanas), idcol = "tipo", fill = T)
setnames(localidades.inegi, c("CVE_ENT", "CVE_MUN", "CVE_AGEB", "CVE_LOC", "NOM_LOC"), c("id_state", "id_mun", "ageb_rural","id_loc", "name_loc_inegi"))
localidades.inegi[, c("id_state", "id_mun", "id_loc") := lapply(.(id_state, id_mun, id_loc), as.numeric)]


ageb.target.distribution <- merge(ageb.target.distribution, localidades.inegi, by = c("id_state", "id_mun", "id_loc"), all.x = T)
ageb.target.distribution[ageb_clave != ageb.construido]

localidades.file <- "00_SCINCE_zip.zip"
localidades.path <- "~/analisis_aislados/INEGI_agebs/SCIENCE/"

ageb.all.names <- ExtractLocalitiesINEGI(localidades.file, localidades.path)
