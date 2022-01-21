#scripts per importar el fitxer de noms geogràfics de l'ICGC i pujar-lo a Wikidata
rm(list=ls())

#carregar de Wikidata tots els elements de Catalunya amb P131 
library(httr)
library(rjson)

treuvar <- function(var, bind) {
  sapply(bind, function(x) (x[[var]]$value))
}

desllista <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(unlist(x))
}

getsparql <- function(url, treuurl=TRUE, coornum=TRUE) {
  cont <- fromJSON(rawToChar(content(GET(url))))
  nomsvars <- cont$head$vars
  llista <- lapply(nomsvars, treuvar, bind=cont$results$bindings)
  names(llista) <- nomsvars
  llista <- lapply(llista, desllista)
  df <- as.data.frame(llista, stringsAsFactors = FALSE)
  if (coornum) {
    if ("lat" %in% colnames(df)) {
      df$lat <- as.numeric(df$lat)
    }
    if ("lon" %in% colnames(df)) {
      df$lon <- as.numeric(df$lon)
    }
  }
  if (treuurl) {
    for (var in names(df)) {
      if (class(df[[var]])=="character") {
        df[[var]] <- gsub("http://www.wikidata.org/entity/", "", df[[var]])
      }
    }
  }
  return(df)
}

getquery <- function(query, treuurl=TRUE, coornum=TRUE) {
  url <- paste0("https://query.wikidata.org/sparql?query=", URLencode(query))
  return(getsparql(url, treuurl, coornum))
}

totcat <- getquery("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  VALUES ?tipuspont {wd:Q12280 wd:Q474}
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31 ?tipus.
  ?tipus wdt:P279* ?tipuspont.
    ?item wdt:P131* ?mun.
    ?mun wdt:P4335 ?idescat.
  OPTIONAL {
?item p:P625 ?coordinate .
?coordinate psv:P625 ?coordinate_node .
?coordinate_node wikibase:geoLatitude ?lat .
?coordinate_node wikibase:geoLongitude ?lon .
  }
SERVICE wikibase:label {
bd:serviceParam wikibase:language 'ca,oc,en,es,pl,sv,ceb'.
}
}")

table(duplicated(totcat))
totcat <- totcat[!duplicated(totcat),]

table(duplicated(totcat$item))

lloctipus <- totcat[nchar(totcat$idescat)>2,] 
resta <- totcat[nchar(totcat$idescat)<=2,]
table(resta$item %in% lloctipus$item)
resta[!resta$item %in% lloctipus$item,]

# treiem paràmetres dels noms
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les) ","",nom)
  gsub("^l'","",nom)
}

lloctipus$nomnet <- sapply(lloctipus$itemLabel, treupar)
lloctipus$nomrel <- treuart(tolower(lloctipus$nomnet))

##############################################
# Carregar noms geogràfics per importar-los
###############################################

#importar noms geogràfics de Catalunya
if (file.exists("~/DADES/pere/varis/ngcatv10cs0f1r011.txt")) {
  ngcatv10cs0f1r011 <- read.csv("~/DADES/pere/varis/ngcatv10cs0f1r011.txt", 
                                sep=";", colClasses = "character")
} else {
  ngcatv10cs0f1r011 <- read.csv("~/pere/diversos/ngcatv10cs0f1r011.txt", 
                                sep=";", colClasses = "character")
}
ngcatv10cs0f1r011$UTMX_ETRS89 <- as.numeric(ngcatv10cs0f1r011$UTMX_ETRS89)
ngcatv10cs0f1r011$UTMY_ETRS89 <- as.numeric(ngcatv10cs0f1r011$UTMY_ETRS89)
summary(ngcatv10cs0f1r011)

nomred <- ngcatv10cs0f1r011
# de moment no elimino noms duplicats dins del mateix municipi


# només ponts:
nomred$Toponim[nomred$CodiGeo==30509]
nomred$Toponim[nomred$CodiGeo==40411]

nomred <- nomred[nomred$CodiGeo==30509 | 
                   (nomred$CodiGeo==40411 & 
                      grepl("pont|aqüeducte",tolower(nomred$Toponim))),]

# canvi coordenades nomenclàtor
library(rgdal)
coords <- as.matrix(nomred[, grepl("UTM", colnames(nomred))])
sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=31N +datum=WGS84")) #aquest funciona (pel Papiol)
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo.df <- as.data.frame(spgeo)
names(spgeo.df) <- c("lon", "lat")
nomred <- cbind(nomred, spgeo.df)

# separem en registres diferents els diferents municipis de cada edifici
nomred$id <- 1:nrow(nomred)
varmuns <- names(nomred)[grepl("CodiMun", names(nomred))]
vartreure <- names(nomred)[grepl("(Codi|Nom)(Mun|Com)", names(nomred))]
nomid <- data.frame()
for (varmun in varmuns) {
  nomi <- nomred[nomred[[varmun]]!="",]
  nomi$idescat <- nomi[[varmun]]
  nomi <- nomi[, names(nomi)[!names(nomi) %in% vartreure]]
  nomid <- rbind(nomid, nomi)
}

# ara tenim tots els ponts de WD amb tots els noms (lloctipus) 
# i tots els ponts del fitxer de noms geogràfics (nomid)
# Els preparem per unir-los.

nomid$nomrel <- treuart(tolower(nomid$Toponim))

#1- unim per nom i municipi
units <- merge(lloctipus, nomid, by=c("nomrel","idescat"))

#veure
table(is.na(units$lat.x))
units[is.na(units$lat.x),]
table(duplicated(units[, names(units)[names(units)!="itemLabel"]]))

# traiem els que siguin duplicats només per tenir dos noms
units <- units[!duplicated(units[, names(units)[names(units)!="itemLabel"]]),]

##################################################################
# Buscar els que no siguin a Wikidata per pujar-los
##############################################################

quedenng <- nomid[!nomid$id %in% units$id,]
quedenwd <- lloctipus

#codi municipi
idescat <- getquery("SELECT ?lloc ?llocLabel ?idescat 
WHERE {
  ?lloc wdt:P4335 ?idescat.
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca' .}
}")

# eliminem duplicats (segurament doble municipi)
quedenng <- quedenng[!duplicated(quedenng$id,),]

# eliminem repetits a municipis diferents (mateix nom, mateixa comarca i poca distància)
muncom <- unique(ngcatv10cs0f1r011[ngcatv10cs0f1r011$CodiMun1 != "", c("CodiMun1","CodiCom1")])
muncom <- muncom[!duplicated(muncom$CodiMun1),]
names(muncom) <- c("idescat","comarca")

crear <- quedenng
leafng <- crear
leafwd <- quedenwd
leafwd <- leafwd[!duplicated(leafwd$id),]

# desaparellats de wikidata en vermell, nomenclàtor en blau
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addCircleMarkers(m, lng=leafng$lon, lat=leafng$lat, popup=leafng$Toponim)
m <- addCircleMarkers(m, lng=leafwd$lon, lat=leafwd$lat, popup=leafwd$itemLabel, color = "red")
#m <- addCircleMarkers(m, lng=leafng$lon, lat=leafng$lat, popup=as.character(leafng$id), color="green")
m <- addRectangles(m, lng1=min(leafwd$lon, na.rm=TRUE), 
                   lng2=max(leafwd$lon, na.rm=TRUE),
                   lat1=min(leafwd$lat, na.rm=TRUE), 
                   lat2=max(leafwd$lat, na.rm=TRUE),
                   fill=FALSE, color="red")
m


