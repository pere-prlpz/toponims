# script per afegir masia als que només tinguin P31 edifici

rm(list=ls())

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

edifwd <- getquery("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?idescat ?lat ?lon
WHERE {
  ?item wdt:P31 wd:Q41176.
  ?item wdt:P31 ?tipus.
  ?item wdt:P131* ?lloc.
  ?lloc wdt:P4335 ?idescat.
?item p:P625 ?coordinate .
?coordinate psv:P625 ?coordinate_node .
?coordinate_node wikibase:geoLatitude ?lat .
?coordinate_node wikibase:geoLongitude ?lon .
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca, oc, en, es, fr'.}
}")

#només edificis sense més P31
tep31 <- edifwd[!edifwd$tipusLabel %in% c("edifici","monument"),]
table(duplicated(tep31$item))
sort(table(tep31$tipusLabel))
sense <- edifwd[!edifwd$item %in% tep31$item,]

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
table(ngcatv10cs0f1r011$Concepte)

nomred <- ngcatv10cs0f1r011[ngcatv10cs0f1r011$Concepte=="edif." & ngcatv10cs0f1r011$CodiGeo=="10301",]

# canvi coordenades nomenclàtor
library(rgdal)
coords <- as.matrix(nomred[, grepl("UTM", colnames(nomred))])
sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=31N +datum=WGS84"))
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

# Els preparem per unir-los.
# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les) ","",nom)
  gsub("^l'","",nom)
}

nomid$nomrel <- treuart(tolower(nomid$Toponim))
sense$nomrel <- treuart(tolower(sense$itemLabel))

# unims
units <- merge(sense, nomid, by=c("nomrel", "idescat"))

# distància segons coordenades
distgeo <- function(lat.x, lon.x, lat.y, lon.y) {
  sqrt(((6371*(lat.x-lat.y)*pi/180)^2+(6371*cos(lat.x/180*pi)*(lon.x-lon.y)*pi/180)^2))
}

units$dist <- with(units, distgeo(lat.x, lon.x, lat.y, lon.y))
summary(units$dist)

crear <- units[units$dist < .4,]
crear <- crear[!duplicated(crear[,c("item")]),]

table(grepl("b[oóò]rda ", crear$nomrel))
table(grepl("^molí ", crear$nomrel))
sample(crear$itemLabe, 100)
crear <- crear[!grepl("^(b[oóò]rda|molí|molinet|molins) ", crear$nomrel),]

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- vector
  return(llista)
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr <- afegeix(instr, c(fila$item, "P31", "Q585956", "S248", "Q98463667")) #masia
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(6501:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)

# sortides per triar
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")

