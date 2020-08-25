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

#vegueries <- c("Q18265", "Q249461", "Q1030024", "Q1113384", "Q1113390", "Q1462520", "Q1849804", "Q579384")
#comamb <- c("Q12600", "Q13948", "Q14303", "Q15348", "Q15351")
partcat <- c("Q18265", "Q1030024", "Q1113384", "Q1113390", "Q1462520", "Q1849804", "Q579384",
             "Q12600", "Q13948", "Q14303", "Q15348", "Q15351")

# baixa tots els elements d'un lloc de Catalunya
totlloc <- function(qlloc) {
  url <- paste0("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131*wd:",qlloc,".
  ?item wdt:P31 ?tipus.
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
  tot <- getquery(url)
  tot <- tot[!duplicated(tot),]
  resum <- tot[nchar(tot$idescat)>2,]
  afegir <- tot[!(tot$item %in% resum$item),]
  resum <- rbind(resum, afegir)
  return(resum)
}

totcat <- data.frame()
for (lloc in partcat) {
  print(lloc)
  totcat <- rbind(totcat, totlloc(lloc))
}
totcat <- totcat[!duplicated(totcat),]

classeswd <- getquery('SELECT DISTINCT ?tipus ?nomtipus
WHERE {
?tipus wdt:P279* wd:Q811979.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca,ca,en,es,fr,eu,de,sv" .
?tipus rdfs:label ?nomtipus
}
}')

lloctipus <- merge(totcat, classeswd, by="tipus")

# veure els tipus d'edifici
sort(table(lloctipus$nomtipus), decreasing = TRUE)
length(table(lloctipus$nomtipus))
taula <- sort(table(lloctipus$nomtipus), decreasing = TRUE)
cumsum(taula)/sum(taula)
cbind(1:length(taula), taula, cumsum(taula), cumsum(taula)/sum(taula))

# alias
alies <- getquery("SELECT DISTINCT ?item ?alias
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:Q5705.
  ?item skos:altLabel ?alias.
}") 
aliesedif <- merge(lloctipus, alies, by="item")
aliesedif$itemLabel <- aliesedif$alias
aliesedif$alias <- NULL
lloctipus <- rbind(lloctipus, aliesedif)
lloctipus <- lloctipus[!duplicated(lloctipus),]

oficial <- getquery("SELECT DISTINCT ?item ?oficial
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:Q5705.
  ?item wdt:P1448 ?oficial.
}")
oficialedif <- merge(lloctipus, oficial, by="item")
oficialedif$itemLabel <- oficialedif$oficial
oficialedif$oficial <- NULL
lloctipus <- rbind(lloctipus, oficialedif)
lloctipus <- lloctipus[!duplicated(lloctipus),]
length(unique(lloctipus$item))

# busquem duplicats a Wikidata

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

#duplicats en els dos sentits
repetits <- function(x) {duplicated(x)|duplicated(x, fromLast = TRUE)}

repewd <- lloctipus[repetits(lloctipus[c("nomrel", "idescat")]),]
table(duplicated(repewd$item))
repewd <- repewd[!duplicated(repewd$item),]
repewd <- repewd[repetits(repewd[c("nomrel", "idescat")]),]
repewd <- repewd[order(repewd$nomrel),]

qplant <- function(qurl) {
  q <- gsub("http://www.wikidata.org/entity/Q", "", qurl)
  pl <- paste0("*{{Q|",q,"}}", collapse = "\n")
  return(pl)
}

# sortida incloent duplicats i homònims
# cat(qplant(repewd$item))
paste("wd", repewd$item, sep=":", collapse=" ")
cat(paste0("*{{Q|", repewd$item, "}} - ", repewd$nomrel, collapse = "\n"), "\n")

# busquem duplicats eliminant els homònims

homonims <- getquery('SELECT DISTINCT ?item ?itemLabel ?noconf ?noconfLabel
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P1889 ?noconf.
  ?item wdt:P131 [].
SERVICE wikibase:label {
bd:serviceParam wikibase:language "[AUTO_LANGUAGE],ca,oc,eu,gl,en,es,an,eu,pl,sv,ceb" .
}
}')

repenohom <- data.frame()
for (it1 in unique(repewd$item)) {
  print(it1)
  homon <- homonims$noconf[homonims$item == it1]
  propi <- repewd[repewd$item==it1, ]
  possibles <- repewd[repewd$idescat %in% propi$idescat,]
  possibles <- possibles[!possibles$item %in% homon,]
  possibles <- possibles[possibles$item != it1,]
  possibles <- possibles[possibles$nomrel %in% propi$nomrel,]
  propi <- propi[propi$nomrel %in% possibles$nomrel,]
  repenohom <- rbind(repenohom, propi, possibles)
}
repenohom <- repenohom[!duplicated(repenohom),]

# trec fosses comunes (homònimes que no marcaré)
repenohom <- repenohom[!grepl("^fossa comuna:",repenohom$nomrel),]

# per actualitzar (sortida sense homònims)
paste("wd", repenohom$item, sep=":", collapse=" ")
cat(paste0("*{{Q|", repenohom$item, "}} - ", repenohom$nomrel, " ", repenohom$item, collapse = "\n"), "\n")





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

# només edificis:
conceptes <- c("edif.", "edif. hist.")
nomred <- 
  nomred[nomred$Concepte %in% conceptes,]

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

# ara tenim tots els edificis de WD amb tots els noms (lloctipus) 
# i tots els edificis del fitxer de noms geogràfics (nomid)
# Els preparem per unir-los.

nomid$nomrel <- treuart(tolower(nomid$Toponim))

#1- unim per nom i municipi
units <- merge(lloctipus, nomid, by=c("nomrel","idescat"))

#veure
table(is.na(units$lat.x))
units[is.na(units$lat.x),]
table(duplicated(units[, names(units)[names(units)!="itemLabel"]]))

units <- units[!duplicated(units[, names(units)[names(units)!="itemLabel"]]),]

#busquem possibles duplicats

#duplicats en els dos sentits
repetits <- function(x) {duplicated(x)|duplicated(x, fromLast = TRUE)}

unitsrepe <- units[repetits(units[c("nomrel", "idescat")]),]
table(duplicated(unitsrepe$item))
unitsrepe <- unitsrepe[!duplicated(unitsrepe$item),]
unitsrepe <- unitsrepe[repetits(unitsrepe[c("nomrel", "idescat")]),]

qplant <- function(qurl) {
  q <- gsub("http://www.wikidata.org/entity/Q", "", qurl)
  pl <- paste0("*{{Q|",q,"}}", collapse = "\n")
  return(pl)
}

cat(qplant(unitsrepe$item))
paste("wd", unitsrepe$item, sep=":", collapse=" ")





with(units, table(nomtipus, Concepte))
units[units$nomtipus=="obra escultòrica",]
units[grep("estació", units$nomtipus),]
#pugem coordenades

#busquem els que són a ng i no a wd
#els pugem