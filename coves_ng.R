#scripts per importar el fitxer de noms geogràfics de l'ICGC i pujar-lo a Wikidata
rm(list=ls())

##############################################
# Carregar noms geogràfics per importar-los
###############################################

# treiem paràmetres dels noms
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  nom <- gsub("^(el|la|els|les|lo) ","",nom)
  gsub("^l'","",nom)
}


# #importar noms geogràfics de Catalunya
# if (file.exists("~/varis/ngcatv10cs0f1r011.txt")) {
#   ngcatv10cs0f1r011 <- read.csv("~/varis/ngcatv10cs0f1r011.txt", 
#                                 sep=";", colClasses = "character", 
#                                 encoding="WINDOWS-1252")
# } else {
#   ngcatv10cs0f1r011 <- read.csv("~/pere/diversos/ngcatv10cs0f1r011.txt", 
#                                 sep=";", colClasses = "character")
# }

library(readr)
ngcatv10cs0f1r011 <- read_delim("varis/ngcatv10cs0f1r011.txt",
                                delim = ";", escape_double = FALSE, col_types = cols(col_character()),
                                locale = locale(encoding = "WINDOWS-1252"),
                                trim_ws = TRUE)

ngcatv10cs0f1r011$UTMX_ETRS89 <- as.numeric(ngcatv10cs0f1r011$UTMX_ETRS89)
ngcatv10cs0f1r011$UTMY_ETRS89 <- as.numeric(ngcatv10cs0f1r011$UTMY_ETRS89)
summary(ngcatv10cs0f1r011)

nomred <- ngcatv10cs0f1r011
# de moment no elimino noms duplicats dins del mateix municipi

nomred <- nomred[nomred$CodiGeo %in% c("50801","50802","50803"),]

# canvi coordenades nomenclàtor
library(dplyr)
library(ggmap)
library(rgdal)

east<-nomred$UTMX_ETRS89
north<-nomred$UTMY_ETRS89
utm <- SpatialPoints(cbind(east,north), 
                     proj4string=CRS("+proj=utm +zone=31 +datume=WGS84 "))
spgeo <- spTransform(utm, CRS("+proj=longlat +datum=WGS84"))
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

nomid$nomrel <- sapply(nomid$Toponim, treupar)
nomid$nomrel <- treuart(tolower(nomid$nomrel))

##################
## Carregar de Wikidata
######################

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

# importem coves de Wikidata
totcat <- getquery("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31 ?tipus.
  ?tipus wdt:P279* wd:Q35509.
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
aliesedif <- aliesedif[!duplicated(aliesedif),]
lloctipus <- rbind(lloctipus, aliesedif)
lloctipus <- lloctipus[!duplicated(lloctipus),]

lloctipus$nomnet <- sapply(lloctipus$itemLabel, treupar)
lloctipus$nomrel <- treuart(tolower(lloctipus$nomnet))

##################################################################
# Buscar els que no siguin a Wikidata per pujar-los
##############################################################

#1- unim per nom i municipi
units <- merge(lloctipus, nomid, by=c("nomrel","idescat"))

#veure
table(is.na(units$lat.x))
units[is.na(units$lat.x),]
table(duplicated(units[, names(units)[names(units)!="itemLabel"]]))

# traiem els que siguin duplicats només per tenir dos noms
units <- units[!duplicated(units[, names(units)[names(units)!="itemLabel"]]),]


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
leafwd <- leafwd[!duplicated(leafwd$item),]

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

##################################################################
# Carregar
##############################################################

#afegir "de"
de <- function(nom) {
  if (grepl("^els? ", nom)) {
    denom <- paste0("d",nom)
  } else {
    if (grepl("^[AEIOUÀÈÉÍÒÓÚ]", nom)) {
      denom <- paste0("d'",nom)
    } else {
      denom <- paste0("de ",nom)
    }
  }
  return(denom)
}

#afegir "a"
a <- function(nom) {
  if (grepl("^els? ", nom)) {
    denom <- gsub("^el", "al", nom)
  } else {
    denom <- paste0("a ",nom)
  }
  return(denom)
}

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- vector
  return(llista)
}

# preparar crear

crear <- merge(crear, idescat, all.x=TRUE)

crear <- crear[!is.na(crear$nomrel),]

#treure els uqe no convenen
#crear[grepl("^institut",crear$nomrel),]
#crear <- crear[!grepl("^institut",crear$nomrel),]

terme <- c("50801"="avenc",
            "50802"="balma",
            "50803"="cova")

termeen <- c("50801"="pit cave",
            "50802"="rock shelter",
            "50803"="cave")

qinst <- c("50801"="Q1435994",
              "50802"="Q1149405",
              "50803"="Q35509")

crear$terme <- terme[crear$CodiGeo]
crear$termeen <- termeen[crear$CodiGeo]
crear$qinst <- qinst[crear$CodiGeo]


# preparar quickstatemens
quick <- function(quadre) {
  fila <- quadre[1,]
  instr <- list(c("CREATE"))
  instr <- afegeix(instr,c("LAST", "Lca", cometes(fila$Toponim)))  
  instr <- afegeix(instr,c("LAST", "Dca", 
                           cometes(paste(fila$terme,
                                         a(fila$llocLabel)))))  
  instr <- afegeix(instr, c("LAST", "P31", fila$qinst, "S248", "Q98463667"))  
  instr <- afegeix(instr, c("LAST", "P131", fila$lloc, "S248", "Q98463667"))  
  instr <- afegeix(instr, c("LAST", "P625", 
                            paste0("@", fila$lat,"/", fila$lon),
                            "S248", "Q98463667"))  
  instr <- afegeix(instr, c("LAST", "P17", "Q29"))  
  instr <- afegeix(instr, c("LAST", "Len", cometes(fila$Toponim)))  
  instr <- afegeix(instr, c("LAST", "Loc", cometes(fila$Toponim)))  
  instr <- afegeix(instr, c("LAST", "Leu", cometes(fila$Toponim)))  
  instr <- afegeix(instr, c("LAST", "Lfr", cometes(fila$Toponim)))  
  instr <- afegeix(instr, c("LAST", "Lpt", cometes(fila$Toponim)))  
  instr <- afegeix(instr, c("LAST", "Lde", cometes(fila$Toponim)))  
  instr <- afegeix(instr, c("LAST", "Den", 
                            paste0('"',fila$termeen,' in ', fila$llocLabel,
                                   " (Catalonia)",'"')))  
  if (nrow(quadre)>1) {
    for (i in 2:nrow(quadre)) {
      instr <- afegeix(instr, c("LAST", "P131", quadre[i,]$lloc, 
                                "S248", "Q98463667"))
    }
  }
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

# preparar instruccions
idmons <- unique(crear$id)
instruccions <- unlist(lapply(idmons, function(i) {quick(crear[crear$id==i,])})) 

# sortides per triar
#cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/varis/instruccions.txt")
#cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")

