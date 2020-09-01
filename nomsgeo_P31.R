#scripts per importar el fitxer de noms geogràfics de l'ICGC i pujar-lo a Wikidata
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

#vegueries <- c("Q18265", "Q249461", "Q1030024", "Q1113384", "Q1113390", "Q1462520", "Q1849804", "Q579384")
#comamb <- c("Q12600", "Q13948", "Q14303", "Q15348", "Q15351")
partcat <- c("Q18265", "Q1030024", "Q1113384", "Q1113390", "Q1462520", "Q1849804", "Q579384",
             "Q12600", "Q13948", "Q14303", "Q15348", "Q15351")

# baixa tots els elements d'un lloc de Catalunya, incloent sense tipus
# REVISAR: TREURE ESTAT PER NO PERDRE ELS QUE NO TENEN NI AIXÒ
totlloc <- function(qlloc) {
  url <- paste0("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131*wd:",qlloc,".
  OPTIONAL {?item wdt:P31 ?tipus.}
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

#carregar de Wikidata tots els elements de Catalunya amb P131,incloent sense P31
totcat <- data.frame()
for (lloc in partcat) {
  print(lloc)
  totcat <- rbind(totcat, totlloc(lloc))
}
totcat <- totcat[!duplicated(totcat),]

head(totcat)

table(is.na(totcat$tipus))

sense <- totcat[is.na(totcat$tipus),]

# busco duplicats (els sense P31 que siguin duplicats d'un altre que sí que tingui P31)

autounits <- merge(sense, totcat[!is.na(totcat$tipus),], by=c("idescat","itemLabel"))

# distància segons coordenades
distgeo <- function(lat.x, lon.x, lat.y, lon.y) {
  sqrt(((6371*(lat.x-lat.y)*pi/180)^2+(6371*cos(lat.x/180*pi)*(lon.x-lon.y)*pi/180)^2))
}

autounits$dist <- with(autounits, distgeo(lat.x, lon.x, lat.y, lon.y))
summary(autounits$dist)

qplant <- function(text) {
  paste0("{{Q|",substring(text,2),"}}")
}

cat(with(autounits,
     paste("*", qplant(item.x), qplant(item.y), qplant(tipus.y), dist, collapse="\n")))

# mirem els no duplicats per tractar-los amb els noms geogràfics
table(sense$item %in% autounits$item.x)
sense <- sense[!sense$item %in% autounits$item.x,]

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
units$dist <- with(units, distgeo(lat.x, lon.x, lat.y, lon.y))
summary(units$dist)

crear <- units[units$dist < .65,]

table(crear$Concepte)
table(crear$CodiGeo)
cat(paste(sort(unique(crear$CodiGeo)),collapse="\n"))

# REVISAR: 50601 (rasos)
dcodis <- "
10000 Q15303838
10106 Q11954567
10301 Q585956
10401 Q13231610
40203 Q216107
40405 Q200141
40508 Q3918
40702 Q108325
40809 Q22746
40826 Q22698
41301 Q17172602
50102 Q2624046
50203 Q8502
50302 Q133056
50303 Q3573216
50304 Q11914211
50306 Q150784
50401 Q11924662
50501 Q1174791
50601 Q8502
50602 Q160091
50701 Q3323149
50803 Q35509
50901 Q4897108
50902 Q4421
50903 Q3547603
50904 Q1616093
50905 Q11914069
50906 Q1350230
50907 Q143970
50908 Q6017969
60108 Q3253281
60201 Q6501028
60304 Q34038"

qcodis <- as.data.frame(matrix(scan(text=dcodis, what="character"), byrow=TRUE, ncol=2), stringsAsFactors=FALSE)
names(qcodis) <- c("CodiGeo", "qtipus")
table(crear$CodiGeo %in% qcodis$CodiGeo)
crear$CodiGeo[!crear$CodiGeo %in% qcodis$CodiGeo]
crear[!crear$CodiGeo %in% qcodis$CodiGeo,]
crear <- merge(crear, qcodis, by="CodiGeo")
head(crear)
table(duplicated(crear[,c("item","qtipus")]))
crear <- crear[!duplicated(crear[,c("item","qtipus")]),]

table(crear$qtipus)

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
  instr <- afegeix(instr, c(fila$item, "P31", fila$qtipus, "S248", "Q98463667"))  
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)

# sortides per triar
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")

