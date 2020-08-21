#scripts per importar el fitxer de noms geogràfics de l'ICGC i pujar-lo a Wikidata

rm(list=ls())

## script per funcionar

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
# no tenim en compte els que tinguin el mateix nom que un altre element del mateix municipi
duplicats <- duplicated(nomred[,c("Toponim","CodiMun1")])|duplicated(nomred[,c("Toponim","CodiMun1")], fromLast = TRUE)
nomred <- nomred[!duplicats,]

#conceptes_pob <- c("edif.", "edif. hist.")
#nomred <- 
#  index_nomenclator_2009[index_nomenclator_2009$Concepte %in% conceptes_pob, 1:13]

# canvi coordenades nomenclàtor
library(rgdal)
coords <- as.matrix(nomred[, grepl("UTM", colnames(nomred))])
sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=31N +datum=WGS84")) #aquest funciona (pel Papiol)
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo.df <- as.data.frame(spgeo)
names(spgeo.df) <- c("lon", "lat")
nomred <- cbind(nomred, spgeo.df)


# carregar de Wikidata
#funcions carregar topònims
library(httr)
library(rjson)

treuvar <- function(var, bind) {
  sapply(bind, function(x) (x[[var]]$value))
}

desllista <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(unlist(x))
}

getsparql <- function(url, coornum=TRUE) {
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
  return(df)
}

nomwd <- getsparql("https://query.wikidata.org/sparql?query=%23%20llocs%20amb%20coordenades%20referenciades%20al%20Nomencl%C3%A0tor%0ASELECT%20%3Fitem%20%3FitemLabel%20%3Finst%20%3FinstLabel%20%3Fmun%20%3FmunLabel%20%3Fidescat%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20p%3AP625%20%3Fcoordinate.%0A%20%20%3Fcoordinate%20prov%3AwasDerivedFrom%20%3Fref.%0A%20%20%3Fref%20pr%3AP248%20wd%3AQ11938912.%0A%20%20%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node.%0A%20%20%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat.%0A%20%20%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon.%0A%20%20OPTIONAL%20%7B%3Fitem%20wdt%3AP31%20%3Finst%7D%0A%20%20OPTIONAL%20%7B%3Fitem%20wdt%3AP131*%20%3Fmun.%0A%20%20%20%20%20%20%20%20%20%20%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20%20%20%20%20%20%20%20%20%20%20%3Fmun%20wdt%3AP4335%20%3Fidescat%7D%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Coc%2Cen%2Ces%2Cfr%22.%20%7D%0A%7D")

#preparem per unir

# nom sense desambiguar
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les) ","",nom)
  gsub("^l'","",nom)
}

nomwd$nomnet <- sapply(nomwd$itemLabel, treupar)

# noms per enganxar
nomwd$nomrel <- treuart(tolower(nomwd$nomnet))
nomred$nomrel <- treuart(tolower(nomred$Toponim))
nomred$ref <- 1:nrow(nomred)

#1- unim per nom i municipi i comprovem coordenades
units <- merge(nomwd, nomred, by.x=c("nomrel", "idescat"), by.y=c("nomrel", "CodiMun1"), all=FALSE)

table(duplicated(units[,1:2]))

# distància segons coordenades
distgeo <- function(lat.x, lon.x, lat.y, lon.y) {
  sqrt(((6371*(lat.x-lat.y)*pi/180)^2+(6371*cos(lat.x/180*pi)*(lon.x-lon.y)*pi/180)^2))
}

units$dist <- with(units, distgeo(lat.x, lon.x, lat.y, lon.y))
summary(units$dist)
hist(log10(units$dist), breaks="scott")
with(units, table(instLabel, Concepte))

# elimino aparellaments entre coses diferents 
units <- units[!(units$Concepte=="edif." & units$instLabel %in% c("disseminat", "assentament humà")),]
units <- units[!(units$Concepte=="hidr." & !grepl("^(llac|font|badia|riu)",units$instLabel)),]
units <- units[!(grepl("^(cim|orogr.)", units$Concepte) & !grepl("^(muntanya)", units$instLabel)),]


# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- vector
  return(llista)
}


# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr <- afegeix(instr, c(paste0("-",fila$quitem), "P625", paste0("@", fila$lat.x,"/", fila$lon.x)))
  instr <- afegeix(instr, c(fila$quitem, "P625", paste0("@", fila$lat.y,"/", fila$lon.y),
                            "S248", "Q98463667"))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}


# filtrem quins elements volem
#crear <- units[units$dist>.2,] #tot
#crear <- units[units$dist>.075 & units$Concepte=="hidr." & units$instLabel=="font",]
crear <- units[units$dist>.075 & grepl("edif|equip",units$Concepte) & grepl("edif|masia|casa|granja|església|castell",units$instLabel),] #FALTEN ESGLÉSIES, etc.

crear$quitem <- gsub("http://www.wikidata.org/entity/", "", crear$item)
crear <- crear[!duplicated(crear$quitem),]  #trec duplicats per tenir dos municipis o dos P31 a Wikidata

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)

# sortides per triar
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")

### script per anàlisi del que fa

# mapes per comparar
library(leaflet)
mapa <- function(df) {
  m <- leaflet()
  m <- addTiles(m)
  m <- addMarkers(m, lng=df$lon, lat=df$lat, popup=df$Toponim)
  return(m)
}

papiol <- nomred[nomred$NomMun1=="el Papiol",]

mapa(papiol)
View(papiol[order(papiol$Toponim),])

alins <- nomred[nomred$NomMun1=="Alins",]
mapa(alins)

bas <- nomred[nomred$CodiMun1=="172076",]
mapa(bas)

units <- units[order(units$dist),]

qplant <- function(qurl) {
  q <- gsub("http://www.wikidata.org/entity/Q", "", qurl)
  pl <- paste0("*{{Q|",q,"}}", collapse = "\n")
  return(pl)
}

cat(qplant(tail(units$item,20)))

mapa(tail(units, 20))

mapa2 <- function(df) {
  m <- leaflet()
  m <- addTiles(m)
  m <- addCircleMarkers(m, lng=df$lon.x, lat=df$lat.x, popup=paste(df$itemLabel,df$munLabel, df$instLabel, sep="; "))
  m <- addCircleMarkers(m, lng=df$lon.y, lat=df$lat.y, 
                        popup=paste(df$Toponim, df$NomMun1, df$Concepte, round(df$dist, 3), sep="; "), color = "red")
  for (i in 1:nrow(df)) {
    m <- addPolylines(m, lat=c(df$lat.x[i], df$lat.y[i]), lng=c(df$lon.x[i], df$lon.y[i]))
  }
  return(m)
}

mapa2(tail(units,50))

table(duplicated(nomred[,c("Toponim","CodiMun1")]))

