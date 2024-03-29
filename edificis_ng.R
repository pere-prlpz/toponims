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
partcat <- c("Q18265", "Q1030024", "Q1113384", "Q1113390", 
             #"Q1462520",#ponent
             "Q12732","Q12726","Q12728","Q12733","Q12727","Q12729",
             "Q1849804", "Q579384",
             "Q12600", "Q13948", "Q14303", "Q15348", "Q15351","Q15352") 

# baixa tots els elements d'un lloc de Catalunya
totlloc <- function(qlloc) {
  url <- paste0("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:",qlloc,".
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

monuments <- getquery("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P1600 [].
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

totcat <- rbind(totcat, monuments)
totcat <- totcat[!duplicated(totcat),]

# trec fosses comunes, que no surten al fitxer de noms geogràfics
totcat <- totcat[-grep("^Fossa comuna:", totcat$itemLabel),]

classeswd <- getquery('SELECT DISTINCT ?tipus ?nomtipus
WHERE {
  VALUES ?generes {wd:Q811979 wd:Q839954}
?tipus wdt:P279* ?generes.
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
aliesedif <- aliesedif[!duplicated(aliesedif),]
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
lloctipus <- lloctipus[nchar(lloctipus$idescat)>2,] # això potser s'hauria d'haver fet abans

##############################################
# Carregar noms geogràfics per importar-los
###############################################

#importar noms geogràfics de Catalunya
if (file.exists("~/DADES/pere/varis/ngcatv10cs0f1r011.txt")) {
  ngcatv10cs0f1r011 <- read.csv("~/DADES/pere/varis/ngcatv10cs0f1r011.txt", 
                                sep=";", colClasses = "character")
} else if (file.exists("~/pere/diversos/ngcatv10cs0f1r011.txt")) {
  ngcatv10cs0f1r011 <- read.csv("~/pere/diversos/ngcatv10cs0f1r011.txt", 
                                sep=";", colClasses = "character")
} else {
  ngcatv10cs0f1r011 <- read.csv("~/varis/ngcatv10cs0f1r011.txt", 
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

# hi afegim tipus concret
dcodis <- "
10000 Q15303838
10105 Q674950
10106 Q11954567
10301 Q585956
10302 Q136689
10401 Q13231610
10402 Q858177
10403 Q1350230
10404 Q143970
20203 Q131596
20802 Q1068383
20805 Q1056327
20904 Q188040
40201 Q205495
40203 Q216107
40401 Q23413
40405 Q350895
40406 Q193475
40407 Q101659
40408 Q20034791
40409 Q4989906
40410 Q811534
40412 Q16560
40502 Q3914
40505 Q3807410
40508 Q3918
40701 Q44613
40702 Q108325
40703 Q1128397
40704 Q10631691
40705 Q16970
40805 Q483110
40809 Q22746
40826 Q22698
40905 Q27108230
41301 Q17172602
41302 Q10145
41303 Q131862
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
51003 Q93352
51008 Q40080
60104 Q1026259
60105 Q23397
60108 Q3253281
60201 Q6501028
60203 Q131681
60304 Q34038
60305 Q124714"

qcodis <- as.data.frame(matrix(scan(text=dcodis, what="character"), byrow=TRUE, ncol=2), stringsAsFactors=FALSE)
names(qcodis) <- c("CodiGeo", "qtipus")

nomid <- merge(nomid, qcodis, by="CodiGeo", all.x=TRUE)

table(is.na(nomid$qtipus))
table(nomid$CodiGeo[is.na(nomid$qtipus)])

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

# traiem els que siguin duplicats només per tenir dos noms
units <- units[!duplicated(units[, names(units)[names(units)!="itemLabel"]]),]

##################################################################
# Buscar els que no siguin a Wikidata per pujar-los
##############################################################

quedenng <- nomid[!nomid$id %in% units$id,]


#eliminem amb menys coincidència de noms
treupart <- function(nom) {
  nou <- trimws(gsub("(^| )((el|la|els|les|en|na) |[ln]')"," ",nom))
  nou <- trimws(gsub("(^| )((de|del|dels|d'en) |d')"," ",nou))
  return(nou)
}
treuparaula <- function(paraula, nom) {
  expreg <- paste0("(^| )(", paraula,")( |$)")
  nou <- trimws(gsub(expreg," ",nom))
  nou <- gsub("  *"," ", nou)
  return(nou)
}

pelanom <- function(nom){
  nou <- nom
  nou <- gsub("mare de déu|verge|nostra senyora", "santa maria", nou)
  nou <- treupart(nou)
  nom <- gsub("’","'", nou)
  nou <- treuparaula(
    paste("església|ermita|convent|santuari|parr[oò]quial?|capella|basílica|monestir",
          "mas|masia|can|ca|cal|casa|torre|finca|palau|seu", sep="|"), nou)
  nou <- treuparaula(
    paste("església|ermita|convent|santuari|parr[oò]quial?|capella|basílica|monestir",
          "mas|masia|can|ca|cal|palau|seu|antiga|pairal|reial", sep="|"), nou)
  nou <- gsub("v", "b", nou) 
  nou <- gsub("ch", "c", nou) 
  nou <- gsub("as$", "es", nou) #afegit i no provat
  return(nou)
}

#codi municipi
idescat <- getquery("SELECT ?lloc ?llocLabel ?idescat 
WHERE {
  ?lloc wdt:P4335 ?idescat.
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca' .}
}")

#sense tenir en compte municipi seria:
#quedenng <- quedenng[!pelanom(quedenng$nomrel) %in% pelanom(lloctipus$nomrel),]

quedenng <- nomid[!nomid$id %in% units$id,]
pelatng <- quedenng[, c("id", "idescat", "nomrel")]
pelatng$nompelat <- pelanom(pelatng$nomrel)
pelatwd <- lloctipus[, c("item", "idescat", "nomrel"),]
pelatwd <- merge(pelatwd, idescat)
pelatwd$nompelat <- pelanom(pelatwd$nomrel)
pelatwd$nompelat <- 
  sapply(1:nrow(pelatwd), 
         function(i) {
           treuparaula(pelanom(tolower(pelatwd$llocLabel[i])), pelatwd$nompelat[i])})
unitspelats <- merge(pelatng, pelatwd, by=c("nompelat", "idescat"))
quedenng <- quedenng[!quedenng$id %in% unitspelats$id, ]
quedenng <- merge(quedenng, idescat, by="idescat")

# eliminem duplicats (segurament doble municipi)
quedenng <- quedenng[!duplicated(quedenng$id,),]

# eliminem repetits a municipis diferents (mateix nom, mateixa comarca i poca distància)
muncom <- unique(ngcatv10cs0f1r011[ngcatv10cs0f1r011$CodiMun1 != "", c("CodiMun1","CodiCom1")])
muncom <- muncom[!duplicated(muncom$CodiMun1),]
names(muncom) <- c("idescat","comarca")

names(quedenng)
head(quedenng)

pelatcomng <- merge(pelatng[!pelatng$id %in% unitspelats$id, ], muncom, by="idescat")
pelatcomng <- pelatcomng[pelatcomng$nompelat != "",]
pelatcomwd <- merge(pelatwd, muncom, by="idescat")
unitspelatcom <- merge(pelatcomng, pelatcomwd, by=c("comarca", "nompelat"))
unitspelatcom<-unitspelatcom[!duplicated(unitspelatcom),]
unitspelatcom <- merge(unitspelatcom, lloctipus[, c("item","lat","lon")], "item")
unitspelatcom <- merge(unitspelatcom, quedenng[, c("id","lat","lon")], "id")

distancia <- function(lat1,lon1,lat2,lon2) {
  sqrt((lat1-lat2)^2+((lon1-lon2)*cos((lat1+lat2)/2*pi/180))^2)/180*pi*6371
}

unitspelatcom$dist <- with(unitspelatcom, distancia(lat.x,lon.x,lat.y,lon.y))
unitspelatcom <- unitspelatcom[unitspelatcom$dist < .6,]
unitspelatcom <- unitspelatcom[!is.na(unitspelatcom$dist),]
quedenng <- quedenng[! quedenng$id %in% unitspelatcom$id,]

# tipus
tipus <- data.frame(Concepte=c("nucli", "diss.", "barri", "edif.", "edif. hist."),
                    iconc=c("Q486972","Q16557344","Q123705", "Q41176", "Q35112127"),
                    dconc=c("Nucli de població",
                            "Disseminat",
                            "Barri",
                            "Edifici",
                            "Edifici històric"),
                    dconcen=c("Populated place",
                              "Populated place",
                              "Neighborhood",
                              "Building",
                              "Historic building"),
                    stringsAsFactors = FALSE)

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
quick <- function(quadre) {
  fila <- quadre[1,]
  instr <- list(c("CREATE"))
  instr <- afegeix(instr,c("LAST", "Lca", cometes(fila$Toponim)))  
  instr <- afegeix(instr,c("LAST", "Dca", 
                           cometes(paste0(fila$dconc, " ",
                                          de(fila$llocLabel)))))  
  if (!is.na(fila$qtipus)) {
    instr <- afegeix(instr, c("LAST", "P31", fila$qtipus, "S248", "Q98463667"))
  }
  instr <- afegeix(instr, c("LAST", "P31", fila$iconc, "S248", "Q98463667"))  
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
                            paste0('"',fila$dconcen,
                                   " in ", fila$llocLabel,
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

#esglésies
crear <- quedenng[grep("sant",quedenng$nomrel),]
crear <- crear[!grepl("^(casa|ca[ln]|torre|borda|mas|granja|molí|cabana|caseta) ",crear$nomrel),]
leafng <- crear
leafwd <- lloctipus[grep("sant",lloctipus$nomrel),]
leafwd <- leafwd[!grepl("^(casa|ca[ln]|torre|borda|mas|granja|molí|cabana|caseta) ",leafwd$nomrel),]
leafwd <- leafwd[!duplicated(leafwd$item),]


#per comarca
comarca <- "BEB"
leafmun <- unique(ngcatv10cs0f1r011$CodiMun1[
  ngcatv10cs0f1r011$CodiCom1==comarca & 
    ngcatv10cs0f1r011$Concepte %in% c("cap", "mun.")])
crear <- quedenng[quedenng$idescat %in% leafmun,]
leafng <- crear
leafwd <- lloctipus[lloctipus$idescat %in% leafmun,]
leafwd <- leafwd[!duplicated(leafwd$item),]

#per municipi
municipi <- "Mas de Barberans"
#municipi <- idescat$llocLabel[grepl("(Artesa|Puigverd) de Lleida",idescat$llocLabel)]
#municipi <- c("Marçà","els Guiamets","Capçanes")
leafmun <- idescat$idescat[idescat$llocLabel %in% municipi]
leafwd <- lloctipus[lloctipus$idescat %in% leafmun,]
leafwd <- leafwd[!duplicated(leafwd$item),]
leafng <- quedenng[quedenng$idescat %in% leafmun,]
crear <- quedenng[quedenng$llocLabel %in% municipi,]


#mirem

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

leafwd$itemLabel[is.na(leafwd$lat)]
#data.frame(leafng$nomrel, pelanom(leafng$nomrel))
# fi mirem

# creem
crear <- merge(crear, tipus)
crear[crear$qtipus=="Q585956",c("dconc","dconcen")] <- "masia"
if (!exists("recents")) {recents <- c()}
table(crear$id %in% recents)
crear <- crear[!crear$id %in% recents,]
crear<- crear[order(crear$Toponim,crear$lat),]

crear$Toponim
#crear <- crear[-c(14),] #per eliminar els que sobrin

table(is.na(crear$qtipus))
crear[is.na(crear$qtipus),]

# Exclou fora del rectangle i un marge
dins <- (crear$lat<max(leafwd$lat, na.rm=TRUE)+1.5*360/40000 &
           crear$lat>min(leafwd$lat, na.rm=TRUE)-1.5*360/40000 &
           crear$lon<max(leafwd$lon, na.rm=TRUE)+1.5*360/40000 &
           crear$lon>min(leafwd$lon, na.rm=TRUE)-1.5*360/40000)
table(dins)
crear <- crear[dins,]
leafng <- leafng[leafng$id %in% crear$id,]

# preparar instruccions
idmons <- unique(crear$id)
instruccions <- unlist(lapply(idmons, function(i) {quick(crear[crear$id==i,])})) 

# sortides per triar
#cat(paste(instruccions, collapse="\n")) #pantalla
#cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
#cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/varis/instruccions.txt")

if (!exists("recents")) {recents <- c()}
recents <- c(recents, crear$id)

##################################################################
# Buscar coincidents per pujar coordenades als que ja siguin a Wikidata sense coordenades
##############################################################

#duplicats en els dos sentits (COMPTE: ja definit més amunt)
repetits <- function(x) {duplicated(x)|duplicated(x, fromLast = TRUE)}

#tafanejar
with(units, table(nomtipus, Concepte))
units[units$nomtipus=="obra escultòrica",]
units[grep("estació", units$nomtipus),]
#pugem coordenades

#busquem els que són a ng i no a wd

# primer netegem els no segurs per potencials homònims
table(duplicated(units[,c("item","id")]))  
crear <- units[!duplicated(units[,c("item","id")]),]
table(repetits(crear[,c("nomrel","idescat"),]))
head(crear[(repetits(crear[,c("nomrel","idescat"),])),])
crear <- crear[!(repetits(crear[,c("nomrel","idescat"),])),]

# triem sense coordenades
crear <- crear[is.na(crear$lat.x),]
# comprovacions redundants per si de cas:
table(duplicated(crear[,c("id", "item")]))
table(duplicated(crear[,c("UTMX_ETRS89", "UTMY_ETRS89")]))
crear <- crear[!duplicated(crear[,c("id", "item")]),]
table(duplicated(crear$item))
table(repetits(crear$item))
crear[repetits(crear$item),]#revisar a mà
crear <- crear[!repetits(crear$item),]
crear[grepl("escult",crear$tipusLabel),]
crear <- crear[!grepl("escult",crear$tipusLabel),] #evitar escultures mobles

#els pugem
# preparar quickstatemens
quickcoor <- function(fila) {
  instr <- list()
  instr[[1]] <- c(fila$item, "P625", 
                  paste0("@", fila$lat.y,"/", fila$lon.y),
                  "S248", "Q98463667")
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quickcoor(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla

# PER FER: MIRAR ELS QUE NO S'HAN PUJAT PERQUÈ TENEN HOMÒNIMS A WIKIDATA O AL FITXER

######################################################################
# buscar duplicats a Wikidata per eliminar-los
######################################################################
# per això només cal haver carregat Wikidata, no el nomenclàtor

#duplicats en els dos sentits
repetits <- function(x) {duplicated(x)|duplicated(x, fromLast = TRUE)}

repewd <- lloctipus[repetits(lloctipus[c("nomrel", "idescat")]),]
#table(duplicated(repewd$item))
repewd <- repewd[!duplicated(repewd$item),]
repewd <- repewd[repetits(repewd[c("nomrel", "idescat")]),]
repewd <- repewd[order(repewd$nomrel, repewd$item),]

qplant <- function(qurl) {
  q <- gsub("http://www.wikidata.org/entity/Q", "", qurl)
  pl <- paste0("*{{Q|",q,"}}", collapse = "\n")
  return(pl)
}

# sortida incloent duplicats i homònims
# cat(qplant(repewd$item))
if (FALSE) {
  paste("wd", repewd$item, sep=":", collapse=" ")
  cat(paste0("*{{Q|", repewd$item, "}} - ", repewd$nomrel, collapse = "\n"), "\n")
}

# busquem duplicats eliminant els homònims (que tenen la propetat de no confondre)

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
  #print(it1)
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
# sortides per triar
cat(paste0("*{{Q|", repenohom$item, "}} - ", repenohom$nomrel, " ", repenohom$item, collapse = "\n"), "\n")
cat(enc2utf8(paste0("*{{Q|", repenohom$item, "}} - ", repenohom$nomrel, " ", 
                        repenohom$item, collapse = "\n")), 
    file="~/DADES/pere/varis/instruccions.txt")

# preparar sortida amb distància
repenohom$nomimun <- paste(repenohom$nomrel, repenohom$mun)
dist <- sqrt((tapply(repenohom$lat, list(repenohom$nomimun), max) - 
  tapply(repenohom$lat, list(paste(repenohom$nomrel, repenohom$mun)), min))^2 +
  (tapply(repenohom$lon, list(paste(repenohom$nomrel, repenohom$mun)), max) - 
     tapply(repenohom$lon, list(paste(repenohom$nomrel, repenohom$mun)), min))^2)
dist <- dist/180*pi*6371
dist <- round(dist,3)
dist <- as.data.frame(dist)
dist$nomimun <- rownames(dist)
repenohom <- merge(repenohom, dist, by="nomimun")
cat(enc2utf8(paste0("*{{Q|", repenohom$item, "}} - ", repenohom$nomrel, " ", 
                    repenohom$dist, " km", collapse = "\n")), 
    file="~/DADES/pere/varis/instruccions.txt")
