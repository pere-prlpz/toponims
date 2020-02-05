# afegir i pujar edificis

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

classeswd <- getsparql("https://query.wikidata.org/sparql?query=SELECT%20DISTINCT%20%3Ftipus%20%3Fnomtipus%0AWHERE%20%7B%0A%3Ftipus%20wdt%3AP279*%20wd%3AQ811979.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Cfr%2Ceu%2Cde%2Csv%2Cceb%22%20.%0A%3Ftipus%20rdfs%3Alabel%20%3Fnomtipus%0A%7D%0A%7D%0A")
municipiswd <- getsparql("https://query.wikidata.org/sparql?query=%23Municipis%20de%20Catalunya%0ASELECT%20%3Fmun%20%3Fmuname%20%3Fcom%20%3Fcomname%0AWHERE%20%7B%0A%20%20VALUES%20%3Fdivisio%20%7Bwd%3AQ19920968%20wd%3AQ937876%7D%0A%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20%3Fmun%20wdt%3AP131%20%3Fcom.%0A%20%20%3Fcom%20wdt%3AP31%20%3Fdivisio.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fmuname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fcom%20rdfs%3Alabel%20%3Fcomname%0A%7D%0A%7D%0A%0A")
llocswd <- getsparql("https://query.wikidata.org/sparql?query=%23Tot%20de%20Catalunya%0ASELECT%20%3Fitem%20%3Fname%20%3Ftipus%20%3Fcoord%20%3Flat%20%3Flon%20%3Fmun%20%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP131*%20wd%3AQ5705.%0A%20%20%3Fitem%20wdt%3AP31%20%3Ftipus.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0A%20%20OPTIONAL%20%7B%0A%20%20%20%20%3Fitem%20wdt%3AP131*%20%3Fmun.%0A%20%20%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%7D%0A%0A")
properswd <- getsparql("https://query.wikidata.org/sparql?query=%23Items%20propers%0ASELECT%20%3Fitem%20%3Fname%20%3Ftipus%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP31%20%3Ftipus.%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon.%0A%20%20FILTER%20(%3Flat%20%3E%2040.3)%0A%20%20FILTER%20(%3Flon%20%3E%200.1)%20%20%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%7D%0A%0A")

#save(classeswd, municipiswd, llocswd, properswd, file="~/DADES/Pere/varis/llocswd.RData")

properswd <- properswd[!properswd$item %in% llocswd$item,]
properswd$mun <- NA
llocswd <- rbind(llocswd, properswd)

edificiswd <- llocswd[llocswd$tipus %in% classeswd$tipus,]

#Conversió municipis WD (noms IEC actuals) a nomenclàtor (noms oficials antics) 
#PER FER: Canviar a l'inrevés per treballar amb els correctes
actnom <- function(name) {
  name[name=="La Granada"] <- "la Granada"
  name[name=="La Sénia"] <- "la Sénia"
  name[name=="Calonge i Sant Antoni"] <- "Calonge"
  name[name=="el Figueró i Montmany"] <- "Figaró-Montmany"
  name[name=="Roda de Berà"] <- "Roda de Barà"
  name[name=="Navars"] <- "Navàs"
  name[name=="l'Esquirol"] <- "Santa Maria de Corcó"
  name[name=="Brunyola i Sant Martí Sapresa"] <- "Brunyola"
  return(name)  
}

municipiswd$muname <- actnom(municipiswd$muname)

#arreglo errors
municipiswd <- municipiswd[!(municipiswd$muname=="Vilademuls"& municipiswd$comname=="Alt Empordà"),]


edificiswd <- merge(edificiswd, classeswd, by="tipus")
edificiswd <- merge(edificiswd, municipiswd, by="mun", 
                    all.x = TRUE, all.y=FALSE)

ttipus <- table(edificiswd$nomtipus)
head(sort(ttipus, decreasing = TRUE), 20)
table(duplicated(edificiswd$item))

# nomenclàtor
library(readxl)
index_nomenclator_2009 <- read_excel("~/DADES/pere/varis/index_nomenclator_2009.xls",
                                     col_types = c("text", "text", "text",
                                                   "text", "text", "text", "text", "text",
                                                   "text", "text", "text", "numeric",
                                                   "numeric", "numeric", "text"))
conceptes_pob <- c("edif.", "edif. hist.")
nomred <- 
  index_nomenclator_2009[index_nomenclator_2009$Concepte %in% conceptes_pob, 1:13]

# canvi coordenades nomenclàtor
library(rgdal)
coords <- as.matrix(nomred[, c("UTM X","UTM Y")])*100
#sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=31N +datum=WGS84"))
sputm <- 
  SpatialPoints(
    coords, 
    proj4string=
      CRS("+init=epsg:23031 +proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo.df <- as.data.frame(spgeo)
names(spgeo.df) <- c("lon", "lat")
nomred <- cbind(nomred, spgeo.df)

# regularització del nom
arreglanom <- function(nom) {
  nom <- gsub(" *", "", nom, fixed = TRUE)
  trossos <- unlist(strsplit(nom, ", "))
  n <- length(trossos)
  nom <- ifelse(n==1,
                nom,
                paste(c(trossos[n], trossos[1:(n-1)]), collapse = " ")
  )
  nom <- gsub("' ","'", nom)
  return (nom)
}

nomred$nom <- sapply(nomred$Topònim, arreglanom)


# nom dels nuclis existents
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les) ","",nom)
  gsub("^l'","",nom)
}

edificiswd$nomnet <- sapply(edificiswd$name, treupar)

# noms per enganxar
edificiswd$nomrel <- treuart(tolower(edificiswd$nomnet))
nomred$nomrel <- treuart(tolower(nomred$nom))
edificiswd$munrel <- tolower(actnom(edificiswd$muname))
nomred$munrel <- tolower(nomred$`Municipi 1`)
nomred$munrel <- gsub(" *","",nomred$munrel, fixed = TRUE)
nomred$ref <- 1:nrow(nomred)

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}
edificiswd$qitem <- itemq(edificiswd$item)

#1- unim per nom i municipi i pugem coordenades
# PER FER: treballar a banda amb els que n'hi ha dos amb el mateix nom
# al mateix municipi, per evitar errors.
units <- merge(edificiswd, nomred, by=c("nomrel", "munrel"), all=FALSE)
#table(units$nomtipus, units$Concepte)
afegircoor <- units[(!(is.na(units$lat.y)) & is.na(units$lat.x)),]
table(is.na(units$lat.y), is.na(units$lat.x))

# preparar quickstatemens
quickcoor <- function(fila) {
  instr <- list()
  instr[[1]] <- c(fila$qitem, "P625", 
                  paste0("@", fila$lat.y,"/", fila$lon.y),
                  "S248", "Q11938912")
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(afegircoor), function(i) {quickcoor(afegircoor[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla

# eliminem
quedenwd <- edificiswd[! edificiswd$item %in% units$item,]
quedennom <- nomred[! nomred$ref %in% units$ref,]

#2- unim per nom i coord i pugem municipis

#comprovo els dels noms repetits (sense que consti municipi igual)
unitsnom <- merge(quedenwd, quedennom, by=c("nomrel"), all=FALSE)
unitsnom <- unitsnom[!duplicated(unitsnom[, -grep("tipus", colnames(unitsnom))]),]

# distància segons coordenades
distgeo <- function(lat.x, lon.x, lat.y, lon.y) {
  sqrt(((6371*(lat.x-lat.y)*pi/180)^2+(6371*cos(lat.x/180*pi)*(lon.x-lon.y)*pi/180)^2))
}

unitsnom$dist <- with(unitsnom, distgeo(lat.x, lon.x, lat.y, lon.y))
unitsnom <- unitsnom[unitsnom$dist<1.2 & !is.na(unitsnom$dist),] #força segurs

# afegir municipis per pujar-los
municipiswd$munrel <- tolower(actnom(municipiswd$muname))

posamunicipis <- function(crear, municipis=municipiswd) {
  names(crear)[names(crear) == "munrel.y"] <- "munrel1"
  names(crear)[names(crear) == "munrel"] <- "munrel1"
  crear$munrel2 <- tolower(crear$`Municipi 2`)
  crear$munrel2 <- gsub(" *","",crear$munrel2, fixed = TRUE)
  crear <- merge(crear, municipis, by.x="munrel1", by.y="munrel", all.x=TRUE)
  crear <- merge(crear, municipis, by.x="munrel2", by.y="munrel", all.x=TRUE)
  names(crear)[names(crear) == "mun.y"] <- "mun1"
  names(crear)[names(crear) == "mun"] <- "mun2"
  crear$mun1 <- itemq(crear$mun1)
  crear$mun2 <- itemq(crear$mun2)
  return(crear)
}

unitsnom <- posamunicipis(unitsnom)

head(unitsnom[ , c("nomrel", "munrel1", "munrel2", colnames(unitsnom)[grep("muname*", colnames(unitsnom))])])

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- vector
  return(llista)
}

# preparar quickstatemens
quickmunicipis <- function(fila) {
  instr <- list()
  if (!is.na(fila$mun1)) {
    instr <- afegeix(instr, c(fila$qitem, "P131", fila$mun1, 
                              "S248", "Q11938912"))
  }
  if (!is.na(fila$mun2)) {
    instr <- afegeix(instr, c(fila$qitem, "P131", fila$mun2, 
                              "S248", "Q11938912"))
  }
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(unitsnom), 
                              function(i) {quickmunicipis(unitsnom[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
#cat(enc2utf8(paste(instruccions, collapse="\n")),
#    file="~/DADES/pere/varis/instruccions.txt")

quedenwd <- quedenwd[! quedenwd$item %in% unitsnom$item,]
quedennom <- quedennom[! quedennom$ref %in% unitsnom$ref,]

#Passem a nom simplificat
simplificanom <- function(nom) {
  nom <- treuart(nom)
  nom <- gsub(" (de|dels) ", " ", nom)
  nom <- gsub(" (el|els|la|les) ", " ", nom)
  nom <- gsub(" l'", " ", nom)
  nom <- gsub("^(església|ermita|santuari|mas|masia|el|la) ", "", nom)
  return(nom)
}

quedenwd$nomrel <- simplificanom(quedenwd$nomrel)
quedennom$nomrel <- simplificanom(quedennom$nomrel)
# llengua
quedennom$llengua <- ifelse(quedennom$`Comarca 1`=="VAR", "oc", "ca") #això hauria de ser abans

#1B- unim per nom i municipi i pugem coordenades
units <- merge(quedenwd, quedennom, by=c("nomrel", "munrel"), all=FALSE)
#table(units$nomtipus, units$Concepte)
afegircoor <- units[(!(is.na(units$lat.y)) & is.na(units$lat.x)),]
table(is.na(units$lat.y), is.na(units$lat.x))

instruccions <- unlist(lapply(1:nrow(afegircoor), function(i) {quickcoor(afegircoor[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla

quedenwd <- quedenwd[! quedenwd$item %in% units$item,]
quedennom <- quedennom[! quedennom$ref %in% units$ref,]

#2B- unim per nom i distància i pugem municipi

#comprovo els dels noms repetits (sense que consti municipi igual)
unitsnom <- merge(quedenwd, quedennom, by=c("nomrel"), all=FALSE)
unitsnom <- unitsnom[!duplicated(unitsnom[, -grep("tipus", colnames(unitsnom))]),]
unitsnom$dist <- with(unitsnom, distgeo(lat.x, lon.x, lat.y, lon.y))
unitsnom <- unitsnom[unitsnom$dist<1.2 & !is.na(unitsnom$dist),] #força segurs
unitsnom <- posamunicipis(unitsnom)

instruccions <- unlist(lapply(1:nrow(unitsnom), 
                              function(i) {quickmunicipis(unitsnom[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla

quedenwd <- quedenwd[! quedenwd$item %in% unitsnom$item,]
quedennom <- quedennom[! quedennom$ref %in% unitsnom$ref,]

#3- eliminem parells nom simplificat sense coordenades
unitsnom <- merge(quedenwd, quedennom, by=c("nomrel"), all=FALSE)
unitsnom <- unitsnom[!duplicated(unitsnom[, -grep("tipus", colnames(unitsnom))]),]
unitsnom$dist <- with(unitsnom, distgeo(lat.x, lon.x, lat.y, lon.y))
unitsnom <- unitsnom[is.na(unitsnom$dist),] #algun pot ser

quedenwd <- quedenwd[! quedenwd$item %in% unitsnom$item,]
quedennom <- quedennom[! quedennom$ref %in% unitsnom$ref,]

head(sort(table(unitsnom$item), decreasing = TRUE), 20)

#PER FER: llistar els que en bloquegen molts per falta de coordenades
#PER FER: no eliminar els que no tenen coordenades però són en municipis distants

#per si calen
quedenwd0 <- quedenwd
quedennom0 <- quedennom

#quedenwd <- quedenwd0
#quedennom <- quedennom0

#4- eliminem els possibles parells per coordenades i creem els que quedin
# deixem estar els que no tenen coordenades
quedenwd <- quedenwd[!is.na(quedenwd$lat),]
quedennom <- quedennom[!is.na(quedennom$lat),]


# elimino només per distància (800 m)
dmin = .6

eliminadist <- function(quedennom, quedenwd, dmin=.8) {
  if (nrow(quedennom)==0 | nrow(quedenwd)==0) {
    return(numeric(0))
  }
  mdist <- outer(1:nrow(quedennom), 1:nrow(quedenwd),
                 function(i,j) {distgeo(quedenwd$lat[j], quedenwd$lon[j],
                                     quedennom$lat[i],quedennom$lon[i])})
  minnom <- apply(mdist, 1, min, na.rm=TRUE)
  return(quedennom$ref[minnom<dmin])
}

trossos <- 10
latb <- quantile(quedennom$lat, probs = seq(0, 1, 1/trossos))
lonb <- quantile(quedennom$lon, probs = seq(0, 1, 1/trossos))
deltalat <- asin(dmin/6371)/pi*180
deltalon <- asin(dmin/6371)/pi*180/cos(41/180*pi)



programa <- data.frame(latmin=rep(head(latb,-1)-deltalat, trossos), 
                       latmax=rep(tail(latb,-1)+deltalat, trossos),
                       lonmin=rep(head(lonb,-1)-deltalon, each=trossos),
                       lonmax=rep(tail(lonb,-1)+deltalon, each=trossos)
                       )

massaprop <- unlist(sapply(1:nrow(programa),
                         FUN = function(i) {
                           #print(i)
                           with(programa[i,], {
                             quadnom <- quedennom[quedennom$lat>latmin &
                                                    quedennom$lat<latmax &
                                                    quedennom$lon>lonmin &
                                                    quedennom$lon<lonmax,]
                             quadwd <- quedenwd[quedenwd$lat>latmin &
                                                  quedenwd$lat<latmax &
                                                  quedenwd$lon>lonmin &
                                                  quedenwd$lon<lonmax,]
                             #print(nrow(quadnom))
                             #print(nrow(quadwd))
                             return(eliminadist(quadnom, quadwd, dmin))
                           }
                           )
                         }
)
)

table(duplicated(massaprop))

crear <- quedennom[!quedennom$ref %in% massaprop, ]

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
crear <- merge(crear, tipus)

# comarca
# s'hauria d'unificar funció amb posamunicipis
posamunicipis2 <- function(crear, municipis=municipiswd) {
  crear$munrel1 <- tolower(crear$`Municipi 1`)
  crear$munrel1 <- gsub(" *","",crear$munrel1, fixed = TRUE)
  crear$munrel2 <- tolower(crear$`Municipi 2`)
  crear$munrel2 <- gsub(" *","",crear$munrel2, fixed = TRUE)
  crear <- merge(crear, municipis, by.x="munrel1", by.y="munrel", all.x=TRUE)
  crear <- merge(crear, municipis, by.x="munrel2", by.y="munrel", all.x=TRUE)
  names(crear)[names(crear) == "mun.x"] <- "mun1"
  names(crear)[names(crear) == "mun.y"] <- "mun2"
  crear$mun1 <- itemq(crear$mun1)
  crear$mun2 <- itemq(crear$mun2)
  crear$nommun1 <- gsub(" *", "", crear$`Municipi 1`, fixed = TRUE)
  crear$nommun2 <- gsub(" *", "", crear$`Municipi 2`, fixed = TRUE) #nommun2 no utilitzat
  return(crear)
}

crear <- posamunicipis2(crear, municipiswd)


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

cometes <- function(text) {
  paste0('"',text,'"')
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list(c("CREATE"))
  instr[[2]] <- c("LAST", "Lca", cometes(fila$nom)) 
  instr[[3]] <- c("LAST", "Dca", 
                  paste0('"',fila$dconc, " ",
                         de(fila$nommun1),
                         " (",fila$comname.x,")",'"'))
  instr[[4]] <- c("LAST", "P31", fila$iconc, "S248", "Q11938912")
  instr[[5]] <- c("LAST", "P131", fila$mun1, "S248", "Q11938912")
  instr[[6]] <- c("LAST", "P625", 
                  paste0("@", fila$lat,"/", fila$lon),
                  "S248", "Q11938912")
  instr[[7]] <- c("LAST", "P17", "Q29")
  instr[[8]] <- c("LAST", "Len", cometes(fila$nom)) 
  instr[[9]] <- c("LAST", "Den", 
                  paste0('"',fila$dconcen,
                         " in ", fila$nommun1,
                         " (",fila$comname.x,", Catalonia)",'"'))
  instr[[10]] <- c("LAST", "Leu", cometes(fila$nom)) 
  instr[[11]] <- c("LAST", "Loc", cometes(fila$nom)) 
  instr[[12]] <- c("LAST", "Lfr", cometes(fila$nom)) 
  instr[[13]] <- c("LAST", "Lpt", cometes(fila$nom)) 
  instr[[14]] <- c("LAST", "P1448", 
                   paste0(fila$llengua,":",
                          cometes(fila$nom)), 
                   "S248", "Q11938912")
  if (!is.na(fila$mun2)) {
    instr[[15]] <- c("LAST", "P131", fila$mun2, "S248", "Q11938912")
  }
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)
#cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")

#PER FER: crear els duplicats (dos llocs amb el mateix nom dins del mateix municipi)
#PER FER: comprovar duplicats amb nom oficial, per evitar tornar a crear els ja fusionats (ex Sant Antoni a Alt Àneu)