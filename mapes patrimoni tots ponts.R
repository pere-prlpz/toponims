## url ponts

pagmapa <- readLines("~/DADES/pere/varis/mapes culturals ponts.txt",
                     encoding = "UTF-8")
pagponts <- pagmapa[grep("Obra civil \\(", pagmapa):
                      grep("Element urbà \\(", pagmapa)]
pagponts <- pagponts[grepl('<h5><a href="/element/',pagponts)]
urlponts <- gsub('^.*href=\\\"(.*)\\\".*$', "\\1", pagponts)
urlponts <- paste0("https://patrimonicultural.diba.cat", urlponts)

## codis municipis

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

## Per carregar municipis si ja existeixen
if (file.exists("~\\DADES\\pere\\varis/idescat.RData")) {
  load(file="~\\DADES\\pere\\varis/idescat.RData", verbose=TRUE)
} else {
  idescat <- getquery("SELECT ?lloc ?llocLabel ?idescat 
  WHERE {
  ?lloc wdt:P4335 ?idescat.
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca' .}
  }")
  oficials <- c("el Figueró i Montmany"="Figaró-Montmany",
                "Navars"="Navàs",
                "Terrassola i Lavit"="Torrelavit")
  oficials <- data.frame(llocLabel=names(oficials),
                         oficial=oficials)
  idescat <- merge(idescat, oficials, all.x=TRUE)
  idescat$oficial[is.na(idescat$oficial)] <- idescat$llocLabel[is.na(idescat$oficial)]
  save(idescat, file="~\\DADES\\pere\\varis/idescat.RData")
}

## llegir dades mapa patrimoni de cada element


## llegir pàgina mapa parimoni culturlal

library(stringr)

treucarac <- function(carac, pag) {
  i0 <- grep(paste0('<div class="patrimonial-element__',carac,'">'), pag)
  if (length(i0)==0) {
    return(NA)
  }
  i1 <- grep("</div>", pag)
  i1 <- min(i1[i1>i0])
  dada <- pag[i0:i1]
  dada <- dada[grep('<div class="patrimonial-element__item">', dada)]
  dada <- gsub('^.*<div class="patrimonial-element__item">(.*)</div>.*$', "\\1", dada)
  return(dada)  
}

unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# llegeix versió data frame
llegeix <- function(url) {
  pag <- readLines(url, encoding="UTF-8")
  tit <- pag[grepl("<title>.*</title>", pag)]
  nom <- gsub("^.*<title>(.*)\\|.*</title>.*$", "\\1", tit)
  nom <- str_trim(nom)
  nom <- unescape_html(gsub(" $","",nom))
  mun <- pag[grepl('<a href=".*/municipi/.*">.*</a>', pag)]
  mun <- gsub('<a href=".*/municipi/.*">(.*)</a>', "\\1", mun)
  mun <- unescape_html((str_trim(mun)))
  qmun <- idescat$lloc[tolower(idescat$llocLabel)==tolower(mun)]
  if (length(qmun)==0) {
    qmun <- idescat$lloc[tolower(idescat$oficial)==tolower(mun)]
  }
  caracs <- c("height", "latitude", "longitude")
  lcaracs <- lapply(caracs, treucarac, pag)
  names(lcaracs) <- caracs
  iest <- grep('<div class="field__label">Estat de conservació</div>', pag)
  cons <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[iest+1])
  estcons <- c("Bo"="Q56557591", "Regular"="Q106379705")
  qcons <- estcons[cons]
  if (is.na(qcons)) {
    print(paste("Estat de conservació desconegut:", cons))
  }
  iest <- grep('<div class="field__label">Any</div>', pag)
  if (length(iest)==1) {
    any <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[iest+1])
    segle <- NA
  } else {
    any <- NA
    iest <- grep('<div class="field__label">Segle</div>', pag)
    if (length(iest)==1) {
      segle <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[iest+1])
    } else {
      segle <- NA
    }
  }
  resultat <- c(list(nom=nom, municipi=mun, qmun=qmun, qcons=qcons),
                lcaracs,
                list(any=any, segle=segle,
                url=url))
  return(as.data.frame(resultat, row.names = NULL))
}

# llegir
if (file.exists("~/DADES/pere/varis/mapes culturals ponts.RData")) {
  load(file="~/DADES/pere/varis/mapes culturals ponts.RData", verbose=TRUE)
}

if(!exists("dadesponts")) {
  dadesponts <- (llegeix(urlponts[1]))
}

urlllegir <- urlponts[! urlponts %in% dadesponts$url]

for (url in urlllegir) {
  print(url)
  dadesponts <- rbind(dadesponts, llegeix(url))
  save(dadesponts,
       file="~/DADES/pere/varis/mapes culturals ponts.RData")
}


## PER FER: INCLOURE AQÜEDUCTES



## carregar de Wikidata tots els ponts de Catalunya amb P131 

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

lloctipus$nomnet <- sapply(lloctipus$itemLabel, treupar)
lloctipus$nomrel <- treuart(tolower(lloctipus$nomnet))

# referències amb url
refwd <- getquery("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?mun ?munLabel ?idescat ?url
WHERE {
  VALUES ?tipuspont {wd:Q12280 wd:Q474}
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31 ?tipus.
  ?tipus wdt:P279* ?tipuspont.
  ?item wdt:P131* ?mun.
  ?mun wdt:P4335 ?idescat.
  ?item p:P131 ?stlloc.
  ?stlloc prov:wasDerivedFrom ?ref.
  ?ref pr:P854 ?url.
SERVICE wikibase:label {
bd:serviceParam wikibase:language 'ca,oc,en,es,pl,sv,ceb'.
}
}")


## preparem dadesponts per unir
load("~/DADES/pere/varis/mapes culturals ponts.RData", verbose=TRUE)
#dadesponts<- dadesponts[!is.na(dadesponts$qmun),]
table(is.na(dadesponts$qmun))

## excloure coses que no són ponts
table(substr(dadesponts$nom, 1, 6))
dadesponts[grepl("^(embassament|resclosa)", tolower(dadesponts$nom)),]
dadesponts[grepl("(de|a)l pont", tolower(dadesponts$nom)) & !grepl("^(base del )?pont", tolower(dadesponts$nom)),]
dadesponts <- dadesponts[!(grepl("(de|a)l pont", tolower(dadesponts$nom)) & !grepl("^(base del )?pont", tolower(dadesponts$nom))),]



dadesponts$nomnet <- sapply(dadesponts$nom, treupar)
dadesponts$nomrel <- treuart(tolower(dadesponts$nomnet))
dadesponts <- merge(dadesponts, idescat, by.x="qmun", by.y="lloc", all.x=TRUE)
table(is.na(dadesponts$idescat))

#1- unim per nom i municipi
units <- merge(lloctipus, dadesponts, by=c("nomrel","idescat"))

#veure
table(is.na(units$lat))
units[is.na(units$lat),]
table(duplicated(units[, names(units)[names(units)!="itemLabel"]]))

# traiem els que siguin duplicats només per tenir dos noms
units <- units[!duplicated(units[, names(units)[names(units)!="itemLabel"]]),]

# units per les referències 
unitsref <- merge(refwd, dadesponts, by=c("url"))


##################################################################
# Buscar els que no siguin a Wikidata per pujar-los
##############################################################

crear <- dadesponts[!dadesponts$url %in% units$url,]
crear  <- crear[!crear$url %in% unitsref$url,]
quedenwd <- lloctipus

leafng <- crear
leafng$latitude <- as.numeric(leafng$latitude)
leafng$longitude <- as.numeric(leafng$longitude)
leafwd <- quedenwd
leafwd <- leafwd[!duplicated(leafwd$item),]

# desaparellats de wikidata en vermell, nomenclàtor en blau
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addCircleMarkers(m, lng=leafng$longitude, lat=leafng$latitude, popup=leafng$nom)
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
  if (grepl("^[eE]ls? ", nom)) {
    denom <- paste0("d",nom)
    denom <- gsub("^dEl", "del", denom)
  } else {
    if (grepl("^[AEIOUÀÈÉÍÒÓÚ]", nom)) {
      denom <- paste0("d'",nom)
    } else {
      denom <- paste0("de ",nom)
    }
  }
  denom <- gsub("^de La ", "de la ", denom)
  denom <- gsub("^de Les ", "de les ", denom)
  denom <- gsub("^de L'", "de l'", denom)
  return(denom)
}

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

netnum <-  function(txt , num=TRUE) {
  txt <- gsub("[a-z]\\.","",txt)
  txt <- gsub("[a-z]","",txt)
  txt <- gsub(",",".",txt)
  if (num) {
    return(as.numeric(txt))
  } else {
    return(txt)
  }
}

datas <- function(any=NA, segle=NA) {
  if (!is.na(any)) {
    any <- strsplit(any,"-")[[1]][1]
    any <- netnum(any)
    if (!is.na(any)) {
      return(paste0("+",any,"-00-00T00:00:00Z/9"))
    } else {
      return(NA)
    }
  } else if (!is.na(segle)) {
    inicial <- (0:20)*100+1
    names(inicial) <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", 
                        "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", 
                        "XX", "XXI")
    inici <- inicial[segle]
    if (!is.na(inici)) {
      inici <- netnum(inici, num=FALSE)
      return(paste0("+",inici,"-00-00T00:00:00Z/7"))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

quick <- function(dades, qid="LAST", altres=c(""), descr=TRUE) {
  curl <- cometes(dades$url)
  if (qid=="LAST") {
    instr <- list(c("CREATE"))
    Lca <- "Lca"
    Len <- "Len"
  } else {
    instr <- list()
    Lca <- "Aca"
    Len <- "Aen"
  }
  if (grepl("aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q18870689", 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (grepl("pont|viaducte|passera|aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q12280", 
                              "S248", "Q9028374", "S854", curl))
    terme <-  c("ca"="pont", "en"="bridge")
  }
  if (grepl("font", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q483453", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="font", "en"="fountain")
  }
  instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
  instr <- afegeix(instr, c(qid, Len, cometes(dades$nom)))  
  if (descr) {
    instr <- afegeix(instr,c(qid, "Dca", 
                             cometes(paste(terme["ca"],
                                           de(dades$municipi)))))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes(paste(terme["en"], 'in', 
                                            dades$municipi,
                                            '(Catalonia)'))))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$qmun, 
                            "S248", "Q9028374", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P625", 
                            paste0("@", dades$latitude,"/", dades$longitude),
                            "S248", "Q9028374", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P17", "Q29")) 
  if (!is.na(dades$height)) {
    instr <- afegeix(instr, c(qid, "P2044", 
                              paste0(netnum(dades$height), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(dades$qcons)) {
    instr <- afegeix(instr, c(qid, "P5816", dades$qcons, 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["llum"])) {
    instr <- afegeix(instr, c(qid, "P2787", 
                              paste0(gsub(" ?m$","",altres["llum"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["llarg"])) {
    instr <- afegeix(instr, c(qid, "P2043", 
                              paste0(gsub(" ?m$","",altres["llarg"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["ample"])) {
    instr <- afegeix(instr, c(qid, "P2049", 
                              paste0(gsub(" ?m$","",altres["ample"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["ulls"])) {
    instr <- afegeix(instr, c(qid, "P1314", 
                              altres["ulls"], 
                              "S248", "Q9028374", "S854", curl))  
  }
  dcrea <- datas(dades$any, dades$segle)
  if (!is.na(dcrea)) {
    instr <- afegeix(instr, c(qid, "P571", 
                              dcrea, 
                              "S248", "Q9028374", "S854", curl))  
  }
  return(instr)
}
#PER FER: FER SERVIR LLOCLABEL EN COMPTES DE MUNICIPI PER EVITAR PROBLEMES AMB MAJÚSCULES

#quick(crear[1,])
#lapply(1:5, function(i) quick(crear[i,]))

instruccions <- paste(unlist(lapply(1:nrow(crear), function(i) quick(crear[i,]))), sep="\t", collapse="\n")
#cat(instruccions)
cat(enc2utf8(instruccions), file="~/DADES/pere/varis/instruccions.txt")

##################################################################
# Buscar els que ja són a Wikidata per completar dades
##############################################################

#units
afegir <- units[units$item != "Q81756144",]
table(duplicated(units$item))
afegir[is.na(afegir$lat),]

quick <- function(dades, altres=c("")) {
  curl <- cometes(dades$url)
  if ("item" %in% names(dades)) {
    qid <- dades$item
    descr <- FALSE
  } else  {
    quid <- "LAST"
    descr <- TRUE
  }
  if (qid=="LAST") {
    instr <- list(c("CREATE"))
    Lca <- "Lca"
    Len <- "Len"
  } else {
    instr <- list()
    Lca <- "Aca"
    Len <- "Aen"
  }
  if (grepl("aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q18870689", 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (grepl("pont|viaducte|passera|aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q12280", 
                              "S248", "Q9028374", "S854", curl))
    terme <-  c("ca"="pont", "en"="bridge")
  }
  if (grepl("font", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q483453", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="font", "en"="fountain")
  }
  if (is.na(dades$itemLabel) | 
      tolower(dades$itemLabel) != tolower(dades$nom)) {
    instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
    instr <- afegeix(instr, c(qid, Len, cometes(dades$nom)))  
  }
  if (descr) {
    instr <- afegeix(instr,c(qid, "Dca", 
                             cometes(paste(terme["ca"],
                                           de(dades$municipi)))))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes(paste(terme["en"], 'in', 
                                            dades$municipi,
                                            '(Catalonia)'))))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$qmun, 
                            "S248", "Q9028374", "S854", curl))
  if (is.na(dades$lat)) {
    instr <- afegeix(instr, c(qid, "P625", 
                              paste0("@", dades$latitude,"/", dades$longitude),
                              "S248", "Q9028374", "S854", curl))  
  }
  instr <- afegeix(instr, c(qid, "P17", "Q29")) 
  if (!is.na(dades$height)) {
    instr <- afegeix(instr, c(qid, "P2044", 
                              paste0(netnum(dades$height), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(dades$qcons)) {
    instr <- afegeix(instr, c(qid, "P5816", dades$qcons, 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["llum"])) {
    instr <- afegeix(instr, c(qid, "P2787", 
                              paste0(gsub(" ?m$","",altres["llum"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["llarg"])) {
    instr <- afegeix(instr, c(qid, "P2043", 
                              paste0(gsub(" ?m$","",altres["llarg"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["ample"])) {
    instr <- afegeix(instr, c(qid, "P2049", 
                              paste0(gsub(" ?m$","",altres["ample"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["ulls"])) {
    instr <- afegeix(instr, c(qid, "P1314", 
                              altres["ulls"], 
                              "S248", "Q9028374", "S854", curl))  
  }
  dcrea <- datas(dades$any, dades$segle)
  if (!is.na(dcrea)) {
    instr <- afegeix(instr, c(qid, "P571", 
                              dcrea, 
                              "S248", "Q9028374", "S854", curl))  
  }
  return(instr)
}

instruccions <- paste(unlist(lapply(31:nrow(afegir), function(i) quick(afegir[i,]))), sep="\t", collapse="\n")
#cat(instruccions)
cat(enc2utf8(instruccions), file="~/DADES/pere/varis/instruccions.txt")

