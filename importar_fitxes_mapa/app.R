#
# Aplicació gràfica per importar fitxes de diferents fonts
# fent servir un mapa
# Versió per desenvolupar botó d'actualitzar
# Situació actual: actualitza però només fa servir les dades actualitzades
# per dibuixar el mapa i no les guarda

library(shiny)
library(leaflet)
library(rclipboard)
library(httr)
library(rjson)

## Preparació

# Dades vàries
load("idescat.RData")

load("barris.RData")
vbarris <- barris$barri
names(vbarris) <- barris$barriLabel
vbarris <- c("Tria un barri"="", vbarris)


# Dades wd
load("dadeswdcat.RData")
## A REACTIVE 2 LÏNIES
# dadeswdinst <- dadeswd[,c("item", "itemLabel", "inst","instLabel")] 
# dadeswd <- dadeswd[!duplicated(dadeswd$item),]
## REACTIVE FINS AQUÏ

## dades Ajuntament
load("dades_cataleg_bcn.RData")

dfbcn <- data.frame(nom=sapply(dadesbcn, function(x) x$nom),
                    idbcn=sapply(dadesbcn, function(x) x$id),
                    lon=sapply(dadesbcn, function(x) x$coord["longitude"]),
                    lat=sapply(dadesbcn, function(x) x$coord["latitude"]),
                    url=sapply(dadesbcn, function(x) x$url))

# dades pcat
load("pcat.RData")

va <- sapply(bdpcat, function(x) x$va)
bdpcatnet <- bdpcat[va]
filaadf <- function(fila) {data.frame(nom=fila$nom[1],
                                      municipi=fila$municipi[1],
                                      coords=fila$coords[1],
                                      url=fila$url[1])}
dfpcat <- lapply(bdpcatnet, filaadf)
dfpcat <- do.call(rbind, dfpcat)

coords <- strsplit(dfpcat$coords, "; ")
coords <- as.data.frame(do.call(rbind, coords))
names(coords) <- c("lat", "lon")
dfpcat <- cbind(dfpcat, coords)
dfpcat$lat <- as.numeric(dfpcat$lat)
dfpcat$lon <- as.numeric(dfpcat$lon)
dfpcat$idpcat <- gsub("https://www.poblesdecatalunya.cat/element.php?e=","", 
                      dfpcat$url, fixed=TRUE)

### PASSAR A REACTIU DES D'AQUÍ
# # Anem separant: elements de Wikidata
# dadeswdverm <- dadeswd[is.na(dadeswd$idbcn) & is.na(dadeswd$idpcat),]
# dadeswdblau <- dadeswd[!is.na(dadeswd$idbcn) & is.na(dadeswd$idpcat),]
# dadeswdverd <- dadeswd[is.na(dadeswd$idbcn) & !is.na(dadeswd$idpcat),]
# dadeswdgris <- dadeswd[!is.na(dadeswd$idbcn) & !is.na(dadeswd$idpcat),]
# 
# # Elements de bd
# bcnblau <- dfbcn[!dfbcn$idbcn %in% dadeswd$idbcn,]
# bcnblau <- bcnblau[!grepl("^(Entorn|Conjunt)",bcnblau$nom),]
# pcatverd <- dfpcat[!dfpcat$idpcat %in% dadeswd$idpcat,] # & dfpcat$municipi=="Barcelona"
### PASSAR A REACTIU FINS AQUÍ

limita <- function(df, limlat, limlon) {
  df[df$lat>limlat[1] & df$lat<limlat[2] & df$lon>limlon[1] & df$lon<limlon[2],]
}

# Representar mapa
fesmapa <- function(dadeswdverm, dadeswdverd, dadeswdblau, dadeswdgris, 
                    bcnblau, pcatverd, 
                    limlat=c(-90,90), limlon=c(-180,180)) {
  dadeswdverm <- limita(dadeswdverm, limlat, limlon)
  dadeswdverd <- limita(dadeswdverd, limlat, limlon)
  dadeswdblau <- limita(dadeswdblau, limlat, limlon)
  dadeswdgris <- limita(dadeswdgris, limlat, limlon)
  bcnblau <- limita(bcnblau, limlat, limlon)
  pcatverd <- limita(pcatverd, limlat, limlon)
  m <- leaflet()
  m <- addTiles(m, options=tileOptions(maxZoom = 24))
  m <- addCircleMarkers(m, lng=dadeswdverm$lon, lat=dadeswdverm$lat, layerId=dadeswdverm$item,
                        popup=paste(dadeswdverm$item,dadeswdverm$itemLabel), color = "red", radius=5)
  m <- addCircleMarkers(m, lng=dadeswdverd$lon, lat=dadeswdverd$lat, layerId=dadeswdverd$item,
                        popup=paste(dadeswdverd$item,dadeswdverd$itemLabel), color = "green", radius=5)
  m <- addCircleMarkers(m, lng=dadeswdblau$lon, lat=dadeswdblau$lat, layerId=dadeswdblau$item,
                        popup=paste(dadeswdblau$item,dadeswdblau$itemLabel), color = "blue", radius=5)
  m <- addCircleMarkers(m, lng=dadeswdgris$lon, lat=dadeswdgris$lat, layerId=dadeswdgris$item,
                        popup=paste(dadeswdgris$item,dadeswdgris$itemLabel), color = "brown", radius=5)
  m <- addCircleMarkers(m, lng=bcnblau$lon, lat=bcnblau$lat, layerId=paste0("bcn",bcnblau$idbcn),
                        popup=(paste(bcnblau$idbcn, bcnblau$nom)), color="blue")
  m <- addCircleMarkers(m, lng=pcatverd$lon, lat=pcatverd$lat, layerId=paste0("pcat", pcatverd$idpcat),
                        popup=(paste(pcatverd$idpcat, pcatverd$nom)), color="green")
  return(m)
}

## funcions explorar/preparar

dadesid <- function(id, dadeswd, dfbcn, dfpcat) {
  if(length(id)==0) {return(list())}
  if(grepl("^Q[0-9]*$", id)) {
    return(dadeswd[dadeswd$item==id,])
  } else if(grepl("^pcat[0-9]*$", id)) {
    idpcat <- gsub("pcat", "", id)
      return(dfpcat[dfpcat$idpcat==idpcat,])
  } else if(grepl("^bcn[0-9]*$", id)) {
    idbcn <- gsub("bcn", "", id)
    return(dfbcn[dfbcn$idbcn==idbcn,])
  }
}

informeid <- function(id, dadeswd, dfbcn, dfpcat) {
  if(length(id)==0) {return("Sense dades")}
  if(grepl("^Q[0-9]*$", id)) {
    dades <- dadeswd[dadeswd$item==id,]
    informe <- paste(dades$item, 
                     dades$itemLabel,
                     paste0("https://www.wikidata.org/wiki/",dades$item),
                     sep="\n")
    return(informe)
  } else if(grepl("^pcat[0-9]*$", id)) {
    idpcat <- gsub("pcat", "", id)
    dades <- dfpcat[dfpcat$idpcat==idpcat,]
    informe <- paste(dades$idpcat, 
                     dades$nom,
                     dades$municipi,
                     dades$url,
                     sep="\n")
    return(informe)
  } else if(grepl("^bcn[0-9]*$", id)) {
    idbcn <- gsub("bcn", "", id)
    dades <- dadesbcn[[idbcn]]
    informe <- paste(dades$id, 
                     dades$nom,
                     dades$url,
                     sep="\n")
    return(informe)
  }
}

urlid <- function(id, dadeswd, dfbcn, dfpcat) {
  if(length(id)==0) {return("Sense dades")}
  if(grepl("^Q[0-9]*$", id)) {
    return(paste0("https://www.wikidata.org/wiki/",id))
  } else if(grepl("^pcat[0-9]*$", id)) {
    idpcat <- gsub("pcat", "", id)
    dades <- dfpcat[dfpcat$idpcat==idpcat,]
    return(dades$url)
  } else if(grepl("^bcn[0-9]*$", id)) {
    idbcn <- gsub("bcn", "", id)
    dades <- dfbcn[[idbcn]]
    return(dades$url)
  }
}

nomid <- function(id, dadeswd, dfbcn, dfpcat) {
  if(length(id)==0) {return("Sense dades")}
  if(grepl("^Q[0-9]*$", id)) {
    return(dadeswd$itemLabel[dadeswd$item==id])
  } else if(grepl("^pcat[0-9]*$", id)) {
    idpcat <- gsub("pcat", "", id)
    dades <- dfpcat[dfpcat$idpcat==idpcat,]
    return(dades$nom)
  } else if(grepl("^bcn[0-9]*$", id)) {
    idbcn <- gsub("bcn", "", id)
    dades <- dfbcn[dfbcn$idbcn==idbcn,]
    return(dades$nom)
  }
}

dadesid <- function(id, dadeswd, dfbcn, dfpcat) {
  if(length(id)==0) {return("Sense dades")}
  if(grepl("^Q[0-9]*$", id)) {
    dades <- dadeswd[dadeswd$item==id,]
    dades$nom <- dades$itemLabel
    dades$url <- paste0("https://www.wikidata.org/wiki/",id)
    if(nrow(dades>1)) {dades <- dades[1,]}
    return(dades)
  } else if(grepl("^pcat[0-9]*$", id)) {
    idpcat <- gsub("pcat", "", id)
    dades <- dfpcat[dfpcat$idpcat==idpcat,]
    return(dades)
  } else if(grepl("^bcn[0-9]*$", id)) {
    idbcn <- gsub("bcn", "", id)
    dades <- dfbcn[dfbcn$idbcn==idbcn,]
    return(dades)
  }
}

instid <- function(id, dadeswd, dfbcn=data.frame(), dfpcat=data.frame(), dadeswdinst) {
  if(grepl("^Q[0-9]*$", id)) {
    inst <- dadeswdinst$instLabel[dadeswdinst$item==id]
    return(paste(inst, collapse=", "))
  } else {
    return(character())
  }
}

linkid  <- function(id, dadeswd, dfbcn, dfpcat, dadeswdinst) {
  dades <- dadesid(id, dadeswd, dfbcn, dfpcat)
  if (is.null(id)) {return("Sense dades")}
  tagList(dades$nom, 
          a(id, 
            href=dades$url,
            target="_blank"),
          instid(id, dadeswd, dadeswdinst=dadeswdinst))
}

## funcions aparellar
# funció general aparellar (per substituir les altres)

aparella <- function(instr=character(), qbase, id, dadeswd) {
  # print(qbase) #
  # print(id) #
  if(grepl("^pcat[0-9]*$", id)) {
    prop <- "P12802" 
  } else if(grepl("^bcn[0-9]*$", id)) {
    prop <- "P11557"
  } else {
    return(instr)
  }
  #print(prop) #
  idpelat <- gsub("^(pcat|bcn)", "", id)
  afegir <- paste(qbase, prop, cometes(idpelat), 
                  sep="\t")
  nomwd <- nomid(qbase, dadeswd, dfbcn, dfpcat)
  nompar <- nomid(id, dadeswd, dfbcn, dfpcat)
  #print(nomwd)
  #print(nompar)
  if (length(nomwd) !=1 | length(nompar) !=1) {
    return(instr)
  }
  if (nomwd != nompar) {
    afalias <- paste(qbase, "Aca", cometes(nompar),
                     sep="\t")
    return(c(instr, afalias, afegir))
  } else {
    return(c(instr, afegir))
  }
}

## funcions carregar nou element

## Funcions per poblesdecatalunya.cat

dictipus <- c("Edifici residencial"="Q11755880",
              "Edifici religiós"="Q24398318",
              "Edifici administratiu o institucional"="Q2519340",
              "Edifici industrial o agrícola"="Q1662011",
              "Torres i dipòsits d'aigua"="Q274153",
              "Pintura mural urbana (grafit)"="Q219423",
              "Museu"="Q33506",
              "Edifici públic"="Q294422",
              "Art Públic"="Q4989906",
              "Parcs i jardins"="Q22746",
              "Hotels i hostals"="Q63099748",
              "Conjunt urbanístic"="Q66626342",
              "Masia"="Q585956",
              "Edifici escolar"="Q1244442")

dictipusen <- c("Edifici residencial"="Residential building",
                "Edifici religiós"="Religious building",
                "Edifici administratiu o institucional"="Administrative building",
                "Edifici industrial o agrícola"="Industrial or agricultural building",
                "Torres i dipòsits d'aigua"="Water tower",
                "Pintura mural urbana (grafit)"="Grafitto",
                "Museu"="Museum",
                "Muralles i torres fortificades"="Wall or tower",
                "Edifici públic"="Public building",
                "Art Públic"="Public art",
                "Parcs i jardins"="Public garden",
                "Hotels i hostals"="Hotel or hostel building",
                "Conjunt urbanístic"="Urbanistic ensemble",
                "Masia"="Masia",
                "Edifici escolar"="School building")

dictipusdesc <- c("Hotels i hostals"="Edifici hoteler")

fqtipus <- function(tipus, nom="") {
  qtipus <- dictipus[tipus]
  if (is.na(qtipus)) {
    if (grepl("Edifici", tipus)) {
      qtipus <- "Q41176"
    } else if(grepl("Muralles", tipus) & grepl("^Torre", nom)) {
      qtipus <- "Q20034791"
    } else {
      qtipus <- "Q811979"
    } 
  }
  return(qtipus)
} 

dicestil <- c("modernisme"="Q1122677",
              "modernista"="Q1122677",
              "noucentisme"="Q1580216",
              "noucentista"="Q1580216",
              "historicisme"="Q51879601",
              "historicista"="Q51879601",
              "romànic"="Q46261",
              "gòtic"="Q176483",
              "gòtic tardà"="Q10924220",
              "renaixentista"="Q236122",
              "renaixement"="Q236122",
              "barroc"="Q840829",
              "neoclàssic"="Q54111",
              "neoclassicisme"="Q54111",
              "neogòtic"="Q186363",
              "historicisme neogòtic"="Q186363",
              "neomudèjar"="Q614624",
              "neoromànic"="Q744373",
              "arquitectura popular"="Q930314",
              "popular"="Q930314",
              "obra popular"="Q930314",
              "eclecticisme"="Q2479493",
              "eclèctic"="Q2479493",
              "racionalisme"="Q2535546",
              "racionalista"="Q2535546",
              "arquitectura del ferro"="Q900557",
              "arquitectura industrial"="Q1477476")

# coses que no són municipis posades com a municipi
dicmun <- c("Monestir de Sant Cugat del Vallès"="Q13936")

completapcat <- function(dades) {
  qmun <- idescat$lloc[tolower(idescat$llocLabel)==tolower(dades$municipi)]
  if (length(qmun)==0) {
    qmun <- idescatAlias$lloc[tolower(idescatAlias$alias)==tolower(dades$municipi)]
    if (length(qmun)==0) {
      qmun <- dicmun[dades$municipi]
    }
    if (length(qmun)==1 & !is.na(qmun)) {
      dades$municipi <- idescat$llocLabel[idescat$lloc==qmun]
    } 
  }
  dades$qmun <- qmun
  dades$qestil <- dicestil[tolower(dades$estil)]
  dades$qtipus <- fqtipus(dades$tipus)
  dades$tipusen <- dictipusen[dades$tipus]
  if (dades$tipus %in% names(dictipusdesc)) {
    dades$tipus <- dictipusdesc[dades$tipus]
  }
  if (is.na(dades$tipusen)) {
    dades$tipusen <- identifica(dades$nom)$terme["en"] # per si de cas no existeix al diccionari
  }
  dades$idpcat <- gsub("^.*=", "", dades$url)
  return (dades)
}

## funcions catàleg barcelona

# diccionaris i dades
# inclou format web i text
diccprot <- c(CPA_03_PL="Q107542530",
              "Béns d'interès documental(D)"="Q112670375",
              "Establiments d'interès(E2)"="Q115459499")
districtes <- 
  structure(list(num = 1:10, 
                 item = c("Q941385", "Q64124", "Q753075", 
                          "Q959944", "Q1765582", "Q852697", "Q1771488", "Q1641049", "Q1650230", 
                          "Q250935"), 
                 label = c("Ciutat Vella", "Eixample", "Sants-Montjuïc", 
                           "Districte de les Corts", "Sarrià - Sant Gervasi", "Gràcia", 
                           "Horta-Guinardó", "Nou Barris", "Sant Andreu", "Sant Martí"
                 )), 
            row.names = 1:10, class = "data.frame")
districtes$curt <- gsub("Districte de ","", districtes$label)
districtes$curt <- gsub(" - ","-", districtes$curt)

completabcn <- function(dades) {
  nt <- identifica(dades$nom)
  dades$terme <- nt$terme
  dades$qtipus <- nt$qtipus
  dades$qestil <- dicestil[tolower(dades$estil)][1]
  nomdistricte <- districtes$label[districtes$item==dades$districte]
  nomdistricte <- gsub("Districte de ","", nomdistricte)
  dades$aldistricte <- paste("al districte de", nomdistricte)
  dades$aldistricteen <- paste("in", nomdistricte, "district")
  return(dades)
}

# treu el tipus a partir del nom
identifica <- function(nom, 
                       default=list(qtipus = "Q41176", 
                                    terme = c("ca"="edifici", "en"="building"))
) {
  if (grepl("aqüeducte", tolower(nom))) {
    qtipus <- "Q18870689"
  } else if (grepl("pont|viaducte|passera|aqüeducte", tolower(nom))) {
    qtipus <- "Q12280"
    terme <-  c("ca"="pont", "en"="bridge")
  } else if (grepl("font", tolower(nom))) {
    qtipus <- "Q483453"
    terme <- c("ca"="font", "en"="fountain")
  } else if (grepl("església", tolower(nom))) {
    qtipus <- "Q16970"
    terme <- c("ca"="església", "en"="church")
  } else if (grepl("^mas((over)?ia)? ", tolower(nom))) {
    qtipus <- "Q585956"
    terme <- c("ca"="masia", "en"="masia")
  } else if (grepl("^(casa |can |ca n'|cal |ca l'|cases |habitatge )", tolower(nom))) {
    qtipus <- "Q3947"
    terme <- c("ca"="casa", "en"="house")
  } else if (grepl("^(cova|coves|gruta) ", tolower(nom))) {
    qtipus <- "Q35509"
    terme <- c("ca"="cova", "en"="cave")
  } else if (grepl("^avencs? ", tolower(nom))) {
    qtipus <- "Q1435994"
    terme <- c("ca"="avenc", "en"="pit cave")
  } else if (grepl("^ba[lu]m(a|es) ", tolower(nom))) {
    qtipus <- "Q35509"
    terme <- c("ca"="balma", "en"="rock shelter")
  } else if (grepl("^barrac(a|es) ", tolower(nom))) {
    qtipus <- "Q2932238"
    terme <- c("ca"="barraca de vinya", "en"="dry stone hut")
  } else if (grepl("^mol(í|ins) de vent", tolower(nom))) {
    qtipus <- "Q38720"
    terme <- c("ca"="molí de vent", "en"="windmill")
  } else if (grepl("^mol(í|ins) ", tolower(nom))) {
    qtipus <- "Q185187"
    terme <- c("ca"="molí", "en"="mill")
  } else if (grepl("museu ", tolower(nom))) {
    qtipus <- "Q33506"
    terme <- c("ca"="museu", "en"="museum")
  } else if (grepl("^(antiga )?(farmàcia) ", tolower(nom))) {
    qtipus <- "Q13107184"
    terme <- c("ca"="farmàcia", "en"="pharmacy")
  } else if (grepl("^monuments? ", tolower(nom))) {
    qtipus <- "Q4989906"
    terme <- c("ca"="monument", "en"="monument")
  } else if (grepl("^min(a|es) ", tolower(nom))) {
    qtipus <- "Q820477"
    terme <- c("ca"="mina", "en"="mine")
  } else if (grepl("^pedrer(a|es) ", tolower(nom))) {
    qtipus <- "Q188040"
    terme <- c("ca"="pedrera", "en"="quarry")
  } else if (grepl("^bass(a|es) ", tolower(nom))) {
    qtipus <- "Q3253281"
    terme <- c("ca"="bassa", "en"="pond")
  } else if (grepl("^gorg(a|ues)? ", tolower(nom))) {
    qtipus <- "Q2385513"
    terme <- c("ca"="gorg", "en"="stream pond")
  } else if (grepl("^resclosa ", tolower(nom))) {
    qtipus <- "Q1066997"
    terme <- c("ca"="resclosa", "en"="weir")
  } else if (grepl("^forns? de calç ", tolower(nom))) {
    qtipus <- "Q59772"
    terme <- c("ca"="forn de calç", "en"="lime kiln")
  } else if (grepl("^cementiri ", tolower(nom))) {
    qtipus <- "Q39614"
    terme <- c("ca"="cementiri", "en"="cemetery")
  } else if (grepl("^xemeneia", tolower(nom))) {
    qtipus <- "Q2962545"
    terme <- c("ca"="xemeneia", "en"="chimney")
  } else if (grepl("^creu ", tolower(nom))) {
    qtipus <- "Q2309609"
    terme <- c("ca"="creu", "en"="cross")
  } else if (grepl("^escultura ", tolower(nom))) {
    qtipus <- "Q860861"
    terme <- c("ca"="escultura", "en"="sculpture")
  } else if (grepl("^pous? de (gel|glaç|neu) ", tolower(nom))) {
    qtipus <- "Q3666499"
    terme <- c("ca"="pou de gel", "en"="ice cellar")
  } else if (grepl("^bòbila", tolower(nom))) {
    qtipus <- "Q198632"
    terme <- c("ca"="bòbila", "en"="brickworks")
  } else if (grepl("^nau industrial", tolower(nom))) {
    qtipus <- "Q9049015"
    terme <- c("ca"="nau industrial", "en"="industrial building")
  } else if (grepl("^estació d'aforament", tolower(nom))) {
    qtipus <- "Q505774"
    terme <- c("ca"="estació d'aforament", "en"="stream gauge")
  } else if (grepl("^rellotge de sol ", tolower(nom))) {
    qtipus <- "Q80793"
    terme <- c("ca"="rellotge de sol", "en"="sundial")
  } else if (grepl("^plaça", tolower(nom))) {
    qtipus <- "Q174782"
    terme <- c("ca"="plaça", "en"="square")
  } else if (grepl("^carrer ", tolower(nom))) {
    qtipus <- "Q79007"
    terme <- c("ca"="carrer", "en"="street")
  } else if (grepl("^barri ", tolower(nom))) {
    qtipus <- "Q123705"
    terme <- c("ca"="barri", "en"="neighborhood")
  } else if (grepl("^pèrgol(a|es)[- ]", tolower(nom))) {
    qtipus <- "Q264458"
    terme <- c("ca"="pèrgola", "en"="pergola")
  } else if (grepl("^(safareig|rentador)", tolower(nom))) {
    qtipus <- "Q1690211"
    terme <- c("ca"="safareig", "en"="washhouse")
  } else if (grepl("^sureres ", tolower(nom))) {
    qtipus <- "Q5688661"
    terme <- c("ca"="sureda", "en"="cork oak woodland")
  } else if (grepl("^xaragalls? ", tolower(nom)) ) { 
    qtipus <- "Q17300700"
    terme <- c("ca"="xaragall", "en"="badlands")
  } else if (grepl("habitatge", tolower(nom)) ) { 
    qtipus <- "Q11755880"
    terme <- c("ca"="edifici residencial", "en"="residential building")
  } else if (grepl("^conjunt ", tolower(nom)) ) { 
    qtipus <- "Q66626342"
    terme <- c("ca"="conjunt urbà", "en"="urban ensemble")
  } else {
    qtipus <- default$qtipus
    terme <- default$terme
  }
  return (list(qtipus=qtipus, terme=terme))
}


## funcions generals llegir

# funció comuna d'arreglar el llegit per tots els origens
poleix_llegit <- function(dades) {
  dades$nom <- gsub("</?i>","",dades$nom)
  return(dades)
}

llegeix <- function(id) {
  if (grepl("^pcat", id)) {
    id <- as.numeric(gsub("pcat","",id))
    dades <- bdpcat[[id]]
  } else if (grepl("^bcn", id)) {
    id <- gsub("bcn","",id)
    dades <- dadesbcn[[id]]
  } else {
    dades <- list()
  }
  dades <- poleix_llegit(dades)
  return(dades)
}

## Funcions per carregar

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

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
  denom <- gsub("^d'Els ", "dels ", denom)
  denom <- gsub("^d'El ", "del ", denom)
  return(denom)
}

#minúscules
minart <- function(nom) {
  nom <- gsub("^El ","el ", nom)
  nom <- gsub("^Els ","els ", nom)
  nom <- gsub("^La ","la ", nom)
  nom <- gsub("^Les ","les ", nom)
  nom <- gsub("^L'","l'", nom)
  return(nom)
}

mintitol <- function(nom) {
  nom <- str_to_title(nom)
  nom <- gsub(" D'", " d'", nom)
  nom <- gsub(" De ", " de ", nom)
  nom <- gsub(" Del ", " del ", nom)
  nom <- gsub(" Dels ", " dels ", nom)
  nom <- gsub(" El "," el ", nom)
  nom <- gsub(" Els "," els ", nom)
  nom <- gsub(" La "," la ", nom)
  nom <- gsub(" Les "," les ", nom)
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

datas <- function(any=NA, segle=NA) {
  if (!is.na(any)) {
    any <- gsub("^c. ", "", any)
    any <- gsub(" c.", "", any)
    any <- gsub("?", "", any, fixed=TRUE)
    if (grepl("-",any)) {
      any <- strsplit(any,"-")[[1]][1]
      return(paste0("+",any,"-00-00T00:00:00Z/8"))
    } else {
      return(paste0("+",any,"-00-00T00:00:00Z/9"))
    }
  } else if (!is.na(segle)) {
    inicial <- (0:20)*100+1
    names(inicial) <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", 
                        "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", 
                        "XX", "XXI")
    inici <- inicial[segle]
    if (!is.na(inici)) {
      return(paste0("+",inici,"-00-00T00:00:00Z/7"))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# quick per pobles de catalunya
quickpcat <- function(dades, url=dades$url, qid="LAST", 
                      altres=c(""), descr=TRUE) {
  curl <- cometes(url)
  if (qid=="LAST") {
    instr <- list(c("CREATE"))
    Lca <- "Lca"
    Len <- "Len"
  } else {
    instr <- list()
    Lca <- "Aca"
    Len <- "Aen"
  }
  instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
  instr <- afegeix(instr, c(qid, Len, cometes(dades$nom)))  
  if (descr) {
    instr <- afegeix(instr,c(qid, "Dca", 
                             cometes(paste(dades$tipus,
                                           de(minart(dades$municipi))))))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes(paste(dades$tipusen, 'in', 
                                            dades$municipi,
                                            '(Catalonia)'))))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$qmun, 
                            "S248", "Q119625160", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P625", 
                            paste0("@", gsub("; ","/", dades$coords)),
                            "S248", "Q119625160", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P17", "Q29")) 
  instr <- afegeix(instr, c(qid, "P973", curl, "P407", "Q7026"))
  if (!is.na(dades$qtipus)) {
    instr <- afegeix(instr, c(qid, "P31", dades$qtipus, 
                              "S248", "Q119625160", "S854", curl))  
  }
  if (length(dades$qestil)>0) {
    if (!is.na(dades$qestil)) {
      instr <- afegeix(instr, c(qid, "P149", dades$qestil, 
                                "S248", "Q119625160", "S854", curl))  
    }
  }
  if (!is.na(dades$adreca)) {
    instr <- afegeix(instr, c(qid, "P6375", paste0("ca:",cometes(dades$adreca)), 
                              "S248", "Q119625160", "S854", curl))
  }
  if (!is.na(dades$titol)) {
    instr <- afegeix(instr, c(qid, "P1476", paste0("ca:",cometes(dades$titol)), 
                              "S248", "Q119625160", "S854", curl))
    instr <- afegeix(instr, c(qid, "Aca", cometes(dades$titol)))
  }
  instr <- afegeix(instr, c(qid, "P12802", cometes(dades$idpcat), 
                            "S248", "Q119625160", "S854", curl))
  return(instr)
}

# instruccions pel quickstatement ajuntament barcelona
quickbcn <- function(dades, url=dades$url, qid="LAST", 
                     altres=c(""), descr=TRUE, 
                     desambigua=FALSE) {
  curl <- cometes(url)
  if (qid=="LAST") {
    instr <- list(c("CREATE"))
    Lca <- "Lca"
    Len <- "Len"
  } else {
    instr <- list()
    Lca <- "Aca"
    Len <- "Aen"
  }
  if (desambigua==FALSE | Lca=="Aca" |
      !grepl("^((edifici|torre|habitatge)s? ?(industrials?|d'habitatges?)|xemeneia|torres?)( (en cantonada|(uni|pluri)familiars?))?( \\(.*\\))?$", 
             tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
    instr <- afegeix(instr, c(qid, Len, cometes(dades$nom)))
  } else {
    nomdesamb <- paste0(dades$nom, " (", dades$id,")")
    instr <- afegeix(instr, c(qid, Lca, cometes(nomdesamb)))
    instr <- afegeix(instr, c(qid, Len, cometes(nomdesamb)))
    instr <- afegeix(instr, c(qid, "Aca", cometes(dades$nom)))
    instr <- afegeix(instr, c(qid, "Aen", cometes(dades$nom)))
  }
  if (descr) {
    instr <- afegeix(instr,c(qid, "Dca", 
                             cometes(paste(dades$terme["ca"]," de Barcelona",
                                           dades$aldistricte))))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes(paste(dades$terme["en"], "in Barcelona",
                                            dades$aldistricteen))))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$districte, 
                            "S248", "Q116698266", "S854", curl))
  if ("coord" %in% names(dades)) {
    instr <- afegeix(instr, c(qid, "P625", 
                              paste0("@", paste0(dades$coord[2:1], 
                                                 collapse="/")),
                              "S248", "Q116698266", "S854", curl))  
  }
  instr <- afegeix(instr, c(qid, "P17", "Q29")) 
  instr <- afegeix(instr, c(qid, "P973", curl, "P407", "Q7026"))
  instr <- afegeix(instr, c(qid, "P11557", cometes(dades$id), 
                            "S248", "Q116698266", "S854", curl))
  if (!is.na(dades$qtipus)) {
    instr <- afegeix(instr, c(qid, "P31", dades$qtipus, 
                              "S248", "Q116698266", "S854", curl))  
  }
  if (length(dades$qprot)>0) {
    if (!is.na(dades$qprot)) {
      instr <- afegeix(instr, c(qid, "P1435", dades$qprot, 
                                "S248", "Q116698266", "S854", curl))  
    }
  }
  if (length(dades$qestil)>0) {
    if (!is.na(dades$qestil)) {
      instr <- afegeix(instr, c(qid, "P149", dades$qestil, 
                                "S248", "Q116698266", "S854", curl))  
    }
  }
  if ("adreca" %in% names(dades)) {
    if (!is.na(dades$adreca)) {
      instr <- afegeix(instr, c(qid, "P6375", paste0("ca:",cometes(dades$adreca)), 
                                "S248", "Q116698266", "S854", curl))
    }
  }
  return(instr)
}

# quick comuú
quick <- function(dades, url=NULL, qid="LAST", altres=c(""), descr=TRUE, 
                  p131="", desambigua=FALSE) {
  if (is.null(url)) {url <- dades$url}
  if (grepl("poblesdecatalunya.cat",url, fixed = TRUE)) {
    instr <- quickpcat(dades, url, qid=qid, altres=altres, descr=descr)
  } else if (grepl("ajuntament.barcelona.cat/informaciourbanistica",url, 
                   fixed = TRUE)) {
    instr <- quickbcn(dades, desambigua=desambigua)
  } else {
    return(list())
  }
  if (length(p131)==1 & nchar(p131)>1) {
    instr <- afegeix(instr, c(qid, "P131", p131))
  }
  return(instr)
}

instrnou <- function(id, barri="", desambigua=TRUE) {
  if (grepl("^pcat", id)) {
    instr <- unlist(quick(completapcat(llegeix(id)), 
                          p131=barri, desambigua=desambigua))
  } else if (grepl("^bcn", id)) {
    instr <- unlist(quick(completabcn(llegeix(id)), 
                          p131=barri, desambigua=desambigua))
  } else {
    instr <- character()
  }
  return(instr)
}

# ajuntar comandes si són del mateix element
unirmateix <- function(comandes) {
  comandes <- comandes[!duplicated(comandes)]
  # deixar només unes coordenades
  icord <- grep("LAST\tP625", comandes, fixed=TRUE)
  comandes[icord]
  if(length(icord)>1) {
    iaj <- grep("ajuntament.barcelona.cat", comandes[icord], fixed=TRUE)
    ipc <- grep("poblesdecatalunya.cat", comandes[icord], fixed=TRUE)
    if (length(iaj)>0 & length(ipc)>0) {
      iaj <- icord[iaj]
      comandes <- comandes[-iaj]
    }
  }
  # deixar només un nom en català (la resta àlies) i el mateix en anglès
  inom <- grep("LAST\tLca", comandes, fixed=TRUE)
  cnoms <- comandes[inom]
  if (length(cnoms)>1) {
    cnomsbons <- cnoms[!grepl("\"((edifici|habitatge|torre)s? ?(industrial|d'habitatges?)|xemeneia)( en cantonada)?\"",
                              tolower(cnoms))]
  } else {
    cnomsbons <- cnoms
  }
  if (length(cnomsbons)==0) {
    cnomsbons <- cnoms
  }
  if (length(cnomsbons)>1) {
    cnomsbons <- cnomsbons[order(nchar(cnomsbons), decreasing = TRUE)][1]
  }
  cnoms <- ifelse(cnoms == cnomsbons, cnoms, gsub("\tLca\t","\tAca\t", cnoms, fixed=TRUE))
  cnomsen <- gsub("\tLca\t","\tLen\t", cnoms, fixed=TRUE)
  cnomsen <- gsub("\tAca\t","\tAen\t", cnomsen, fixed=TRUE)
  comandes <- comandes[!grepl("\t[LA](ca|en)\t", comandes)]
  comandes <- c(comandes[1], cnoms, cnomsen, tail(comandes,-1))
  # deixar una sola descripció en català
  idesca <- grep("\tDca\t", comandes, fixed=TRUE)
  cdesca <- comandes[idesca]
  cdescabo <- cdesca[order(nchar(cdesca), decreasing = TRUE)][1]
  comandes <- comandes[-idesca]
  comandes <- c(comandes, cdescabo)
  
  idesen <- grep("\tDen\t", comandes, fixed=TRUE)
  cdesen <- comandes[idesen]
  cdesenbo <- cdesen[order(nchar(cdesen), decreasing = TRUE)][1]
  comandes <- comandes[-idesen]
  comandes <- c(comandes, cdesenbo)
  return(comandes)
}

## Funcions per actualitzar dades des de Wikidata

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

treucoord <- function(x) {
  coord <- gsub("Point\\(","",x$coord)
  coord <- gsub(")","",coord)
  coord <- as.data.frame(strsplit(coord, " "))
  coord <- as.data.frame(t(coord))
  coord <- as.data.frame(lapply(coord, as.numeric))
  names(coord) <- c("lon","lat")
  x <- cbind(x, coord)
  return(x)
}

# elements del catàleg de Barcelona
getwdbcn <-  function() {
  consulta4 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P11557 ?idbcn.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  OPTIONAL {?item wdt:P12802 ?idpcat}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'
  return(treucoord(getquery(consulta4)))
}


# elements de poblesdecatalunya.cat
getwdpcat <- function() {
  consulta5 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  ?item wdt:P12802 ?idpcat.
  OPTIONAL {  ?item wdt:P11557 ?idbcn.}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'
  return(treucoord(getquery(consulta5)))
}

# substituir/actualitzar a dadeswd
substdadeswd <- function(dadesvelles, dadesact) {
  dadesqueden <- dadesvelles[!dadesvelles$item %in% dadesact$item,]
  dadesnoves <- rbind(dadesqueden, dadesact)
  dadesnoves <- dadesnoves[!duplicated(dadesnoves),]
}


# Define server logic 
server <- function(input, output) {
  
  dadeswdbrut <- reactiveVal(value=dadeswd)
  
  dwdinst <- reactive(dadeswdbrut()[,c("item", "itemLabel", "inst","instLabel")] )
  dwd <- reactive(dadeswdbrut()[!duplicated(dadeswdbrut()$item),])
  
  # Anem separant: elements de Wikidata
  dwdverm <- reactive(dwd()[is.na(dwd()$idbcn) & is.na(dwd()$idpcat),])
  dwdblau<- reactive(dwd()[!is.na(dwd()$idbcn) & is.na(dwd()$idpcat),])
  dwdverd <- reactive(dwd()[is.na(dwd()$idbcn) & !is.na(dwd()$idpcat),])
  dwdgris <- reactive(dwd()[!is.na(dwd()$idbcn) & !is.na(dwd()$idpcat),])
  
  # Elements de bd
  dbcnblau <- reactive({
    d0 <- dfbcn[!dfbcn$idbcn %in% dwd()$idbcn,]
    d0[!grepl("^(Entorn|Conjunt)",d0$nom),]
    })
  dpcatverd <- reactive(dfpcat[!dfpcat$idpcat %in% dwd()$idpcat,])

  limits <- reactiveValues(lat=c(41.3, 41.45), lon=c(2.05, 2.25))
  
  qitembase <- reactiveVal()

  instruccions <- reactiveVal(value=character()) # instruccions de sortida
  
  instseparades <- reactiveVal(value=character()) # instruccions per remenar abans d'ajuntar

  instpreparades <- reactive({
    if (input$mateixprep) {
      unirmateix(instseparades())
    } else {
      instseparades()
    }
  })
  
  observe({
    clickid <- input$mapa_marker_click$id
    if(length(clickid)==0) {clickid <- "no click"}
    if(isolate(input$eines)=="Aparellar") {
      if(grepl("^Q[0-9]*$", clickid)) {
        qitembase(clickid)
      } else if(length(qitembase())>0) {
        novainstr <- aparella(isolate(instruccions()), 
                              qitembase(), clickid, dadeswd=dwd())
        instruccions(novainstr)
      }
    } else if (isolate(input$eines)=="Importar") {
      novainstr <- c(isolate(instruccions()), 
                     instrnou(clickid, barri=isolate(input$barri3), desambigua=TRUE))
      instruccions(novainstr)
    } else if (isolate(input$eines)=="Importar avançat") {
      novainstr <- c(isolate(instseparades()), 
                     instrnou(clickid, barri=isolate(input$barri4), desambigua=FALSE))
      instseparades(novainstr)
    } else if (isolate(input$eines)=="Explorar") {
      if(grepl("^Q[0-9]*$", clickid)) {
        qitembase(clickid)
      }      
    }
  })
  
  observeEvent(input$esborrar, {
    instruccions(character())
  })

  observeEvent(input$esborrar1, {
    instruccions(head(instruccions(), -1))
  })
  
  observeEvent(input$copiaclip, {
    writeClipboard(paste(instruccions(), collapse="\n"))
  })
  
  observeEvent(input$esborrarprep, {
    instseparades(character())
  })
  
  observeEvent(input$carregarprep, {
    instruccions(c(isolate(instruccions()), isolate(instpreparades())))
    if (isolate(input$esborrarencarregar)) {instseparades(character())}
  })
  
  observeEvent(input$omplirmapa,{
    limits$lat <- c(input$mapa_bounds$south, input$mapa_bounds$north)
    limits$lon <- c(input$mapa_bounds$west, input$mapa_bounds$east)
  })
  
  observeEvent(input$actbcnpcat, {
    nous <- rbind(getwdbcn(), getwdpcat())
    nous <- nous[!duplicated(nous),]
    dadeswd <- substdadeswd(isolate(dadeswdbrut()), nous)
    dadeswdbrut(dadeswd)
    actdadesbcn <- Sys.time()
    actdadespcat<- Sys.time()
    save(dadeswd, actdadeswdcat, actdadesbcn, actdadespcat,
         file="dadeswdcat.RData")
  })
  
  
    output$mapa <- renderLeaflet({
      fesmapa(dadeswdverm=dwdverm(), 
              dadeswdverd=dwdverd(), 
              dadeswdblau=dwdblau(), 
              dadeswdgris=dwdgris(), 
              bcnblau=dbcnblau(), 
              pcatverd=dpcatverd(), 
              limits$lat, limits$lon)
        })
    
    output$instruccions <- renderPrint({
      cat(instruccions(), sep="\n")
    })
    
    output$instr_prep <- renderPrint({
      cat(instpreparades(), sep="\n")
    })
    
    # no usat
    output$itembase <- renderPrint({
      print(qitembase())
    })
    
    # no usat
    output$infbase <- renderText({
      print(informeid(qitembase(), dadeswd, dfbcn, dfpcat))
    })

    output$linkbase <- renderUI({
      linkid(qitembase(), 
             dwd(), dfbcn, dfpcat, dwdinst())
    })

    # no emprat
    output$linkbase2 <- renderUI({
      linkid(qitembase(), 
             dwd(), dfbcn, dfpcat, dwdinst())
    })
    
    output$linkclicat <- renderUI({
      linkid(input$mapa_marker_click$id, 
             dwd(), dfbcn, dfpcat, dwdinst())
    })
    
    output$linkclicat2 <- renderUI({
      linkid(input$mapa_marker_click$id, 
             dwd(), dfbcn, dfpcat, dwdinst())
    })
    
    # no usat
    output$infover <- renderText({
      print(informeid(input$mapa_marker_mouseover$id, dadeswd, dfbcn, dfpcat))
    })

    output$linkover <- renderUI({
      linkid(input$mapa_marker_mouseover$id,
             dwd(), dfbcn, dfpcat, dwdinst())
    })

    output$linkover2 <- renderUI({
      linkid(input$mapa_marker_mouseover$id, 
             dwd(), dfbcn, dfpcat, dwdinst())
    })
    
    output$linkover3 <- renderUI({
      linkid(input$mapa_marker_mouseover$id, 
             dadeswd, dfbcn, dfpcat,dwdinst())
    })
    
    output$linkover4 <- renderUI({
      linkid(input$mapa_marker_mouseover$id, 
             dwd(), dfbcn, dfpcat, dwdinst())
    })
    
    output$dataActualitzacio <- renderText({
      paste(paste("Dades generals:",as.character(actdadeswdcat)),
            paste("Catàleg Barcelona:",as.character(actdadesbcn)),
            paste("Poblesdecatalunya:",as.character(actdadespcat))
            )
      
            })
    
    output$limitsmapa <- renderText({
      paste(paste(c("Latitud:", limits$lat), collapse=" "),
            paste(c("Longitud:", limits$lon), collapse=" "), sep="\n\n")
    })
    
    output$prova <- renderPrint({
      #print(input$eines)
      print(input$mapa_marker_click)
      print(input$mapa_marker_mouseover)
      #print(dadesid(input$mapa_marker_mouseover$id, dadeswd, dfbcn, dfpcat))
      #print(dadesid(qitembase(), dadeswd, dfbcn, dfpcat))
      #print(dadesid(input$mapa_marker_mouseover$id, dadeswd, dfbcn, dfpcat))
      #print(input$barri3)
      #print(input$barri4)
      #print(instruccions())
      #print(input$mapa_bounds)
      #print("Barcelona:")
      #print(head(getwdbcn()))
      #print("PCat:")
      #print(head(getwdpcat()))
      print(table(is.na(dadeswdbrut()$idbcn), is.na(dadeswdbrut()$idpcat)))
      print(table(is.na(dwd()$idbcn), is.na(dwd()$idpcat)))
    })
}

# Define UI for application 
ui <- fluidPage(
  
  # Application title

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(style = "height: 90vh; overflow-y: auto;", 
      titlePanel("Importar fitxes patrimoni"),
      p("Comandes per copiar al Quickstatements:"),
      verbatimTextOutput("instruccions", placeholder=TRUE),
      actionButton("esborrar", "Esborra-ho tot"),
      actionButton("esborrar1", "Esborra'n un"),
      if (Sys.info()["sysname"]=="Windows") {
        actionButton("copiaclip", "Al portapapers")
        } else {""},
      tabsetPanel(id="eines",
                  tabPanel("Aparellar",
                           p(strong("Els id s'ajuntaran a l'item:\n"),
                           #textOutput("infbase"),
                           uiOutput("linkbase")),
                           p(strong("Darrer element sobrevolat:\n"),
                           #textOutput("infover"),
                           uiOutput("linkover"))
                  ),
                  tabPanel("Importar",
                           selectInput("barri3", "Afegir barri:",
                                       vbarris),
                           p(strong("Darrer element importat:\n"),
                           uiOutput("linkclicat")),
                           p(strong("Darrer element sobrevolat:\n"),
                           uiOutput("linkover3")),
                  ),
                  tabPanel("Importar avançat",
                           selectInput("barri4", "Afegir barri:",
                                       vbarris),
                           p("Comandes per afegir:"),
                           verbatimTextOutput("instr_prep", placeholder=TRUE),
                           actionButton("carregarprep", "Afegeix a les instruccions"),
                           actionButton("esborrarprep", "Esborra"),
                           checkboxInput("mateixprep", 
                                         "Totes les dades són del mateix element",
                                         value=TRUE),
                           checkboxInput("esborrarencarregar", 
                                         "Esborrar automàticament en carregar",
                                         value=TRUE),
                           p(strong("Elements clicats:")),
                           uiOutput("linktots"),
                           p(strong("Darrer element sobrevolat:\n",
                           uiOutput("linkover4"))),
                  ),
                  tabPanel("Explorar",
                           p(strong("Darrer element clicat:\n"), 
                           uiOutput("linkclicat2")),
                           p(strong("Darrer element sobrevolat:\n"),
                           uiOutput("linkover2"))
                  ),
                  tabPanel("Mapa",
                           p("Actualització dades:"),
                           textOutput("dataActualitzacio"),
                           p("Límits del mapa:"),
                           textOutput("limitsmapa"),
                           actionButton("omplirmapa","Omplir mapa mostrat"),
                           actionButton("actbcnpcat",
                                        "Actualizar identificadors"))
      ),
      #verbatimTextOutput("itembase")#,
      #p("Sortides de prova:"),
      verbatimTextOutput("prova")
    ),
    
    # Panel principal
    mainPanel(
      leafletOutput("mapa", height = 950)
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
