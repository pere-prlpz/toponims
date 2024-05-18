#
# Aplicació per importar a Wikidata fitxes dels mapes de patrimoni
# i altres fonts
#

library(shiny)

## Previs

library(httr)
library(rjson)
#library(sf)
#library(oce)
library(stringr)

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
if (file.exists("idescat.RData")) { # PER FER: path adaptable
  load(file="idescat.RData", verbose=TRUE)
} else {
  idescat <- getquery("SELECT ?lloc ?llocLabel ?idescat 
  WHERE {
  ?lloc wdt:P4335 ?idescat.
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca' .}
  }")
  idescatAlias <- getquery('SELECT ?lloc ?llocLabel ?idescat ?alias
  WHERE {
  ?lloc wdt:P4335 ?idescat.
  ?lloc skos:altLabel ?alias.
  FILTER(LANG(?alias) = "ca").
  SERVICE wikibase:label {bd:serviceParam wikibase:language "ca".}
  }')
  save(idescat, idescatAlias, file="idescat.RData")
}

# per separar les url de l'entrada
separa <- function(x) {
  sep <- unlist(strsplit(x, ",", fixed=TRUE))
  sep <- str_trim(sep)
  return(sep)
}

## funcions per llegir pàgina mapa patrimoni cultural

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
  dada <- gsub(",",".",dada)
  return(dada)  
}

unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

llegeixmp <- function(url) {
  pag <- readLines(url, encoding="UTF-8")
  tit <- pag[grepl("<title>.*</title>", pag)]
  nom <- gsub("^.*<title>(.*)\\|.*</title>.*$", "\\1", tit)
  nom <- str_trim(nom)
  nom <- unescape_html(gsub(" $","",nom))
  mun <- pag[grepl('<meta name="geo.placename" content=".*" />', pag)]
  mun <- gsub('<meta name="geo.placename" content="(.*)" />', "\\1", mun)
  mun <- unescape_html((str_trim(mun)))
  nom <- gsub(paste0("\\. ",mun), "", nom)
  qmun <- idescat$lloc[tolower(idescat$llocLabel)==tolower(mun)]
  diumasia <- any(grepl("[Mm]as((over)?ia)?\\b",pag))
  diuescultura <- any(grepl("[Ee]scultur(a|es)\\b",pag))
  if (length(qmun)==0) {
    qmun <- idescatAlias$lloc[tolower(idescatAlias$alias)==tolower(mun)]
    if (length(qmun)==1) {
      mun <- idescat$llocLabel[idescat$lloc==qmun]
    }
  }
  caracs <- c("height", "latitude", "longitude")
  lcaracs <- lapply(caracs, treucarac, pag)
  names(lcaracs) <- caracs
  itipologia <- grep('<div class="field__label">Tipologia</div>', pag)
  tipologia <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[itipologia+1])
  tipologia <- str_trim(gsub('^.*<div class="field__item">', "", tipologia))
  iest <- grep('<div class="field__label">Estat de conservació</div>', pag)
  cons <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[iest+1])
  estcons <- c("Bo"="Q56557591", "Regular"="Q106379705")
  qcons <- estcons[cons]
  if (length(qcons)==0) {qcons <- NA}
  if (is.na(qcons)) {
    print(paste("Estat de conservació desconegut:", cons))
  }
  iest <- grep('<div class="field__label">Any</div>', pag)
  if (length(iest)==1) {
    any <- gsub('^.*<div class="field__item">(.*)(</div>)?.*$', "\\1", pag[iest+1])
    any <- gsub('</div>', "", any)
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
                any=any, segle=segle, tipologia=tipologia,
                diumasia=diumasia, diuescultura=diuescultura,
                url=url)
  return(resultat)
}

## Funció comuna (però de moment només emprada per ajuntament)

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
  } else {
    qtipus <- default$qtipus
    terme <- default$terme
  }
  return (list(qtipus=qtipus, terme=terme))
}


##########################################################
# Funcions per poblesdecatalunya.cat

llegeixpcat <- function(url) {
  pag <- readLines(url, encoding="UTF-8")
  tit <- pag[grepl("<title>.*</title>", pag)]
  tit <- gsub("^.*<title>(.*)</title>.*$", "\\1", tit)
  tittrossos <- unlist(strsplit(tit, " - "))
  nom <- tittrossos[1]
  mun <- tittrossos[2]
  mun <- trimws(gsub("\\|.*$","",mun))
  tipus <- pag[grepl("<tr><th>Tipus</th><td>", pag)]
  tipus <- gsub("^.*<tr><th>Tipus</th><td>(.*)</td></tr>.*$","\\1", tipus)
  estil <- pag[grepl("<tr><th>Estil</th><td>", pag)]
  estil <- gsub("^.*<tr><th>Estil</th><td>(.*)</td></tr>.*$","\\1", estil)
  estil <- gsub("</td>.*$", "", estil)
  coords <- pag[grepl('<meta name="geo.position"', pag)]
  coords <- gsub('^.*<meta name="geo.position" content="(.*)"><style type="text/css">.*$',"\\1", coords)
  adreca <- pag[grep("Premeu per a veure la situacio de l'element al mapa", pag)+1]
  adreca <- gsub("</td></tr>", "", adreca)
  resultat <- c(list(nom=nom, municipi=mun,  
                     tipus=tipus, estil=estil, coords=coords,
                     adreca=adreca,
                     url=url))
  return(resultat)
}

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
              "arquitectura del ferro"="Q900557")

# coses que no són municipìs posades com a municipi
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
  return (dades)
}

## Funcions aj bcn #####################

load("dades_cataleg_bcn.RData")

# La resta ajuntament no emprada

# diccionaris i dades
diccprot <- c(CPA_03_PL="Q107542530")
districtes <- 
  structure(list(num = 1:10, 
                 item = c("Q941385", "Q64124", "Q753075", 
                          "Q959944", "Q1765582", "Q852697", "Q1771488", "Q1641049", "Q1650230", 
                          "Q250935"), 
                 label = c("Ciutat Vella", "l'Eixample", "Sants-Montjuïc", 
                           "les Corts", "Sarrià - Sant Gervasi", "Gràcia", 
                           "Horta-Guinardó", "Nou Barris", "Sant Andreu", "Sant Martí"
                 )), 
            row.names = 1:10, class = "data.frame")

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


# Funcions comunes llegir #######

llegeix <- function(url) {
  #print(paste("url:", url))
  if (grepl("patrimonicultural.diba.cat",url, fixed = TRUE)) {
    return(llegeixmp(url))
  } else if (grepl("poblesdecatalunya.cat",url, fixed = TRUE)) {
      return(completapcat(llegeixpcat(url)))
  } else if (url %in% names(dadesbcn)) {
    return(completabcn(dadesbcn[[url]]))
  } else {
    return(list())
  }
}


##################################################################
# Funcions per carregar
##############################################################


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

# quick per mapes de patrimoni cultural
quickmp <- function(dades, url, qid="LAST", altres=c(""), descr=TRUE) {
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
  if (grepl("aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q18870689", 
                              "S248", "Q9028374", "S854", curl))  
  } else if (grepl("pont|viaducte|passera|aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q12280", 
                              "S248", "Q9028374", "S854", curl))
    terme <-  c("ca"="pont", "en"="bridge")
  } else if (grepl("font", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q483453", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="font", "en"="fountain")
  } else if (grepl("església", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q16970", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="església", "en"="church")
  } else if ((grepl("^mas((over)?ia)? ", tolower(dades$nom)))|
             (dades$diumasia & dades$tipologia=="Edifici")) {
    instr <- afegeix(instr, c(qid, "P31", "Q585956", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="masia", "en"="masia")
  } else if (grepl("^(casa |can |ca n'|cal |ca l'|cases |habitatge )", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q3947", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="casa", "en"="house")
  } else if (grepl("^(cova|coves|gruta) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q35509", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="cova", "en"="cave")
  } else if (grepl("^avencs? ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q1435994", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="avenc", "en"="pit cave")
  } else if (grepl("^ba[lu]m(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q35509", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="balma", "en"="rock shelter")
  } else if (grepl("^barrac(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2932238", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="barraca de vinya", "en"="dry stone hut")
  } else if (grepl("^mol(í|ins) de vent", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q38720", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="molí de vent", "en"="windmill")
  } else if (grepl("^mol(í|ins) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q185187", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="molí", "en"="mill")
  } else if (grepl("museu ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q33506", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="museu", "en"="museum")
  } else if (grepl("^monuments? ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q4989906", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="monument", "en"="monument")
  } else if (grepl("^min(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q820477", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="mina", "en"="mine")
  } else if (grepl("^pedrer(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q188040", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="pedrera", "en"="quarry")
  } else if (grepl("^bass(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q3253281", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="bassa", "en"="pond")
  } else if (grepl("^gorg(a|ues)? ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2385513", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="gorg", "en"="stream pond")
  } else if (grepl("^resclosa ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q1066997", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="resclosa", "en"="weir")
  } else if (grepl("^forns? de calç ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q59772", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="forn de calç", "en"="lime kiln")
  } else if (grepl("^cementiri ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q39614", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="cementiri", "en"="cemetery")
  } else if (grepl("^xemeneia ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2962545", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="xemeneia", "en"="chimney")
  } else if (grepl("^creu ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2309609", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="creu", "en"="cross")
  } else if (grepl("^escultura ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q860861", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="escultura", "en"="sculpture")
  } else if (grepl("^pous? de (gel|glaç|neu) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q3666499", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="pou de gel", "en"="ice cellar")
  } else if (grepl("^bòbila", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q198632", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="bòbila", "en"="brickworks")
  } else if (grepl("^nau industrial", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q9049015", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="nau industrial", "en"="industrial building")
  } else if (grepl("^estació d'aforament", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q505774", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="estació d'aforament", "en"="stream gauge")
  } else if (grepl("^rellotge de sol ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q80793", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="rellotge de sol", "en"="sundial")
  } else if (grepl("^plaça", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q174782", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="plaça", "en"="square")
  } else if (grepl("^carrer ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q79007", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="carrer", "en"="street")
  } else if (grepl("^barri ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q123705", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="barri", "en"="neighborhood")
  } else if (grepl("^pèrgol(a|es)[- ]", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q264458", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="pèrgola", "en"="pergola")
  } else if (grepl("^(safareig|rentador)", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q1690211", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="safareig", "en"="washhouse")
  } else if (grepl("^sureres ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q5688661", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="sureda", "en"="cork oak woodland")
  } else if (grepl("^xaragalls? ", tolower(dades$nom)) ) { #& dades$tipologia=="Zona d'interès"
    instr <- afegeix(instr, c(qid, "P31", "Q17300700", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="xaragall", "en"="badlands")
  } else if(dades$tipologia=="Edifici") {
    instr <- afegeix(instr, c(qid, "P31", "Q41176", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="edifici", "en"="building")
  } else if(dades$tipologia=="Element urbà") {
    if (dades$diuescultura) {
      instr <- afegeix(instr, c(qid, "P31", "Q860861", 
                                "S248", "Q9028374", "S854", curl))
      terme <- c("ca"="escultura", "en"="sculpture")
    } else {
    instr <- afegeix(instr, c(qid, "P31", "Q13397636", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="element urbà", "en"="urban element")}
  } else if(dades$tipologia=="Element arquitectònic") {
    instr <- afegeix(instr, c(qid, "P31", "Q391414", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="element arquitectònic", "en"="architectural element")
  } else if(dades$tipologia=="Conjunt arquitectònic") {
    instr <- afegeix(instr, c(qid, "P31", "Q1497375", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="conjunt arquitectònic", "en"="architectural ensemble")
    } else if(dades$tipologia=="Conjunt arquitectònic") {
    instr <- afegeix(instr, c(qid, "P31", "Q1497375", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="conjunt arquitectònic", "en"="architectural ensemble")
} else if(dades$tipologia=="Espècimen botànic") {
    instr <- afegeix(instr, c(qid, "P31", "Q811534", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="arbre singular", "en"="remarkable tree")
} else {
    terme <- c("ca"="lloc", "en"="place")
    #print("Instància desconeguda")
  }
  instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
  instr <- afegeix(instr, c(qid, Len, cometes(dades$nom))) 
  instr <- afegeix(instr, c(qid, "Aca", 
                            cometes(
                              paste0(dades$nom," (",
                                     minart(dades$municipi),")"))))
  if (descr & all(!is.na(terme))) {
    instr <- afegeix(instr,c(qid, "Dca", 
                             cometes(
                               paste(terme["ca"],
                                     de(minart(dades$municipi))))))
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
                              paste0(gsub(" ?m$","",dades$height), "U11573"), 
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
  instr <- afegeix(instr, c(qid, "P973", curl, "P407", "Q7026"))
  return(instr)
}

# quick per pobles de catalunya
quickpcat <- function(dades, url=dades$url, qid="LAST", altres=c(""), descr=TRUE) {
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
  return(instr)
}

# instruccions pel quickstatement ajuntament barcelona
quickbcn <- function(dades, url=dades$url, qid="LAST", altres=c(""), descr=TRUE) {
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
                             cometes(paste(dades$terme["ca"]," de Barcelona",
                                           dades$aldistricte))))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes(paste(dades$terme["en"], "in Barcelona",
                                            dades$aldistricteen))))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$districte, 
                            "S248", "Q116698266", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P625", 
                            paste0("@", paste0(dades$coord[2:1], 
                                               collapse="/")),
                            "S248", "Q116698266", "S854", curl))  
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
  return(instr)
}


# quick comuú
quick <- function(dades, url=NULL, qid="LAST", altres=c(""), descr=TRUE) {
  if (is.null(url)) {url <- dades$url}
  url <- gsub("https://ajuntament.barcelona.cat/informaciourbanistica/cerca/ca/fitxa/|/--/--/cp/",
              "", url)
  if (grepl("patrimonicultural.diba.cat",url, fixed = TRUE)) {
    return(quickmp(dades, url, qid=qid, altres=altres, descr=descr))
  } else if (grepl("poblesdecatalunya.cat",url, fixed = TRUE)) {
    return(quickpcat(dades, url, qid=qid, altres=altres, descr=descr))
  } else if (url %in% names(dadesbcn)) {
    return(quickbcn(dades))
  } else {
    return(list())
  }
}

### Aplicació

# Define server logic
server <- function(input, output) {
  
  urlsep <- reactive(separa(input$url))
  
  llegit <- reactive(if (nchar(input$url)>0) {lapply(urlsep(), llegeix)
    } else {
      list()
    })

    output$instr <- renderPrint({
      if (length(llegit())>0) {
        cat(sapply(llegit(), function(x) {
          enc2utf8(paste0(paste(unlist(quick(x)), sep="\t", collapse="\n"), "\n"))
        }))
      } else {
        cat("Sense dades")
      }
    })
    
    output$urlsep <- renderPrint({
      urlsep()
    })

    output$dades <- renderPrint({
      llegit()
    })
    
    output$prova <- renderPrint({
      print(input$url)
      print(head(names(dadesbcn)))
      print(input$url %in% names(dadesbcn))
      if (input$url %in% names(dadesbcn)) {
        print("És bcn")
        print(dadesbcn[[input$url]])
      }
    }) 
}

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Importar fitxes a Wikidata"),
  
  # Introducció de dades
  sidebarLayout(
    sidebarPanel(
      textInput("url", "url de mapa de patrimoni o de poblesdecatalunya 
                o id del catàleg de l'Ajuntament"),
      h3("Instruccions:"),
      p("1. Enganxeu la url d'una fitxa dels mapes de patrimoni de la Diputacion de Barcelona
        o de poblesdecatalunya.cat al camp url, o un número d'identificació 
        del catàleg de patrimoni de l'Ajuntament de Barcelona"),
      p("2. Copieu el codi obtingut al quickstatements"),
      p("3. Comproveu a Wikidata l'element que heu creat"),
      p("Si voleu crear més d'un element a la vegada, entreu les url o els números separats amb comes."),
      p("Algunes coses a tenir en compte:"),
      p("- Compte a no crear duplicats a Wikidata."),
      p("- L'aplicació no es gaire hàbil a l'hora a encertar la instància i la descripció 
        i pot ser que les hàgiu de corregir. Quan no pot deduir una instància la deixa 
        en blanc."),
      p("- Després de crear l'element a Wikidata podeu comprovar si la fitxa
        té dades que l'aplicació no ha fet servir i que valgui la pena copiar
        manualment."),
      p("- Les dades dels mapes de patrimoni i de poblesdecatalunya s'agafen
        de la url que heu enganxat. Les de patrimoni de l'Ajuntament de Barcelona
        s'agafen d'una versió desada de les", 
        a(href="https://opendata-ajuntament.barcelona.cat/data/ca/dataset/patrimoni-arquitectonic-protegit",
          "dades obertes"), " que no inclou tots els elements del catàleg."),
      p("Comentaris i missatges", 
        a(href="https://ca.wikipedia.org/wiki/Usuari_Discussi%C3%B3:Pere_prlpz",
          "aquí"),".")
    ),
    
    # Resultat
    mainPanel(
      "Codi per copiar a QuickStatements:",
      verbatimTextOutput("instr"),
      "Dades entrades:",
      verbatimTextOutput("urlsep"),
      "Dades llegides:",
      verbatimTextOutput(("dades"))#,
      #"Sortida de prova:",
      #verbatimTextOutput(("prova"))
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
