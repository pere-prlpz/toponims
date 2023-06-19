## codis municipis
# alternativament, load més avall

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
if (file.exists("~\\varis/idescat.RData")) { # PER FER: path adaptable
  load(file="~\\varis/idescat.RData", verbose=TRUE)
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
  save(idescat, idescatAlias, file="~\\varis/idescat.RData")
}


## llegir pàgina mapa patrimoni cultural

library(stringr)

#url <- "https://www.poblesdecatalunya.cat/element.php?e=16490"
#url <- "https://www.poblesdecatalunya.cat/element.php?e=16264"

llegeix <- function(url) {
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
  coords <- pag[grepl('<meta name="geo.position"', pag)]
  coords <- gsub('^.*<meta name="geo.position" content="(.*)"><style type="text/css">.*$',"\\1", coords)
  resultat <- c(list(nom=nom, municipi=mun,  
                     tipus=tipus, estil=estil, coords=coords,
                     url=url))
  return(resultat)
}

llegeix(url)

dictipus <- c("Edifici residencial"="Q11755880",
              "Edifici religiós"="Q24398318",
              "Pintura mural urbana (grafit)"="Q219423",
              "Museu"="Q33506")

dictipusen <- c("Edifici residencial"="Residential building",
              "Edifici religiós"="Religious building",
              "Pintura mural urbana (grafit)"="Grafitto",
              "Museu"="Museum")

fqtipus <- function(tipus) {
  qtipus <- dictipus[tipus]
  if (is.na(qtipus)) {
    if (grepl("Edifici", tipus)) {
      qtipus <- "Q41176"
    } else {
        qtipus <- "Q811979"
      } 
  }
  return(qtipus)
} 

dicestil <- c("Barroc"="Q840829",
              "Modernisme"="Q1122677")

completa <- function(dades) {
  qmun <- idescat$lloc[tolower(idescat$llocLabel)==tolower(dades$mun)]
  if (length(qmun)==0) {
    qmun <- idescatAlias$lloc[tolower(idescatAlias$alias)==tolower(dades$mun)]
    if (length(qmun)==1) {
      mun <- idescat$llocLabel[idescat$lloc==qmun]
    }
  }
  dades$qmun <- qmun
  dades$qestil <- dicestil[dades$estil]
  dades$qtipus <- fqtipus(dades$tipus)
  dades$tipusen <- dictipusen[dades$tipus]
  return (dades)
}

#completa(llegeix(url))


# carregar

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
  return(nom)
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

quick <- function(dades, url=dades$url, qid="LAST", altres=c(""), descr=TRUE) {
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
  if (!is.na(dades$qtipus)) {
    instr <- afegeix(instr, c(qid, "P31", dades$qtipus, 
                              "S248", "Q119625160", "S854", curl))  
  }
  if (!is.na(dades$qestil)) {
    instr <- afegeix(instr, c(qid, "P149", dades$qestil, 
                              "S248", "Q119625160", "S854", curl))  
  }
  return(instr)
}

#quick(completa(llegeix(url)))

totinstr <- function(url, qid="LAST", altres=c(""), descr=TRUE) {
  cat(enc2utf8(
    paste(c(unlist(
      quick(completa(llegeix(url)), url, qid, altres, descr)), 
    sep="\t", collapse="\n"),"\n")))
}

#

totinstr(url)

# element nou (url agafada del portapapers)
totinstr(scan("clipboard", what="character"))



###############################3
# proves
pag <- readLines(url, encoding="UTF-8")
tit <- pag[grepl("<title>.*</title>", pag)]
tit <- gsub("^.*<title>(.*)</title>.*$", "\\1", tit)
tittrossos <- unlist(strsplit(tit, " - "))
nom <- tittrossos[1]
mun <- tittrossos[2]
mun <- gsub("\\|.*$","",mun)
nom
mun
tipus <- pag[grepl("<tr><th>Tipus</th><td>", pag)]
tipus <- gsub("^.*<tr><th>Tipus</th><td>(.*)</td></tr>.*$","\\1", tipus)
tipus
estil <- pag[grepl("<tr><th>Estil</th><td>", pag)]
estil <- gsub("^.*<tr><th>Estil</th><td>(.*)</td></tr>.*$","\\1", estil)
estil
qmun <- idescat$lloc[tolower(idescat$llocLabel)==tolower(mun)]
if (length(qmun)==0) {
  qmun <- idescatAlias$lloc[tolower(idescatAlias$alias)==tolower(mun)]
  if (length(qmun)==1) {
    mun <- idescat$llocLabel[idescat$lloc==qmun]
  }
}
qmun
coords <- pag[grepl('<meta name="geo.position"', pag)]
coords <- gsub('^.*<meta name="geo.position" content="(.*)"><style type="text/css">.*$',"\\1", coords)
