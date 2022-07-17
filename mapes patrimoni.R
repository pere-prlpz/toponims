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

#codi municipi
idescat <- getquery("SELECT ?lloc ?llocLabel ?idescat 
WHERE {
  ?lloc wdt:P4335 ?idescat.
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca' .}
}")

save(idescat, file="~\\DADES\\pere\\varis/idescat.RData")

## Per carregar municipis si ja existeixen
#load(file="~\\DADES\\pere\\varis/idescat.RData", verbose=TRUE)

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
  resultat <- c(list(nom=nom, municipi=mun, qmun=qmun, qcons=qcons),lcaracs)
  return(resultat)
}

#url <- "https://patrimonicultural.diba.cat/element/pont-del-cami-de-molnell-a-la-muga-per-vimboca"

#dades <- llegeix(url)

#dades$qmun <- idescat$lloc[idescat$llocLabel==dades$municipi]

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

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

quick <- function(dades, url) {
  curl <- cometes(url)
  instr <- list(c("CREATE"))
  instr <- afegeix(instr,c("LAST", "Lca", cometes(dades$nom)))  
  instr <- afegeix(instr,c("LAST", "Dca", 
                           cometes(paste("pont",
                                         de(dades$municipi)))))
  instr <- afegeix(instr, c("LAST", "Len", cometes(dades$nom)))  
  instr <- afegeix(instr, c("LAST", "Den", 
                            paste('"bridge in', dades$municipi,
                                   '(Catalonia)"')))  
  if (grepl("aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c("LAST", "P31", "Q18870689", 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (grepl("pont|viaducte|passera|aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c("LAST", "P31", "Q12280", 
                              "S248", "Q9028374", "S854", curl))  
  }
  instr <- afegeix(instr, c("LAST", "P131", dades$qmun, 
                            "S248", "Q9028374", "S854", curl))  
  instr <- afegeix(instr, c("LAST", "P625", 
                            paste0("@", dades$latitude,"/", dades$longitude),
                            "S248", "Q9028374", "S854", curl))  
  instr <- afegeix(instr, c("LAST", "P17", "Q29")) 
  if (!is.na(dades$height)) {
    instr <- afegeix(instr, c("LAST", "P2044", 
                              paste0(gsub(" ?m$","",dades$height), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(dades$qcons)) {
    instr <- afegeix(instr, c("LAST", "P5816", dades$qcons, 
                              "S248", "Q9028374", "S854", curl))  
  }
  return(instr)
}

#quick(dades, url)

#instruccions <- paste(unlist(quick(dades, url)), sep="\t", collapse="\n")
#cat(instruccions)

# escriure aquí la url de la fitxa de patrimoni
url <- "https://patrimonicultural.diba.cat/element/aqueducte-de-can-nyac"
cat(enc2utf8(paste(unlist(quick(llegeix(url), url)), sep="\t", collapse="\n")))
