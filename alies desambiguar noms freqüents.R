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
masies <- getquery("SELECT DISTINCT ?item ?municipi ?itemLabel ?municipiLabel
  WHERE {
    ?item wdt:P17 wd:Q29.
    ?item wdt:P31 wd:Q585956.
    ?item wdt:P131* ?municipi.
    ?municipi wdt:P31 wd:Q33146843.
    SERVICE wikibase:label {
        bd:serviceParam wikibase:language 'ca' .
        }
  }")

sort(table(masies$itemLabel), decreasing = TRUE)


alies <- getquery("SELECT DISTINCT ?item ?alias
  WHERE {
    ?item wdt:P17 wd:Q29.
    ?item wdt:P31 wd:Q585956.
    ?item wdt:P131* ?municipi.
    ?municipi wdt:P31 wd:Q33146843.
    ?item skos:altLabel ?alias.
    FILTER(lang(?alias)='ca')
  }")

taula <- table(masies$itemLabel)

frequents <- names(taula[taula>10])

posar <- masies[masies$itemLabel %in% frequents,]

posar$aliasnou <- with(posar, paste0(itemLabel," (",municipiLabel,")"))                        

table(alies$alias %in% masies$aliasnou)

jahison <- alies[alies$alias %in% posar$aliasnou,]

posar <- merge(posar, jahison, by="item", all.x=TRUE)

nocal <- posar[!is.na(posar$alias),]
nocal <- nocal[nocal$aliasnou==nocal$alias,]
posar <- posar[!(posar$item %in% nocal$item),]
posar <- posar[order(posar$aliasnou),]

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

# preparar quickstatemens
quick <- function(quadre) {
  fila <- quadre[1,]
  instr <- list()
  instr <- afegeix(instr,c(fila$item, "Aca", cometes(fila$aliasnou)))  
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

# preparar instruccions
idmons <- unique(posar$item)
instruccions <- unlist(lapply(idmons, function(i) {quick(posar[posar$item==i,])})) 

# sortides per triar
#cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/varis/instruccions.txt")
#cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")

