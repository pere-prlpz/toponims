##################
## Carregar de Wikidata
######################

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

consulta <- 'SELECT DISTINCT ?unitat ?unitatLabel ?aca ?aen ?afr ?ade ?aes WHERE {
  ?unitat wdt:P5061 [].
  ?unitat wdt:P5061 ?aen.
  FILTER(LANG(?aen)="en")
  OPTIONAL {
    ?unitat wdt:P5061 ?aca.
  FILTER(LANG(?aca)="ca")
  }
  OPTIONAL {
    ?unitat wdt:P5061 ?afr.
  FILTER(LANG(?afr)="fr")
  }
  OPTIONAL {
    ?unitat wdt:P5061 ?ade.
  FILTER(LANG(?ade)="de")
  }
  OPTIONAL {
    ?unitat wdt:P5061 ?aes.
  FILTER(LANG(?aes)="es")
  }
     SERVICE wikibase:label {bd:serviceParam wikibase:language "ca,en" .}
}'

unitats <- getquery(consulta)

hiha <- apply(unitats[,4:7], 1, function(x) {sum(!is.na(x))})
table(hiha)
igen <- apply(unitats[,4:7], 1, function(x) {sum(x==x[1], na.rm=TRUE)})
table(igen)

unitats[hiha>2 & is.na(unitats$aca),]
unitats[hiha>2 & is.na(unitats$aca) & igen==hiha,]
unitats[hiha>1 & is.na(unitats$aca) & igen==hiha,]

crear <- unitats[hiha>1 & is.na(unitats$aca) & igen==hiha, c("unitat","aen")]

afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# instruccions pel quickstatement
quick <- function(dades) {
  instr <- list()
  instr <- afegeix(instr, c(dades$unitat, "P5061", paste0("ca:",cometes(dades$aen))))
  return(instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
cat(paste(instruccions, collapse="\n"), file="~/varis/instruccions.txt") #pantalla
