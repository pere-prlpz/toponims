#traduir comtats estatunidencs
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

url <- 'SELECT DISTINCT ?item ?nameca ?nameor
WHERE {
  ?item wdt:P31/wdt:P279* wd:Q47168.
  ?item wdt:P17 wd:Q30.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca" .
?item rdfs:label ?nameca
}
SERVICE wikibase:label {
bd:serviceParam wikibase:language "en".
?item rdfs:label ?nameor
}
}'

etiquetes <- getquery(url)

crear <- etiquetes[etiquetes$nameca==etiquetes$item &
                     etiquetes$nameor!=etiquetes$item,]

crear$posade <- grepl(" County.*$", crear$nameor)
table(crear$posade)
crear[!crear$posade,]
crear$nom[crear$posade] <- gsub(" County.*$", "", crear$nameor[crear$posade])
crear$label <- 
  ifelse(crear$posade,
         ifelse(grepl("^[AEIOU]",crear$nom),
                paste0("comtat d'", crear$nom),
                paste("comtat de", crear$nom)),
         crear$nameor)

cometes <- function(text) {
  paste0('"',text,'"')
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr[[1]] <- c(fila$item, "Lca", cometes(fila$label))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")

