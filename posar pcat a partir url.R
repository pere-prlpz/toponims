# posar poblesdecatalunya.cat a partir de la URL
## funcions generals
library(httr)
library(rjson)


## dades de Wikidata
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

consulta <- 'SELECT DISTINCT ?item ?itemLabel ?url WHERE {
  ?item wdt:P973 ?url.
  FILTER regex(STR(?url), "poblesdecatalunya\\\\.cat")
      SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
  MINUS {?item wdt:P12802 []}
}'

dadeswd <- getquery(consulta)

crear <- dadeswd
crear$idpcat <- gsub("https://www.poblesdecatalunya.cat/element.php?e=","",
                     crear$url, fixed=TRUE)

# instruccions pel quickstatement ajuntament barcelona
quick <- function(dades, url=dades$url, qid=dades$item, altres=c(""), descr=TRUE) {
  instr <- list()
  instr <- afegeix(instr, c(qid, "P12802", cometes(dades$idpcat)))  
  return(instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
cat(paste(instruccions, collapse="\n"), file="~/varis/instruccions.txt") #pantalla
