# posar mpc a partir de la URL
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

consulta1 <- 'SELECT DISTINCT ?item ?itemLabel ?url WHERE {
  ?item wdt:P973 ?url.
  FILTER regex(STR(?url), "patrimonicultural.diba.cat/element")
      SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
  MINUS {?item wdt:P12860 []}
}'


consulta2 <- 'SELECT DISTINCT ?item ?itemLabel ?url
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* ?mun.
  ?mun wdt:P4335 ?idescat.
  ?item p:P131 ?stlloc.
  ?stlloc prov:wasDerivedFrom ?ref.
  ?ref pr:P854 ?url.
  FILTER regex(STR(?url), "patrimonicultural.diba.cat/element")
  MINUS {?item wdt:P12860 []}
  SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'

dadeswd1 <- getquery(consulta1)
dadeswd2 <- getquery(consulta2)
dadeswd <- rbind(dadeswd1, dadeswd2)
dadeswd <- dadeswd[!duplicated(dadeswd),]

crear <- dadeswd
crear$idmpc <- gsub("https://patrimonicultural.diba.cat/element/","",
                     crear$url, fixed=TRUE)

cometes <- function(text) {
  paste0('"',text,'"')
}

afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

# instruccions pel quickstatement ajuntament barcelona
quick <- function(dades, url=dades$url, qid=dades$item, altres=c(""), descr=TRUE) {
  instr <- list()
  instr <- afegeix(instr, c(qid, "P12860", cometes(dades$idmpc)))  
  return(instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
cat(paste(instruccions, collapse="\n"), file="~/varis/instruccions.txt") #pantalla
