# Actualitzar només elements de Wikidata amb identificadors

## Funcions per carregar de Wikidata

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

## Carregar elements amb catàleg

# elements del catàleg de Barcelona
consulta4 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P11557 ?idbcn.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  OPTIONAL {?item wdt:P12802 ?idpcat}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'


# elements de poblesdecatalunya.cat
consulta5 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  ?item wdt:P12802 ?idpcat.
  OPTIONAL {  ?item wdt:P11557 ?idbcn.}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'

dadeswd4 <- getquery(consulta4)
dadeswd5 <- getquery(consulta5)

## Carregar del disc 
load("~/varis/importar_fitxes_mapa/dadeswdcat.RData", verbose=TRUE)

# actualitzar
actdadesbcn <-Sys.time()
actdadespcat <- Sys.time()

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

dadesact <- rbind(dadeswd4, dadeswd5)
dadesact <- treucoord(dadesact)

dadeswd <- dadeswd[!dadeswd$item %in% dadesact$item,]
dadeswd <- rbind(dadeswd, dadesact)
dadeswd <- dadeswd[!duplicated(dadeswd),]

save(dadeswd, actdadeswdcat, actdadespcat, actdadesbcn,
     file="~/varis/importar_fitxes_mapa/dadeswdcat.RData") #actualitzar app
