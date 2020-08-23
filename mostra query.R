#com fer una query 
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

vegueries <- getquery('SELECT ?item ?itemLabel
WHERE {?item wdt:P31 wd:Q697390.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca,oc,en,es,pl,sv,ceb" .
}
}
')

vegueries2 <- getquery('SELECT ?item ?itemLabel
WHERE {?item wdt:P31 wd:Q697390.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca,oc,en,es,pl,sv,ceb" .
}
}
', treuurl=FALSE)

comarquesamb <- getquery('SELECT ?item ?itemLabel
WHERE {?item wdt:P31 wd:Q937876.
?item wdt:P131 wd:Q249461.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca,oc,en,es,pl,sv,ceb" .
}
}
')


