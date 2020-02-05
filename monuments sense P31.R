library(httr)
library(rjson)

treuvar <- function(var, bind) {
  sapply(bind, function(x) (x[[var]]$value))
}

desllista <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(unlist(x))
}

# monuments sense P31
url <- "https://query.wikidata.org/sparql?query=%23Gats%0ASELECT%20%3Fitem%20%3FitemLabel%20%0AWHERE%20%0A%7B%0A%20%20%3Fitem%20wdt%3AP1600%20%5B%5D.%0A%20%20MINUS%20%7B%0A%20%20%20%20%3Fitem%20wdt%3AP31%20%5B%5D%0A%20%20%7D%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%22.%20%7D%0A%7D"
cont <- fromJSON(rawToChar(content(GET(url))))
nomsvars <- cont$head$vars
llista <- lapply(nomsvars, treuvar, bind=cont$results$bindings)
names(llista) <- nomsvars
llista <- lapply(llista, desllista)
df <- as.data.frame(llista, stringsAsFactors = FALSE)

fonts <- df
fonts <- fonts[grep("[Ff]ont ",fonts$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",fonts$item)
instruccions <- paste(items, "P31", "Q483453", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla

esglesies <- df
esglesies <- esglesies[grep("[Ee]sglésia ",esglesies$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",esglesies$item)
instruccions <- paste(items, "P31", "Q16970", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla

esglesies <- df
esglesies <- esglesies[grep("[Ee]rmita ",esglesies$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",esglesies$item)
instruccions <- paste(items, "P31", "Q56750657", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla

coses <- df
coses <- coses[grep("[Hh]habitatge|[Cc]asa ",coses$itemLabel),]
coses <- coses[-grep("de la [Vv]ila|de( la)? [Cc]asa", coses$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",coses$item)
instruccions <- paste(items, "P31", "Q3947", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla

coses <- df
coses <- coses[grep("[Cc]asa de la [Vv]ila|[Aa]juntament",coses$itemLabel),]
coses <- coses[-grep("de l'[Aa]juntament", coses$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",coses$item)
instruccions <- paste(items, "P31", "Q543654", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla

coses <- df
coses <- coses[grep("[Ee]difici ",coses$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",coses$item)
instruccions <- paste(items, "P31", "Q41176", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla

coses <- df
coses <- coses[grep("[Cc]reu de [Tt]erme ",coses$itemLabel),]

items <- gsub("http://www.wikidata.org/entity/","",coses$item)
instruccions <- paste(items, "P31", "Q2309609", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla
