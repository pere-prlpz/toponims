# afegir i pujar edificis

#funcions carregar topònims
library(httr)
library(rjson)

treuvar <- function(var, bind) {
  sapply(bind, function(x) (x[[var]]$value))
}

desllista <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(unlist(x))
}

getsparql <- function(url, coornum=TRUE) {
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
  return(df)
}

#Alt Penedès
#llocs <- getsparql("https://query.wikidata.org/sparql?query=SELECT%20%3Fitem%20%3FitemLabel%20%3Fmun%20%3FmunLabel%20%3FitemDescription%20WHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP131*%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP131%20wd%3AQ15355.%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%20%7D%0A%7D")
#Garraf
#llocs <- getsparql("https://query.wikidata.org/sparql?query=SELECT%20%3Fitem%20%3FitemLabel%20%3Fmun%20%3FmunLabel%20%3FitemDescription%20WHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP131*%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP131%20wd%3AQ15354.%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%20%7D%0A%7D")
#Anoia
llocs <- getsparql("https://query.wikidata.org/sparql?query=SELECT%20%3Fitem%20%3FitemLabel%20%3Fmun%20%3FmunLabel%20%3FitemDescription%20WHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP131*%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP131%20wd%3AQ15352.%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%20%7D%0A%7D")

crear <- llocs[!is.na(llocs$itemDescription) & llocs$itemDescription=="building in Tarragona Province, Spain",]
crear$munLabel <- gsub(" (municipality)","",crear$munLabel, fixed = TRUE)
#crear$descrip <- paste0("building in ", crear$munLabel, ", Alt Penedès, Catalonia")
#crear$descrip <- paste0("building in ", crear$munLabel, ", Garraf, Catalonia")
crear$descrip <- paste0("building in ", crear$munLabel, ", Anoia, Catalonia")

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}
crear$qitem <- itemq(crear$item)

cometes <- function(text) {
  paste0('"',text,'"')
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr[[1]] <- c(fila$qitem, "Den", cometes(fila$descrip))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}


instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
