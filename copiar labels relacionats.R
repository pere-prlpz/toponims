#copiar labels en català a partir d'una altra llengua
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

url <- 'SELECT DISTINCT ?item ?altre ?nameca ?nameor
WHERE {
  VALUES ?alem {wd:Q183 wd:Q40}
  ?altre wdt:P802 ?item.
  ?item wdt:P27 ?alem.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca" .
?item rdfs:label ?nameca
}
SERVICE wikibase:label {
bd:serviceParam wikibase:language "de".
?item rdfs:label ?nameor
}
}'

etiquetes <- getquery(url)

crear <- etiquetes[etiquetes$nameca==etiquetes$item &
                     etiquetes$nameor!=etiquetes$item,]
crear <- crear[!duplicated(crear[,c("item","nameor")]),]
table(crear$nameor==crear$item)

crear$label <- gsub(" [(].*$", "", crear$nameor)

table(crear$label==crear$nameor)
crear[crear$label!=crear$nameor,]
summary(nchar(crear$label))
table(nchar(crear$label)>20)
crear[nchar(crear$label)>20,]

crear$desamb <- gsub("^.*[, ]","",crear$label)
table(crear$desamb)

#subconjunt i simplifica
# crear <- crear[grep("^Rio ",crear$label),]
# crear$label <- gsub("^Rio ", "", crear$label)
# crear <- crear[!grepl("^(do|dos|da|das) ",crear$label),]
# crear <- crear[!grepl("River|Branch|Fork|[Cc]reek|Stream|Bayou|Brook|Run|[,]",crear$label),]
#crear <- crear[nchar(crear$label)<=20,]
#crear <- crear[!grepl("[,]", crear$label),]
#table(grepl("[,]", crear$label))
#crear <- crear[!grepl(" ", crear$label),]

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

