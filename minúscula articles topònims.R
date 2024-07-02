## Posar en minúscula els articles dels topònims catalans al label en català

##################
## Carregar de Wikidata

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

# diferents origens de dades

querymun <- 'SELECT ?item ?itemLabel {
  ?item wdt:P31 wd:Q33146843.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "ca" .}
}'

queryadm1 <-'SELECT DISTINCT ?item ?itemLabel {
  ?lloc wdt:P31 wd:Q33146843.
  ?item wdt:P131 ?lloc.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31/wdt:P279?/wdt:P279? wd:Q56061.  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "ca" .}
}'

queryvaris1 <- 'SELECT DISTINCT ?item ?itemLabel {
  VALUES ?inst {wd:Q585956 wd:Q123705 wd:Q674950 wd:Q41176 wd:Q3947}
  ?lloc wdt:P31 wd:Q33146843.
  ?item wdt:P131* ?lloc.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31 ?inst.  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "ca" .}
}'

querymunt <- 'SELECT DISTINCT ?item ?itemLabel {
  VALUES ?inst {wd:Q8502 wd:Q207326}
  ?lloc wdt:P31 wd:Q33146843.
  ?item wdt:P131* ?lloc.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31 ?inst.  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "ca" .}
}'

querypob <- 'SELECT DISTINCT ?item ?itemLabel {
  VALUES ?inst {wd:Q3055118 wd:Q11939023}
  ?lloc wdt:P31 wd:Q33146843.
  ?item wdt:P131* ?lloc.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P31 ?inst.  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "ca" .}
}'



# importem (triar-ne un)
#labels <- getquery(querymun); lab1 <- labels
#labels <- getquery(queryadm1); lab2 <- labels
#labels <- getquery(queryvaris1); lab3 <- labels
#labels <- getquery(querymunt); lab4 <- labels
#labels <- rbind(lab1, lab2, lab3, lab4)

# o tots a la vegada (triga una mica)
labels <- rbind(getquery(querymun), getquery(queryadm1), 
                getquery(queryvaris1),getquery(querymunt),
                getquery(querypob))
table(duplicated(labels))
labels <- labels[!duplicated(labels),]

###### Correcció
posar <- labels[grepl("^(L'|(El|La|Els|Les) )",labels$itemLabel),]

posar$label <- posar$itemLabel
posar$label <- gsub("^L'","l'",posar$label)
posar$label <- gsub("^El ","el ",posar$label)
posar$label <- gsub("^La ","la ",posar$label)
posar$label <- gsub("^Les ","les ",posar$label)
posar$label <- gsub("^Els ","els ",posar$label)


##################################################################
# Carregar
##############################################################

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
  instr <- afegeix(instr,c(fila$item, "Lca", cometes(fila$label)))  
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

# preparar instruccions
idmons <- unique(posar$item)
instruccions <- unlist(lapply(idmons, function(i) {quick(posar[posar$item==i,])})) 

# sortides per triar
cat(paste(instruccions, collapse="\n")) #pantalla
#cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/varis/instruccions.txt")
#cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")

