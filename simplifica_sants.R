#scripts per posar a Wikidata alies simplificats dels noms de les esglésies

rm(list=ls())

#carregar de Wikidata tots els elements de Catalunya amb P131 
# (copiat d'edificis_ng)
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

#vegueries <- c("Q18265", "Q249461", "Q1030024", "Q1113384", "Q1113390", "Q1462520", "Q1849804", "Q579384")
#comamb <- c("Q12600", "Q13948", "Q14303", "Q15348", "Q15351")
partcat <- c("Q18265", "Q1030024", "Q1113384", "Q1113390", "Q1462520", "Q1849804", "Q579384",
             "Q12600", "Q13948", "Q14303", "Q15348", "Q15351")

# baixa tots els elements d'un lloc de Catalunya
totlloc <- function(qlloc) {
  query <- paste0("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131*wd:",qlloc,".
  ?item wdt:P31 ?tipus.
    ?item wdt:P131* ?mun.
    ?mun wdt:P4335 ?idescat.
  OPTIONAL {
?item p:P625 ?coordinate .
?coordinate psv:P625 ?coordinate_node .
?coordinate_node wikibase:geoLatitude ?lat .
?coordinate_node wikibase:geoLongitude ?lon .
  }
SERVICE wikibase:label {
bd:serviceParam wikibase:language 'ca,oc,en,es,pl,sv,ceb'.
}
}")
  tot <- getquery(query)
  tot <- tot[!duplicated(tot),]
  resum <- tot[nchar(tot$idescat)>2,]
  afegir <- tot[!(tot$item %in% resum$item),]
  resum <- rbind(resum, afegir)
  return(resum)
}

totcat <- data.frame()
for (lloc in partcat) {
  print(lloc)
  totcat <- rbind(totcat, totlloc(lloc))
}
totcat <- totcat[!duplicated(totcat),]

monuments <- getquery("SELECT ?item ?itemLabel ?tipus ?tipusLabel ?lat ?lon ?mun ?idescat
WHERE {
  ?item wdt:P1600 [].
  OPTIONAL {?item wdt:P31 ?tipus.}
    ?item wdt:P131* ?mun.
    ?mun wdt:P4335 ?idescat.
  OPTIONAL {
?item p:P625 ?coordinate .
?coordinate psv:P625 ?coordinate_node .
?coordinate_node wikibase:geoLatitude ?lat .
?coordinate_node wikibase:geoLongitude ?lon .
  }
SERVICE wikibase:label {
bd:serviceParam wikibase:language 'ca,oc,en,es,pl,sv,ceb'.
}
}")

totcat <- rbind(totcat, monuments)
totcat <- totcat[!duplicated(totcat),]

classeswd <- getquery('SELECT DISTINCT ?tipus ?nomtipus
WHERE {
  VALUES ?generes {wd:Q811979 wd:Q839954}
?tipus wdt:P279* ?generes.
SERVICE wikibase:label {
bd:serviceParam wikibase:language "ca,ca,en,es,fr,eu,de,sv" .
?tipus rdfs:label ?nomtipus
}
}')

lloctipus <- merge(totcat, classeswd, by="tipus")

# veure els tipus d'edifici
sort(table(lloctipus$nomtipus), decreasing = TRUE)
length(table(lloctipus$nomtipus))
taula <- sort(table(lloctipus$nomtipus), decreasing = TRUE)
cumsum(taula)/sum(taula)
cbind(1:length(taula), taula, cumsum(taula), cumsum(taula)/sum(taula))

# alias
alies <- getquery("SELECT DISTINCT ?item ?alias
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:Q5705.
  ?item skos:altLabel ?alias.
}") 
aliesedif <- merge(lloctipus, alies, by="item")
aliesedif$itemLabel <- aliesedif$alias
aliesedif$alias <- NULL
aliesedif <- aliesedif[!duplicated(aliesedif),]
lloctipus <- rbind(lloctipus, aliesedif)
lloctipus <- lloctipus[!duplicated(lloctipus),]

oficial <- getquery("SELECT DISTINCT ?item ?oficial
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:Q5705.
  ?item wdt:P1448 ?oficial.
}")
oficialedif <- merge(lloctipus, oficial, by="item")
oficialedif$itemLabel <- oficialedif$oficial
oficialedif$oficial <- NULL
lloctipus <- rbind(lloctipus, oficialedif)
lloctipus <- lloctipus[!duplicated(lloctipus),]
length(unique(lloctipus$item))

# busquem duplicats a Wikidata

# treiem paràmetres dels noms
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les) ","",nom)
  gsub("^l'","",nom)
}

lloctipus$nomnet <- sapply(lloctipus$itemLabel, treupar)
lloctipus$nomrel <- treuart(tolower(lloctipus$nomnet))
lloctipus <- lloctipus[nchar(lloctipus$idescat)>2,] # això potser s'hauria d'haver fet abans

#################################
# part nova (simplificar noms esglésies)

totcat[sample(1:nrow(totcat),5),]
lloctipus[sample(1:nrow(lloctipus),5),]
table(grepl(".*Sant? .* d['e]", lloctipus$nomnet))
head(lloctipus[grepl(".*Sant? .* d['e]", lloctipus$nomnet),])
tail(lloctipus[grepl(".*Sant? .* d['e]", lloctipus$nomnet),])
head(lloctipus[grepl("(església|ermita|capella|convent)?.*Sant? .* d['e]", lloctipus$nomnet),])
tail(lloctipus[grepl("(església|ermita|capella|convent)?.*Sant? .* d['e]", lloctipus$nomnet),])
sants <- lloctipus[grepl("([Ee]sglésia|[Ee]rmita|[Cc]apella|[Cc]onvent|[Oo]ratori).*Sant.? .* d['e]", 
                         lloctipus$nomnet),]
sants$nomsant <- gsub(".*([Ee]sglésia|[Ee]rmita|[Cc]apella|[Cc]onvent|[Oo]ratori).*(Sant.? .*) d['e].*", "\\2", 
                      sants$nomnet)
santstot <- merge(sants, lloctipus, by="item")
jafets <- santstot[santstot$nomnet.y==santstot$nomsant,]
crear <- sants[!(sants$item %in% jafets$item),]

######################3
# sense nom església

sants <- lloctipus[grepl("^Sant.? .* d['e]", 
                         lloctipus$nomnet),]
sants$nomsant <- gsub("^(Sant.? .*) d['e].*", "\\1", 
                      sants$nomnet)
santstot <- merge(sants, lloctipus, by="item")
jafets <- santstot[santstot$nomnet.y==santstot$nomsant,]
crear <- sants[!(sants$item %in% jafets$item),]
sort(table(crear$tipusLabel),descending=TRUE)
table(grepl("Assís|Padua|Jesús|Déu|Salle", crear$nomsant))
crear[grepl("Assís|Padua|Jesús|Déu", crear$nomsant),]
table(crear$nomsant[grepl(" d[e']", crear$nomsant)])
crear <- crear[(!grepl(" d[e']", crear$nomsant))|grepl("Assís|Padua|Jesús|Déu", crear$nomsant),]
table(duplicated(crear$item))
table(duplicated(crear$item, crear$nomsant))
crear <- crear[!duplicated(crear$item, crear$nomsant),]

#######################333
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
  instr <- afegeix(instr,c(fila$item, "Aca", cometes(fila$nomsant)))  
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

idmons <- unique(crear$item[31:4000])
instruccions <- unlist(lapply(idmons, function(i) {quick(crear[crear$item==i,])})) 

# sortides per triar
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")
