# confrontar entitats singulars de població Wikidata i nomenclàtor IDESCAT

## funcions generals
library(httr)
library(rjson)

cometes <- function(text) {
  paste0('"',text,'"')
}

afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}


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

## Entitats Wikidata

consulta <- 'SELECT DISTINCT ?item ?itemLabel ?mun ?munLabel ?IDESCATmun WHERE {
  ?item wdt:P31 wd:Q3055118.
  ?item wdt:P131/wdt:P131? ?mun.
  ?mun wdt:P4335 ?IDESCATmun.
  SERVICE wikibase:label {bd:serviceParam wikibase:language "ca".}   
}'

dadeswd <- getquery(consulta)
table(nchar(dadeswd$IDESCATmun))
dadeswd[nchar(dadeswd$IDESCATmun)==10,]
dadeswd[nchar(dadeswd$IDESCATmun)==2,]
dadeswd <- dadeswd[nchar(dadeswd$IDESCATmun)==6,]

## dades IDESCAT
library(readr)
nomenclator <- read_csv("varis/nomenclator (1).csv",
                        skip = 3)
dadesid <- nomenclator[nomenclator$Nivell=="Entitat singular",]
names(dadesid)

## unint
artdavant <- function(x) {
  x <- gsub("^(.*), (l'|el|la|els|les)$","\\2 \\1", x)
  x <- gsub("^l' ", "l'", x)
  return(x)
}

dadesid$nomnet <- artdavant(dadesid$Nom)
names(dadeswd)[c(2,5)] <- c("nomnet","Codi")
units <- merge(dadeswd, dadesid, all=TRUE)
units <- units[order(!is.na(units$munLabel)),]
units <- units[order(!is.na(units$Nivell)),]
units <- units[order(units$Codi),]

## preparar llista
load("~/varis/idescat.RData")

municipis <- idescat[nchar(idescat$idescat)>2,]

infmun <- function(i) {
  dadesmun <- municipis[i,]
  inf <- paste0("# {{Q|", dadesmun$lloc,"}}")
  #trobats <- units[units$Codi==dadesmun$idescat & !is.na(units$item) & !is.na(units$Nivell),]
  trobats <- units[units$Codi==dadesmun$idescat,]
  if(nrow(trobats)>0) {
    inftrobats <- sapply(1:nrow(trobats), 
                         function(j) {infentitats(trobats[j,])})
    inf <- c(inf, inftrobats)
  }
  #return(trobats)
  return(inf)
}

infentitats <- function(dades) {
  paste("## ", dades$nomnet, 
        ifelse(is.na(dades$item),"",paste0("[[:d:", dades$item,"]]")),
        " ", dades$Nivell)
}

#infmun(3)
#infentitats(units[15,])

informe <- sapply(1:nrow(municipis), infmun)
informe <- unlist(informe)

cat(informe, sep="\n", file="~/varis/instruccions.txt")


## Nuclis

consulta <- 'SELECT DISTINCT ?item ?itemLabel ?mun ?munLabel ?IDESCATmun WHERE {
  ?item wdt:P31 wd:Q11939023.
  ?item wdt:P131/wdt:P131? ?mun.
  ?mun wdt:P4335 ?IDESCATmun.
  SERVICE wikibase:label {bd:serviceParam wikibase:language "ca".}   
}'

dadeswd <- getquery(consulta)
table(nchar(dadeswd$IDESCATmun))
dadeswd[nchar(dadeswd$IDESCATmun)==10,]
dadeswd[nchar(dadeswd$IDESCATmun)==2,]
dadeswd <- dadeswd[nchar(dadeswd$IDESCATmun)==6,]

## dades IDESCAT
# library(readr)
# nomenclator <- read_csv("varis/nomenclator (1).csv",
#                         skip = 3)
dadesid <- nomenclator[nomenclator$Nivell=="Nucli",]
names(dadesid)

## unint
artdavant <- function(x) {
  x <- gsub("^(.*), (l'|el|la|els|les)$","\\2 \\1", x)
  x <- gsub("^l' ", "l'", x)
  return(x)
}

dadesid$nomnet <- artdavant(dadesid$Nom)
names(dadeswd)[c(2,5)] <- c("nomnet","Codi")
units <- merge(dadeswd, dadesid, all=TRUE)
units <- units[order(!is.na(units$munLabel)),]
units <- units[order(!is.na(units$Nivell)),]
units <- units[order(units$Codi),]

## preparar llista
municipis <- idescat[nchar(idescat$idescat)>2,]

infmun <- function(i) {
  dadesmun <- municipis[i,]
  inf <- paste0("# {{Q|", dadesmun$lloc,"}}")
  #trobats <- units[units$Codi==dadesmun$idescat & !is.na(units$item) & !is.na(units$Nivell),]
  trobats <- units[units$Codi==dadesmun$idescat,]
  if(nrow(trobats)>0) {
    inftrobats <- sapply(1:nrow(trobats), 
                         function(j) {infentitats(trobats[j,])})
    inf <- c(inf, inftrobats)
  }
  #return(trobats)
  return(inf)
}

infentitats <- function(dades) {
  paste("## ", dades$nomnet, 
        ifelse(is.na(dades$item),"",paste0("[[:d:", dades$item,"]]")),
        " ", dades$Nivell)
}

#infmun(3)
#infentitats(units[15,])

informe <- sapply(1:nrow(municipis), infmun)
informe <- unlist(informe)

cat(informe, sep="\n", file="~/varis/instruccions2.txt")

