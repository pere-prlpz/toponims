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

dades <- getquery("SELECT DISTINCT ?municipi ?municipiLabel ?comarca ?comarcaLabel ?lat ?lon
WHERE {
  ?municipi wdt:P31 wd:Q33146843.
  ?municipi wdt:P131* ?comarca.
  ?comarca wdt:P31 wd:Q937876.
  ?municipi p:P625 ?coordinate .
?coordinate psv:P625 ?coordinate_node .
?coordinate_node wikibase:geoLatitude ?lat .
?coordinate_node wikibase:geoLongitude ?lon .
  SERVICE wikibase:label {
bd:serviceParam wikibase:language 'ca' . } 
}")

head(dades)

com <- dades[dades$comarcaLabel=="Pla d'Urgell",]
com <- com[order(com$municipiLabel),]
com

fer <- com
fer <- com[com$lat<median(com$lat),] 
fer <- com[com$lat>=median(com$lat),] 
fer <- com[com$lat<quantile(com$lat, probs = 1/3),] 
fer <- com[com$lat>=quantile(com$lat, probs = 1/3) & com$lat<quantile(com$lat, probs = 2/3),] 
fer <- com[com$lat>=quantile(com$lat, probs = 2/3),] 
fer <- com[com$lat>=quantile(com$lat, probs = 12/31),] 
fer <- com[com$lat<=quantile(com$lat, probs = 8/19),] 
fer <- com[com$lat>quantile(com$lat, probs = 8/19),] 
nofet <- com[! com$municipi %in% fer$municipi,]
fer <- nofet[nofet$lat<median(nofet$lat),] 
fer <- nofet[nofet$lat>=median(nofet$lat),] 
fer <- nofet[nofet$lon<median(nofet$lon),] 
fer <- nofet[nofet$lon>=median(nofet$lon),] 
fer <- com[com$lon<median(com$lon),] 
fer <- com[com$lon>=median(com$lon),] 
fer <- com[com$lon<quantile(com$lon, probs=14/30),] 
fer <- com[com$lon>=quantile(com$lon, probs=14/30),] 

fer[order(fer$lon),]

nlluc <- scan(text=" Alpens, Lluçà, Olost, Oristà, Perafita, Prats de Lluçanès, Sant Agustí de Lluçanès, Sant Bartomeu del Grau, Sant Boi de Lluçanès, Sant Martí d'Albars, Sobremunt",
              what=character(), sep=",", quote=NULL)
library(stringr)
nlluc <- str_trim(nlluc)
lluc <- com[com$municipiLabel %in% nlluc,]
fer <- lluc
fer <- com[!(com$municipi %in% fer$municipi),]
fer <- fer[fer$lat<median(fer$lat),] 
fer <- fer[fer$lat<quantile(fer$lat, probs = 1/3),] 
fer <- fer[fer$lat>=quantile(fer$lat, probs = 1/3) & fer$lat<quantile(fer$lat, probs = 2/3),] 
fer <- fer[fer$lat>=quantile(fer$lat, probs = 2/3),] 


cat(paste(fer$municipi, collapse=","))
cat(paste0("wd:",fer$municipi, collapse=" "))
cat(paste0("[[",fer$municipiLabel,"]]", collapse=", "))

com <- dades[dades$comarcaLabel=="Selva",]
com <- com[order(com$municipiLabel),]
com
fer <- com[com$lat-com$lon<quantile(com$lat-com$lon, probs = 1/3),] 
fer <- com[com$lat-com$lon>=quantile(com$lat-com$lon, probs = 1/3) & com$lat-com$lon<quantile(com$lat-com$lon, probs = 2/3),] 
fer <- com[com$lat-com$lon>=quantile(com$lat-com$lon, probs = 2/3),] 

cat(paste(fer$municipi, collapse=","))
cat(paste0("wd:",fer$municipi, collapse=" "))
cat(paste0("[[",fer$municipiLabel,"]]", collapse=", "))

