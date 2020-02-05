#interpolar municipis de platges i més accidents costaners

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


# incloc valencians i balears
accidents <- getsparql("https://query.wikidata.org/sparql?query=%23coses%20costaneres%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcoord%20%3Flat%20%3Flon%20%3Fitemun%20%3Fnomun%0AWHERE%20%7B%0A%23%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%2Fwdt%3AP1269%3F%20wd%3AQ93352.%0A%20%20%3Fitem%20wdt%3AP31%20%3Ftipus.%0AOPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0AOPTIONAL%20%7B%0A%20%20%3Fitem%20wdt%3AP131%20%3Fitemun%0A%7D%20%20%0A%20%20%23MINUS%20%7B%3Fitem%20wdt%3AP131%20%5B%5D%7D%0A%20%20FILTER%20%28%3Flat%20>%2037.82%29%0A%20%20FILTER%20%28%3Flon%20>%20-1%29%20%20%0A%20%20FILTER%20%28%3Flat%20<%2042.5%29%0A%20%20FILTER%20%28%3Flon%20<%204%29%20%20%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%20%20%20%20SERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%22%20.%0A%3Fitemun%20rdfs%3Alabel%20%3Fnomun%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
illes <- getsparql("https://query.wikidata.org/sparql?query=%23illes%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcoord%20%3Flat%20%3Flon%20%3Fitemun%20%3Fnomun%0AWHERE%20%7B%0A%23%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ23442.%0AOPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0AOPTIONAL%20%7B%0A%20%20%3Fitem%20wdt%3AP131%20%3Fitemun%0A%7D%20%20%0A%20%20%23MINUS%20%7B%3Fitem%20wdt%3AP131%20%5B%5D%7D%0A%20%20FILTER%20%28%3Flat%20>%2037.82%29%0A%20%20FILTER%20%28%3Flon%20>%20-1%29%20%20%0A%20%20FILTER%20%28%3Flat%20<%2042.5%29%0A%20%20FILTER%20%28%3Flon%20<%204%29%20%20%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%20%20%20%20SERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%22%20.%0A%3Fitemun%20rdfs%3Alabel%20%3Fnomun%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
peninsules <- getsparql("https://query.wikidata.org/sparql?query=%23pen%C3%ADnsules%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcoord%20%3Flat%20%3Flon%20%3Fitemun%20%3Fnomun%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ34763.%0AOPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0AOPTIONAL%20%7B%0A%20%20%3Fitem%20wdt%3AP131%20%3Fitemun%0A%7D%20%20%0A%20%20%23MINUS%20%7B%3Fitem%20wdt%3AP131%20%5B%5D%7D%0A%20%20FILTER%20%28%3Flat%20>%2037.82%29%0A%20%20FILTER%20%28%3Flon%20>%20-1%29%20%20%0A%20%20FILTER%20%28%3Flat%20<%2042.5%29%0A%20%20FILTER%20%28%3Flon%20<%204%29%20%20%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%20%20%20%20SERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%22%20.%0A%3Fitemun%20rdfs%3Alabel%20%3Fnomun%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
platges <- rbind(accidents, illes, peninsules)

# incloc municipis espanyos
municipis <- getsparql("https://query.wikidata.org/sparql?query=%23Municipis%20%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ2074737%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%7D%0A")

platges <- platges[!duplicated(platges[,c("item","itemun")]),]
platges <- platges[platges$lat < 40.5+.7*platges$lon, ] #buidem terra endins

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}

platges$qitem <- itemq(platges$item)
platges$qitemun <- itemq(platges$itemun)
municipis$qitemun <- itemq(municipis$item)
platges$fet <- platges$qitemun %in% municipis$qitemun
platges$mun0 <- ifelse(platges$fet, platges$qitemun, NA)
platges$mun1 <- platges$mun0


## funcions per preparar quickstatements

cometes <- function(text) {
  paste0('"',text,'"')
}

afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- vector
  return(llista)
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr <- afegeix(instr, c(paste0("-",fila$qitem), "P131", ifelse(fila$lat>40.525,"Q5705","Q5720")))
  instr <- afegeix(instr, c(fila$qitem, "P131", fila$mun0))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}



#Fins a Cadaqués direcció Nord-Sud
subplatges <- platges[platges$lat>42.27,]
subplatges$par <- subplatges$lat
subplatges <- subplatges[order(subplatges$par, decreasing = TRUE),]
for (i in 1:10) {
  subplatges$mun0 <- ifelse (is.na(subplatges$mun0), c(tail(subplatges$mun0, -i), rep(NA, i)), subplatges$mun0)
  subplatges$mun1 <- ifelse (is.na(subplatges$mun1), c(rep(NA, i), head(subplatges$mun1, -i)), subplatges$mun1)
}

crear <- subplatges[(!subplatges$fet) & 
                      subplatges$mun0==subplatges$mun1 & 
                      !(is.na(subplatges$mun0)) &
                      !(is.na(subplatges$mun1)),]
creartot <- crear


# Cadaqués a l'Escala direcció oblíqua
subplatges <- platges[platges$lat<42.27 & platges$lat>42.13,]
subplatges$par <- subplatges$lat+subplatges$lon
subplatges <- subplatges[order(subplatges$par, decreasing = TRUE),]
for (i in 1:10) {
  subplatges$mun0 <- ifelse (is.na(subplatges$mun0), c(tail(subplatges$mun0, -i), rep(NA, i)), subplatges$mun0)
  subplatges$mun1 <- ifelse (is.na(subplatges$mun1), c(rep(NA, i), head(subplatges$mun1, -i)), subplatges$mun1)
}

crear <- subplatges[(!subplatges$fet) & 
                      subplatges$mun0==subplatges$mun1 & 
                      !(is.na(subplatges$mun0)) &
                      !(is.na(subplatges$mun1)),]
creartot <- rbind(creartot, crear)


# l'Escala a Badalona
subplatges <- platges[platges$lat<42.13 & platges$lat>41.41,]
subplatges$par <- subplatges$lat
subplatges <- subplatges[order(subplatges$par, decreasing = TRUE),]
for (i in 1:10) {
  subplatges$mun0 <- ifelse (is.na(subplatges$mun0), c(tail(subplatges$mun0, -i), rep(NA, i)), subplatges$mun0)
  subplatges$mun1 <- ifelse (is.na(subplatges$mun1), c(rep(NA, i), head(subplatges$mun1, -i)), subplatges$mun1)
}

crear <- subplatges[(!subplatges$fet) & 
                      subplatges$mun0==subplatges$mun1 & 
                      !(is.na(subplatges$mun0)) &
                      !(is.na(subplatges$mun1)),]
creartot <- rbind(creartot, crear)

# El Prat a l'Ampolla en direcció oblíqua
subplatges <- platges[platges$lat<41.3 & platges$lat>40.78,]
subplatges$par <- subplatges$lat+subplatges$lon
subplatges <- subplatges[order(subplatges$par, decreasing = TRUE),]
for (i in 1:10) {
  subplatges$mun0 <- ifelse (is.na(subplatges$mun0), c(tail(subplatges$mun0, -i), rep(NA, i)), subplatges$mun0)
  subplatges$mun1 <- ifelse (is.na(subplatges$mun1), c(rep(NA, i), head(subplatges$mun1, -i)), subplatges$mun1)
}

crear <- subplatges[(!subplatges$fet) & 
                      subplatges$mun0==subplatges$mun1 & 
                      !(is.na(subplatges$mun0)) &
                      !(is.na(subplatges$mun1)),]
creartot <- rbind(creartot, crear)

# Deltebre en avall (inclòs País Valencià, excloses les Illes)
subplatges <- platges[platges$lat<40.78 & platges$lon < 1,]
subplatges$par <- subplatges$lat
subplatges <- subplatges[order(subplatges$par, decreasing = TRUE),]
for (i in 1:10) {
  subplatges$mun0 <- ifelse (is.na(subplatges$mun0), c(tail(subplatges$mun0, -i), rep(NA, i)), subplatges$mun0)
  subplatges$mun1 <- ifelse (is.na(subplatges$mun1), c(rep(NA, i), head(subplatges$mun1, -i)), subplatges$mun1)
}

crear <- subplatges[(!subplatges$fet) & 
                      subplatges$mun0==subplatges$mun1 & 
                      !(is.na(subplatges$mun0)) &
                      !(is.na(subplatges$mun1)),]
creartot <- rbind(creartot, crear)

#casos problemàtics
creartot <- creartot[!creartot$qitem %in% c(
  "Q216884", "Q456084", "Q28121159","Q28121160", 
  "Q23995200", "Q23987652", "Q918717", "Q1440916", "Q24018903"), ]

crear <- creartot
instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")),
    file="~/DADES/pere/varis/instruccions.txt")
