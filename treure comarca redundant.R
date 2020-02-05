#interpolar treure comarca dels edificis que ja tenen municipi

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

#edificis amb municipis i comarques
#muncom <- getsparql("https://query.wikidata.org/sparql?query=%23Edificis%20P131*%20Catalunya%20menys%20P131*%20municipis%20de%20Catalunya%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcom%20%3Fcomname%20%3Fmun%20%3Fmunname%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ41176.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fcom.%0A%20%20%3Fcom%20wdt%3AP31%20wd%3AQ937876.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fcom%20rdfs%3Alabel%20%3Fcomname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fmunname%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
#edificis amb muncipi i província
#muncom <- getsparql("https://query.wikidata.org/sparql?query=%23Edificis%20P131*%20Catalunya%20menys%20P131*%20municipis%20de%20Catalunya%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcom%20%3Fcomname%20%3Fmun%20%3Fmunname%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ41176.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fcom.%0A%20%20%3Fcom%20wdt%3AP31%20wd%3AQ162620.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fcom%20rdfs%3Alabel%20%3Fcomname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fmunname%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
#muntanyes
muncom <- getsparql("https://query.wikidata.org/sparql?query=%23Edificis%20P131*%20Catalunya%20menys%20P131*%20municipis%20de%20Catalunya%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcom%20%3Fcomname%20%3Fmun%20%3Fmunname%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ8502.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fcom.%0A%20%20%3Fcom%20wdt%3AP31%20wd%3AQ937876.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fcom%20rdfs%3Alabel%20%3Fcomname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fmunname%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
#muncom2 <- getsparql("https://query.wikidata.org/sparql?query=%23Edificis%20P131*%20Catalunya%20menys%20P131*%20municipis%20de%20Catalunya%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcom%20%3Fcomname%20%3Fmun%20%3Fmunname%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ8502.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fcom.%0A%20%20%3Fcom%20wdt%3AP31%20wd%3AQ162620.%0A%20%20%3Fitem%20wdt%3AP131%20%3Fmun.%0A%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fcom%20rdfs%3Alabel%20%3Fcomname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fmunname%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3AMap")
#muncom <- rbind(muncom, muncom2)

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}

muncom$qitem <- itemq(muncom$item)
muncom$qcom <- itemq(muncom$com)


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
  instr <- afegeix(instr, c(paste0("-",fila$qitem), "P131", fila$qcom))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

crear <- muncom
instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
instruccions <- unique(instruccions)

cat(paste(instruccions, collapse="\n")) #pantalla
#cat(enc2utf8(paste(instruccions, collapse="\n")),
#    file="~/DADES/pere/varis/instruccions.txt")
