#arreglar idioma etiqueta adreces

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

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}

cometes <- function(text) {
  paste0('"',text,'"')
}

adrcat <- getsparql("https://query.wikidata.org/sparql?query=SELECT%20%3Fitem%20%3FitemLabel%20%3Fadr%20%3Fcodi%20%3Fllengua%20%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP131*%20wd%3AQ5705.%0A%20%20%3Fitem%20wdt%3AP6375%20%3Fadr.%0A%20%20OPTIONAL%20%7B%3Fitem%20wdt%3AP1600%20%3Fcodi%7D%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Coc%2Cen%2Ces%2Cfr%22.%20%7D%0A%20%20BIND%20(LANG(%3Fadr)%20AS%20%3Fllengua)%0A%7D")
adraran <- getsparql("https://query.wikidata.org/sparql?query=%23%20Adreces%20de%20llocs%20de%20l%27Aran%2C%20amb%20l%27etiqueta%20de%20llengua%20de%20l%27adre%C3%A7a%20i%20el%20codi%20IPAC%20si%20n%27hi%20ha%0ASELECT%20%3Fitem%20%3FitemLabel%20%3Fadr%20%3Fcodi%20%3Fllengua%20%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP131*%20wd%3AQ12602.%0A%20%20%3Fitem%20wdt%3AP6375%20%3Fadr.%0A%20%20OPTIONAL%20%7B%3Fitem%20wdt%3AP1600%20%3Fcodi%7D%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Coc%2Cen%2Ces%2Cfr%22.%20%7D%0A%20%20BIND%20(LANG(%3Fadr)%20AS%20%3Fllengua).%0A%7D")

summary(adrcat)
table(adrcat$llengua)
adrcat[!adrcat$llengua %in% c("ca","es","oc"),]
table(grepl("[Cc]alle|[Pp]laza", adrcat$adr))
adrcat[grep("[Cc]alle|[Pp]laza| [Ll]as |[Cc]amino|[Pp]aseo|[Aa]lameda|[Aa]venida|[Ee]scalera|[Mm]uelle|Aragón", adrcat$adr),]
View(adrcat[is.na(adrcat$codi) & adrcat$llengua!="ca",])

crear <- adrcat[adrcat$llengua=="es",]
crear <- crear[!crear$item %in% adraran$item,]
crear <- crear[-grep("[Cc]alle|[Pp]laza| [Ll]as |[Cc]amino|[Pp]aseo|[Aa]lameda|[Aa]venida|[Ee]scalera|[Mm]uelle|Aragón", adrcat$adr),]
crear$item <- itemq(crear$item) 

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr[[1]] <- c(paste0("-",fila$item), "P6375", paste0("es:",cometes(fila$adr)))
  instr[[2]] <- c(fila$item, "P6375", paste0("ca:",cometes(fila$adr)))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(101:500, function(i) {quick(crear[i,])})) #1:nrow(crear)
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
