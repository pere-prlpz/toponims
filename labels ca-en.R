# posar etiquetes en anglès a partir de les etiquetes en català

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

items1 <- getsparql("https://query.wikidata.org/sparql?query=%23etiquetes%20items%20de%20Catalunya%20que%20tenen%20coordenades%0ASELECT%20DISTINCT%20%3Fitem%20%3Fnameca%20%3Fnameen%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP131*%20wd%3AQ5705.%0A%20%20%3Fitem%20wdt%3AP625%20%5B%5D.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fnameca%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%0A%3Fitem%20rdfs%3Alabel%20%3Fnameen%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3ATable")
items2 <- getsparql("https://query.wikidata.org/sparql?query=%23etiquetes%20items%20de%20Catalunya%20que%20tenen%20protecci%C3%B3%0ASELECT%20DISTINCT%20%3Fitem%20%3Fnameca%20%3Fnameen%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP131*%20wd%3AQ5705.%0A%20%20%3Fitem%20wdt%3AP1435%20%5B%5D.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fnameca%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%0A%3Fitem%20rdfs%3Alabel%20%3Fnameen%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3ATable")
items3 <- getsparql("https://query.wikidata.org/sparql?query=%23etiquetes%20items%20a%20IPAC%0ASELECT%20DISTINCT%20%3Fitem%20%3Fnameca%20%3Fnameen%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP1600%20%5B%5D.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fnameca%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%0A%3Fitem%20rdfs%3Alabel%20%3Fnameen%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A%23defaultView%3ATable")
items <- rbind(items1, items2, items3)
items <- unique(items)

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}
tot <- items
tot$qitem <- itemq(tot$item)

table(tot$qitem==tot$nameen)
table(tot$qitem==tot$nameca)
table(tot$qitem==tot$nameen & tot$qitem!=tot$nameca)
tot[grep("^Río ", tot$nameca),]
tot[grep("^R[ií]o ", tot$nameca),]
table(grepl("^([Hh]abitatge|[Cc]asa|[Ee]difici) a", tot$nameca))
table(grepl("^([Hh]abitatge|[Cc]asa|[Ee]difici) al carrer", tot$nameca))

crear <- tot[tot$qitem==tot$nameen & tot$qitem!=tot$nameca,]

head(crear)

treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

crear$nopar <- sapply(crear$nameca, treupar)
crear$nomnou <- crear$nopar

crear$nomnou <- 
  gsub("^([Hh]abitatge|[Cc]asa) (a|de)(l | la | l'|ls | les )([Cc]arrer|[Pp]laça|[Pp]asseig|muralla|riba|[Rr]iera|avinguda|[Cc]arretera|travess(i|er)a|[Rr]ambla|baixada|Força|barri|placeta|esplanada|Gran Via|[Vv]ia|riba|[Cc]amí|[Rr]onda)", 
     "House in \\4", crear$nomnou)
crear$nomnou <- 
  gsub("^([Ee]difici|[Ee]difici d'habitatges) (a|de)(l | la | l'|ls | les )([Cc]arrer|[Pp]laça|[Pp]asseig|muralla|riba|[Rr]iera|avinguda|[Cc]arretera|travess(i|er)a|[Rr]ambla|baixada|Força|barri|placeta|esplanada|Gran Via|[Vv]ia|riba|[Cc]amí|[Rr]onda)", 
                     "Building in \\4", crear$nomnou)
crear$nomnou <- 
  gsub("^([Ee]difici|[Ee]difici d'habitatges) (c.|pg.|ptge.|pl.|[Cc]arrer|[Pp]laça|[Pp]asseig|muralla|riba|[Rr]iera|avinguda|[Cc]arretera|travess(i|er)a|[Rr]ambla|baixada|Força|barri|placeta|esplanada|Gran Via|[Vv]ia|riba|[Cc]amí|[Rr]onda)", 
       "Building in \\2", crear$nomnou)
crear$nomnou <- gsub("^Fossa comuna:", 
                     "Mass grave:", crear$nomnou)
crear$nomnou <- gsub("^[Ee]stació d'aforament (de |d')(.*)$", 
                     "\\2 stream gauge", crear$nomnou)
crear$nomnou <- gsub("^[Ee]stació d(els? .*)$", 
                     "\\1 station", crear$nomnou)
crear$nomnou <- gsub("^[Ee]stació d(e |')(.*)$", 
                     "\\2 station", crear$nomnou)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

crear$nomnou <- firstup(crear$nomnou)


cometes <- function(text) {
  paste0('"',text,'"')
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr[[1]] <- c(fila$qitem, "Len", cometes(fila$nomnou))
  if (fila$nameca != fila$nopar) {
    instr[[2]] <- c(fila$qitem, "Aca", cometes(fila$nameca))
    instr[[3]] <- c(fila$qitem, "Lca", cometes(fila$nopar))
  }
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")

