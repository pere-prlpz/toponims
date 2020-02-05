# afegir i pujar edificis
# versió2:
# - No carrega nomenclàtor (tampoc el feia servir)
# - Canvi de Mare de Déu a Santa Maria

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

classeswd <- getsparql("https://query.wikidata.org/sparql?query=SELECT%20DISTINCT%20%3Ftipus%20%3Fnomtipus%0AWHERE%20%7B%0A%3Ftipus%20wdt%3AP279*%20wd%3AQ811979.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Cfr%2Ceu%2Cde%2Csv%2Cceb%22%20.%0A%3Ftipus%20rdfs%3Alabel%20%3Fnomtipus%0A%7D%0A%7D%0A")
municipiswd <- getsparql("https://query.wikidata.org/sparql?query=%23Municipis%20de%20Catalunya%0ASELECT%20%3Fmun%20%3Fmuname%20%3Fcom%20%3Fcomname%0AWHERE%20%7B%0A%20%20VALUES%20%3Fdivisio%20%7Bwd%3AQ19920968%20wd%3AQ937876%7D%0A%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843.%0A%20%20%3Fmun%20wdt%3AP131%20%3Fcom.%0A%20%20%3Fcom%20wdt%3AP31%20%3Fdivisio.%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fmuname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%22%20.%0A%3Fcom%20rdfs%3Alabel%20%3Fcomname%0A%7D%0A%7D%0A%0A")
llocswd <- getsparql("https://query.wikidata.org/sparql?query=%23Tot%20de%20Catalunya%0ASELECT%20%3Fitem%20%3Fname%20%3Ftipus%20%3Fcoord%20%3Flat%20%3Flon%20%3Fmun%20%0AWHERE%20%7B%0A%20%20%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP131*%20wd%3AQ5705.%0A%20%20%3Fitem%20wdt%3AP31%20%3Ftipus.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0A%20%20OPTIONAL%20%7B%0A%20%20%20%20%3Fitem%20wdt%3AP131*%20%3Fmun.%0A%20%20%20%20%3Fmun%20wdt%3AP31%20wd%3AQ33146843%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%7D%0A%0A")
# el propers no els agafem perquè necessitem els municipis
#properswd <- getsparql("https://query.wikidata.org/sparql?query=%23Items%20propers%0ASELECT%20%3Fitem%20%3Fname%20%3Ftipus%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%3Fitem%20wdt%3AP17%20wd%3AQ29.%0A%20%20%3Fitem%20wdt%3AP31%20%3Ftipus.%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon.%0A%20%20FILTER%20(%3Flat%20%3E%2040.3)%0A%20%20FILTER%20(%3Flon%20%3E%200.1)%20%20%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%7D%0A%0A")

#save(classeswd, municipiswd, llocswd, properswd, file="~/DADES/Pere/varis/llocswd.RData")

#properswd <- properswd[!properswd$item %in% llocswd$item,]
#properswd$mun <- NA
#llocswd <- rbind(llocswd, properswd)

edificiswd <- llocswd[llocswd$tipus %in% classeswd$tipus,]

#Conversió municipis WD (noms IEC actuals) a nomenclàtor (noms oficials antics) 
#PER FER: Canviar a l'inrevés per treballar amb els correctes
actnom <- function(name) {
  name[name=="La Granada"] <- "la Granada"
  name[name=="La Sénia"] <- "la Sénia"
  name[name=="Calonge i Sant Antoni"] <- "Calonge"
  name[name=="el Figueró i Montmany"] <- "Figaró-Montmany"
  name[name=="Roda de Berà"] <- "Roda de Barà"
  name[name=="Navars"] <- "Navàs"
  name[name=="l'Esquirol"] <- "Santa Maria de Corcó"
  name[name=="Brunyola i Sant Martí Sapresa"] <- "Brunyola"
  return(name)  
}

municipiswd$muname <- actnom(municipiswd$muname)

#arreglo errors
municipiswd <- municipiswd[!(municipiswd$muname=="Vilademuls"& municipiswd$comname=="Alt Empordà"),]


edificiswd <- merge(edificiswd, classeswd, by="tipus")
edificiswd <- merge(edificiswd, municipiswd, by="mun", 
                    all.x = TRUE, all.y=FALSE)

ttipus <- table(edificiswd$nomtipus)
head(sort(ttipus, decreasing = TRUE), 20)
table(duplicated(edificiswd$item))


# regularització del nom
arreglanom <- function(nom) {
  nom <- gsub(" *", "", nom, fixed = TRUE)
  trossos <- unlist(strsplit(nom, ", "))
  n <- length(trossos)
  nom <- ifelse(n==1,
                nom,
                paste(c(trossos[n], trossos[1:(n-1)]), collapse = " ")
  )
  nom <- gsub("' ","'", nom)
  return (nom)
}


# nom dels nuclis existents
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les) ","",nom)
  gsub("^l'","",nom)
}

edificiswd$nomnet <- sapply(edificiswd$name, treupar)

# noms per enganxar
edificiswd$nomrel <- treuart(tolower(edificiswd$nomnet))
edificiswd$munrel <- tolower(actnom(edificiswd$muname))

itemq <- function(item) {gsub("http://www.wikidata.org/entity/", "",
                              item, fixed=TRUE)}
edificiswd$qitem <- itemq(edificiswd$item)

santswd <- edificiswd[grepl("([Ss]ant.? .*|[Vv]erge de|[Mm]are de [Dd]éu)\\b", edificiswd$nomnet),]
santswd <- santswd[!grepl("^([Cc]arrer|[Pp]laça|[Pp]asseig|[Hh]abitatge|[Ee]scola) ", santswd$nomnet),]
santswd$sant <- tolower(santswd$nomnet)
santswd$sant <- gsub("verge de", "santa Maria de",santswd$sant)
santswd$sant <- gsub("mare de déu", "santa maria",santswd$sant)
santswd$sant <- gsub("^.*(sant? .+?\\>)", "\\1", santswd$sant)
santswd$sant <- gsub("^(sant.? [^ ]*)( .+)+$", "\\1", santswd$sant)

table(duplicated(santswd$sant))
head(sort(table(santswd$sant), decreasing = TRUE), 50)

autounits <- merge(santswd, santswd, by=c("mun","sant"))
table(autounits$item.x==autounits$item.y)
autounits <- autounits[!autounits$item.x==autounits$item.y,]

dobles <- santswd[santswd$item %in% autounits$item.x,]
dobles <- dobles[order(dobles$muname, dobles$sant),]
dobles <- dobles[!duplicated(dobles[, c("item", "sant", "mun"),]),]
dobles$recent <- as.numeric(gsub("Q","",dobles$qitem))>72.5e6
dobles$negreta <- ifelse(dobles$recent, "'''", "")
library(tools)
dobles$sant <- toTitleCase(dobles$sant)
dobles$pqitem <- gsub("^Q(.+)$", "{{Q|\\1}}", dobles$qitem)
dobles <- dobles[order(dobles$muname,
                       dobles$sant,
                       as.numeric(as.character(gsub("Q", "",dobles$qitem)))),]

munsant <- unique(dobles[, c("mun","muname","sant")])
mun <- unique(munsant[,c("mun","muname")])

codiedif <- function(fila) {
  paste0("** ",fila$pqitem," ",
         fila$negreta,
         fila$nomnet,
         fila$negreta)
}

codisant <- function(filasant) {
  llista0 <- list(
    paste0("* ",filasant$sant)
  )
  llista <- lapply(which(
    dobles$muname==filasant$muname & dobles$sant==filasant$sant),
    function(i) {codiedif(dobles[i,])})
  return(c(llista0, llista))
}

codimun <- function(nommun) {
  llista0 <- list(
    paste0("== ",nommun, " ==")
  )
  llista <- lapply(which(
    munsant$muname==nommun),
    function(i) {codisant(munsant[i,])})
  return(c(llista0, llista))
}

codi <- lapply(mun$muname,
               codimun)

write.table(unlist(codi[1:20]), quote=FALSE, row.names = FALSE)
write.table(unlist(codi), quote=FALSE, 
            row.names = FALSE,
            col.names = FALSE,
            file="~/DADES/pere/varis/wikitext.txt")

# proves
codiedif(dobles[25,])
codisant(munsant[6,])
codimun(mun$muname[134])
# proves
table(grepl("(Sant? .*)\\b",edificiswd$nomnet))
head(gsub("^.*(Sant? .*)\\>", "\\1", edificiswd$nomnet), 30)
gsub("^.*(Sant? .+?\\>)", "\\1", c("Sant Pere", "Cala de Sant Pere Regalat", "Església de Sant Pere"))
gsub("\\b", "ESPAI", "Hi havia una vegada")
gsub("\\>", "ESPAI", "Hi havia una vegada")
head(gsub("^(Sant.?.? .*) .+$", "\\1", santswd$sant))
head(gsub("^(Sant.? .*)( .+)+$", "\\1", santswd$sant))
grepl("$(Sant.? .*)( .+)$", c("Sant Pere", "Sant Pere Regalat"))
head(gsub("^(Sant.? [^ ]*)( .+)+$", "\\1", santswd$sant))
table(grepl("Mare de Déu", edificiswd$nomnet))
table(grepl("Mare-de-déu", edificiswd$nomnet))
table(grepl("Verge", edificiswd$nomnet))
edificiswd$nomnet[grepl("Verge de", edificiswd$nomnet)]
gsub("Verge de", "Santa Maria de",edificiswd$nomnet[grepl("Verge de", edificiswd$nomnet)])
table(grepl("^(Carrer|Plaça|Passeig) ", santswd$nomnet))
santswd$nomnet[grepl("^(Carrer|Plaça|Passeig) ", santswd$nomnet)]
gsub("^Q(.+)$", "{{Q|\\1}}", c("Q1", "Q5489175"))
