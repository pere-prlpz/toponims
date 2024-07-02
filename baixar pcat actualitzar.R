## Previs

library(httr)
library(rjson)
#library(sf)
#library(oce)
library(stringr)

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

llegeixpcat <- function(url) {
  pag <- readLines(url, encoding="UTF-8")
  tit <- pag[grepl("<title>.*</title>", pag)]
  tit <- gsub("^.*<title>(.*)</title>.*$", "\\1", tit)
  tittrossos <- unlist(strsplit(tit, " - "))
  nom <- tittrossos[1]
  if (grepl("<i>",nom)) {
    titol <- gsub("^.*<i>(.*)</i>.*$","\\1",nom)
    nom <- gsub("</?i>","",nom)
  } else {
    titol <- NA
  }
  mun <- tittrossos[2]
  mun <- trimws(gsub("\\|.*$","",mun))
  tipus <- pag[grepl("<tr><th>Tipus</th><td>", pag)]
  tipus <- gsub("^.*<tr><th>Tipus</th><td>(.*)</td></tr>.*$","\\1", tipus)
  estil <- pag[grepl("<tr><th>Estil</th><td>", pag)]
  estil <- gsub("^.*<tr><th>Estil</th><td>(.*)</td></tr>.*$","\\1", estil)
  estil <- gsub("</td>.*$", "", estil)
  coords <- pag[grepl('<meta name="geo.position"', pag)]
  coords <- gsub('^.*<meta name="geo.position" content="(.*)"><style type="text/css">.*$',"\\1", coords)
  adreca <- pag[grep("Premeu per a veure la situacio de l'element al mapa", pag)+1]
  adreca <- gsub("</td></tr>", "", adreca)
  resultat <- c(list(nom=nom, municipi=mun,  
                     tipus=tipus, estil=estil, coords=coords,
                     adreca=adreca, titol=titol,
                     url=url, va=TRUE))
  return(resultat)
}



llegeixpcati <- function(x) {
  url <- paste0("https://www.poblesdecatalunya.cat/element.php?e=", x)
  tryCatch({
    llegeixpcat(url)
  }, error = function(e) list(url=url, va=FALSE))
}

load("~/varis/pcat.RData")

va <- sapply(bdpcat, function(x) x$va)
va0 <- va
illegir <- which(!va)

Sys.time()
for (i in illegir) {bdpcat[[i]] <- llegeixpcati(i)}
Sys.time()

va1 <- sapply(bdpcat, function(x) x$va)
table(va0,va1)
which(va1 & !va0)

save(bdpcat, file="~/varis/pcat.RData")


