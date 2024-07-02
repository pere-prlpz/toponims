##################
## Carregar de Wikidata
######################

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

consulta <- 'SELECT DISTINCT ?edifici ?edificiLabel ?idbcn ?coord
WHERE {
  ?edifici wdt:P17 wd:Q29.
  ?edifici wdt:P131* wd:Q1492.
  ?edifici wdt:P625 ?coord.
  MINUS {?edifici wdt:P625 [].
         MINUS {?edifici wdt:P31/wdt:P279* wd:Q41176.}
         MINUS {?edifici wdt:P1435 []}
        }
   OPTIONAL {?edifici wdt:P11557 ?idbcn}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'

dadeswd <- getquery(consulta)

class(dadeswd$coord)
head(dadeswd$coord)

treucoord <- function(x) {
  x <- dadeswd
  coord <- gsub("Point\\(","",x$coord)
  coord <- gsub(")","",coord)
  coord <- as.data.frame(strsplit(coord, " "))
  coord <- as.data.frame(t(coord))
  coord <- as.data.frame(lapply(coord, as.numeric))
  names(coord) <- c("lon","lat")
  x <- cbind(x, coord)
  return(x)
}

dadeswd <- treucoord(dadeswd)

## dades ajuntament
load("~/varis/dades_cataleg_bcn.RData")

dfbcn <- data.frame(nom=sapply(dadesbcn, function(x) x$nom),
                    idbcn=sapply(dadesbcn, function(x) x$id),
                    lon=sapply(dadesbcn, function(x) x$coord["longitude"]),
                    lat=sapply(dadesbcn, function(x) x$coord["latitude"]))

## traiem els existents
units <- merge(dadeswd, dfbcn, by="idbcn")

table(dadeswd$idbcn %in% units$idbcn)
leafwd <- dadeswd[!dadeswd$idbcn %in% units$idbcn,]
table(dfbcn$idbcn %in% units$idbcn)
leafbcn <- dfbcn[!dfbcn$idbcn %in% units$idbcn,]
table(grepl("^Entorn",leafbcn$nom))
leafbcn <- leafbcn[!grepl("^(Entorn|Conjunt)",leafbcn$nom),]

## Representar amb Leaflet

# desaparellats de wikidata en vermell, nomenclàtor en blau
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addCircleMarkers(m, lng=leafbcn$lon, lat=leafbcn$lat, popup=(paste(leafbcn$idbcn, leafbcn$nom)))
m <- addCircleMarkers(m, lng=leafwd$lon, lat=leafwd$lat, popup=leafwd$edificiLabel, color = "red", radius=5)
m

## Representar amb map draw
i <- 1:nrow(leafbcn)
puntsmd <- paste0("| title",i,"=",leafbcn$id," ",
                  leafbcn$nom," | coordinates",i,"=",
                  leafbcn$lat,",",leafbcn$lon,
                  collapse="\n")
#cat(puntsmd)                  
codimd <- paste("{{map draw",
                puntsmd,paste("|text= Actualitzat",Sys.time()),"}}", sep="\n")
#cat(codimd)
cat(codimd, file="~/varis/instruccions.txt")
