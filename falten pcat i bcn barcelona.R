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

# versió més restringida
consulta <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord WHERE {   
  {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131+ wd:Q1492.
  ?item wdt:P625 ?coord.
  }
  ?item wdt:P31 [].
  MINUS { ?item wdt:P31 wd:Q1190554. }
  MINUS { ?item wdt:P31 wd:Q1076486. }
  MINUS { ?item wdt:P31 wd:Q41253. }
  MINUS { ?item wdt:P31 wd:Q11315. }
  MINUS { ?item wdt:P31 wd:Q43325366.}
  MINUS { ?item wdt:P31/wdt:P279* wd:Q7075. }
  MINUS { ?item wdt:P31/wdt:P279* wd:Q5341295.
         MINUS {?item wdt:P31/wdt:P279* wd:Q811979}
        }
  MINUS { ?item wdt:P31/wdt:P279* wd:Q1002697. }
  MINUS { ?item wdt:P31 wd:Q1329623. }
  MINUS { ?item wdt:P31 wd:Q1060829. }
  MINUS { ?item wdt:P31 wd:Q1195942. }
  MINUS { ?item wdt:P31 wd:Q150139}
  MINUS { ?item wdt:P31 wd:Q61696039}
  MINUS { ?item wdt:P31 wd:Q113636120}
  MINUS { ?item wdt:P31 wd:Q200764}
  MINUS { ?item wdt:P31 wd:Q213441.
           MINUS {?item wdt:P1435 [].}
        }
  MINUS { ?item wdt:P31 wd:Q48204. }
   MINUS { ?item wdt:P31 wd:Q372690. }
  MINUS { ?item wdt:P31 wd:Q16917.
           MINUS {?item wdt:P1435 [].}
        }
  MINUS { ?item wdt:P31 wd:Q27686. }
  MINUS { ?item wdt:P31 wd:Q1254933.
           MINUS {?item wdt:P1435 [].}
        } 
  MINUS { ?item wdt:P31 wd:Q4287745. }
  MINUS { ?item wdt:P31 wd:Q11707. }
  MINUS { ?item wdt:P31 wd:Q163740. }
  MINUS { ?item wdt:P31 wd:Q1228895. }
  MINUS { ?item wdt:P31 wd:Q847017. }
  MINUS { ?item wdt:P31 wd:Q157031. }
  MINUS { ?item wdt:P31 wd:Q43229. }
  MINUS { ?item wdt:P31 wd:Q200538}
  OPTIONAL {?item wdt:P11557 ?idbcn}
  OPTIONAL {?item wdt:P12802 ?idpcat}
  SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
  }'

# versió menys restringida
consulta <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:Q1492.
  ?item wdt:P625 ?coord.
  MINUS {?item wdt:P625 [].
         MINUS {?item wdt:P31/wdt:P279* wd:Q41176.}
         MINUS {?item wdt:P1435 []}
        }
  OPTIONAL {?item wdt:P11557 ?idbcn}
  OPTIONAL {?item wdt:P12802 ?idpcat}
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
#save(dadeswd, file="~/varis/dadeswdbcn.RData")
#save(dadeswd, file="~/varis/importar_fitxes_grafic/dadeswdbcn.RData") #actualitzar app

## dades ajuntament
load("~/varis/dades_cataleg_bcn.RData")

dfbcn <- data.frame(nom=sapply(dadesbcn, function(x) x$nom),
                    idbcn=sapply(dadesbcn, function(x) x$id),
                    lon=sapply(dadesbcn, function(x) x$coord["longitude"]),
                    lat=sapply(dadesbcn, function(x) x$coord["latitude"]))

# dades pcat
load("~/varis/pcat.RData")

va <- sapply(bdpcat, function(x) x$va)
bdpcatnet <- bdpcat[va]
filaadf <- function(fila) {data.frame(nom=fila$nom[1],
                                      municipi=fila$municipi[1],
                                      coords=fila$coords[1],
                                      url=fila$url[1])}
dfpcat <- lapply(bdpcatnet, filaadf)
dfpcat <- do.call(rbind, dfpcat)

coords <- strsplit(dfpcat$coords, "; ")
coords <- as.data.frame(do.call(rbind, coords))
names(coords) <- c("lat", "lon")
dfpcat <- cbind(dfpcat, coords)
dfpcat$lat <- as.numeric(dfpcat$lat)
dfpcat$lon <- as.numeric(dfpcat$lon)
dfpcat$idpcat <- gsub("https://www.poblesdecatalunya.cat/element.php?e=","", 
                      dfpcat$url, fixed=TRUE)

# Anem separant: elements de Wikidata
table(is.na(dadeswd$idbcn), is.na(dadeswd$idpcat))
dadeswdverm <- dadeswd[is.na(dadeswd$idbcn) & is.na(dadeswd$idpcat),]
dadeswdblau <- dadeswd[!is.na(dadeswd$idbcn) & is.na(dadeswd$idpcat),]
dadeswdverd <- dadeswd[is.na(dadeswd$idbcn) & !is.na(dadeswd$idpcat),]
dadeswdgris <- dadeswd[!is.na(dadeswd$idbcn) & !is.na(dadeswd$idpcat),]

# Elements de bd
bcnblau <- dfbcn[!dfbcn$idbcn %in% dadeswd$idbcn,]
bcnblau <- bcnblau[!grepl("^(Entorn|Conjunt)",bcnblau$nom),]
pcatverd <- dfpcat[!dfpcat$idpcat %in% dadeswd$idpcat & dfpcat$municipi=="Barcelona",]

library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addCircleMarkers(m, lng=dadeswdverm$lon, lat=dadeswdverm$lat, popup=paste(dadeswdverm$item,dadeswdverm$itemLabel), color = "red", radius=5)
m <- addCircleMarkers(m, lng=dadeswdverd$lon, lat=dadeswdverd$lat, popup=paste(dadeswdverd$item,dadeswdverd$itemLabel), color = "green", radius=5)
m <- addCircleMarkers(m, lng=dadeswdblau$lon, lat=dadeswdblau$lat, popup=paste(dadeswdblau$item,dadeswdblau$itemLabel), color = "blue", radius=5)
m <- addCircleMarkers(m, lng=dadeswdgris$lon, lat=dadeswdgris$lat, popup=paste(dadeswdgris$item,dadeswdgris$itemLabel), color = "brown", radius=5)
m <- addCircleMarkers(m, lng=bcnblau$lon, lat=bcnblau$lat, popup=(paste(bcnblau$idbcn, bcnblau$nom)), color="blue")
m <- addCircleMarkers(m, lng=pcatverd$lon, lat=pcatverd$lat, popup=(paste(pcatverd$idpcat, pcatverd$nom)), color="green")
m
