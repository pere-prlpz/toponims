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

## carregar dades Cat

partcat <- c("Q18265", "Q1030024", "Q1113384", "Q1113390", 
             #"Q1462520",#ponent
             "Q12732","Q12726","Q12728","Q12733","Q12727","Q12729",
             "Q1849804", "Q579384",
             "Q12600", "Q13948", "Q14303", "Q15348", "Q15351","Q15352") 

# versió més restringida (llista de topònims)
consulta1 <- paste0('SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord  ?inst ?instLabel
WHERE {   
  {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:',partcat,'.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
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
  }')

# versió menys restringida (protegits i edificis)
consulta2 <- paste0('SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:',partcat,'.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  MINUS {?item wdt:P625 [].
         MINUS {?item wdt:P31/wdt:P279* wd:Q41176.}
         MINUS {?item wdt:P1435 []}
        }
  OPTIONAL {?item wdt:P11557 ?idbcn}
  OPTIONAL {?item wdt:P12802 ?idpcat}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}')

# elements protegits
consulta3 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P131* wd:Q5705.
  ?item wdt:P625 ?coord.
  ?item wdt:P1435 [].
  ?item wdt:P31 ?inst.
  OPTIONAL {?item wdt:P11557 ?idbcn}
  OPTIONAL {?item wdt:P12802 ?idpcat}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'

# elements del catàleg de Barcelona
consulta4 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P11557 ?idbcn.
  ?item wdt:P17 wd:Q29.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  OPTIONAL {?item wdt:P12802 ?idpcat}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'


# elements de poblesdecatalunya.cat
consulta5 <- 'SELECT DISTINCT ?item ?itemLabel ?idbcn ?idpcat ?coord ?inst ?instLabel
WHERE {
  ?item wdt:P17 wd:Q29.
  ?item wdt:P625 ?coord.
  ?item wdt:P31 ?inst.
  ?item wdt:P12802 ?idpcat.
  OPTIONAL {  ?item wdt:P11557 ?idbcn.}
SERVICE wikibase:label {bd:serviceParam wikibase:language "ca" .}
}'

print(Sys.time())
dades0 <- lapply(consulta1, getquery)
dadeswd1 <- do.call(rbind, dades0)
print(Sys.time())
dades0 <- lapply(consulta2, getquery)
dadeswd2 <- do.call(rbind, dades0)
print(Sys.time())
dadeswd3 <- getquery(consulta3)
print(Sys.time())
dadeswd4 <- getquery(consulta4)
dadeswd5 <- getquery(consulta5)
print(Sys.time())

dadeswd <- rbind(dadeswd1, dadeswd2, dadeswd3, dadeswd4, dadeswd5)
dadeswd <- dadeswd[!duplicated(dadeswd),]

table(duplicated(dadeswd$item))
head(dadeswd[duplicated(dadeswd$item),])

# endreçar

treucoord <- function(x) {
  coord <- gsub("Point\\(","",x$coord)
  coord <- gsub(")","",coord)
  coord <- as.data.frame(strsplit(coord, " "))
  coord <- as.data.frame(t(coord))
  coord <- as.data.frame(lapply(coord, as.numeric))
  names(coord) <- c("lon","lat")
  x <- cbind(x, coord)
  return(x)
}

actdadeswdcat <- Sys.time()
actdadesbcn <-Sys.time()
actdadespcat <- Sys.time()
dadeswd <- treucoord(dadeswd)
save(dadeswd, actdadeswdcat, actdadespcat, actdadesbcn, file="~/varis/dadeswdcat.RData")
save(dadeswd, actdadeswdcat, actdadespcat, actdadesbcn,
     file="~/varis/importar_fitxes_mapa/dadeswdcat.RData") #actualitzar app
