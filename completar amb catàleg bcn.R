# completar dades amb catàleg Barcelona

## funcions generals
library(httr)
library(rjson)

cometes <- function(text) {
  paste0('"',text,'"')
}

afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}


## dades de Wikidata
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

consulta <- 'SELECT ?item ?itemLabel ?id ?coord ?prot ?protLabel ?estil ?estilLabel WHERE {
  ?item wdt:P11557 ?id.
  OPTIONAL {?item wdt:P625 ?coord}
  OPTIONAL {?item wdt:P1435 ?prot}
  OPTIONAL {?item wdt:P149 ?estil}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],ca" } .
}'

dadeswd <- getquery(consulta)

## dades ajuntament
load("~/varis/dades_cataleg_bcn.RData")

## coordenades
crear <- dadeswd[is.na(dadeswd$coord), c("item", "itemLabel", "id")]
crear <- crear[!duplicated(crear),]
coordbcn <- sapply(dadesbcn, function(x) {c(nom=x$nom, id=x$id, x$coord[1], x$coord[2])})
coordbcn <- as.data.frame(t(coordbcn))
coordbcn$url <- paste0("https://ajuntament.barcelona.cat/informaciourbanistica/cerca/ca/fitxa/",
                       coordbcn$id,
                       "/--/--/cp/")
crear[!crear$id %in% coordbcn$id,] #els que no podem omplir
crear <- merge(crear, coordbcn)

# instruccions pel quickstatement ajuntament barcelona
quick <- function(dades, url=dades$url, qid=dades$item, altres=c(""), descr=TRUE) {
  curl <- cometes(url)
  instr <- list()
  instr <- afegeix(instr, c(qid, "P625", 
                            paste0("@", paste0(dades[c("latitude","longitude")], 
                                               collapse="/")),
                            "S248", "Q116698266", "S854", curl))  
  return(instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla

## proteccions
table(dadeswd$prot)
table(dadeswd$protLabel)
dput(unique(dadeswd$protLabel))

protaj <- c("bé cultural d'interès local", 
            "bé cultural d'interès nacional", 
            "bé amb protecció urbanística", 
            "Bé amb elements d'interès", 
            "Bé d'interès documental",
            "Establiment d'interès", 
            "Element d'interès paisatgístic")

dadeswdprot <- dadeswd[,c("item","itemLabel","id","prot","protLabel")]
dadeswdprot <- dadeswdprot[!duplicated(dadeswdprot),]
#dadesprotNA <- dadeswdprot[is.na(dadeswdprot$prot),]
dadesprotaj <- dadeswdprot[dadeswdprot$protLabel %in% protaj,]
crear <- dadeswdprot[!dadeswdprot$id %in% dadesprotaj$id | is.na(dadeswdprot$prot),]

protbcn <- sapply(dadesbcn, function(x) {c(nom=x$nom, id=x$id, qprot=x$qprot)})
protbcn <- as.data.frame(t(protbcn))
protbcn$url <- paste0("https://ajuntament.barcelona.cat/informaciourbanistica/cerca/ca/fitxa/",
                      protbcn$id,
                       "/--/--/cp/")

crear <- merge(crear, protbcn)

quick <- function(dades, url=dades$url, qid=dades$item, altres=c(""), descr=TRUE) {
  curl <- cometes(url)
  instr <- list()
  instr <- afegeix(instr, c(qid, "P1435", dades$qprot, 
                            "S248", "Q116698266", "S854", curl))  
  return(instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla

## estil
table(dadeswd$estilLabel, useNA = "always")
crear <- dadeswd[is.na(dadeswd$estil), c("item", "itemLabel", "id", "estil", "estilLabel")]
crear <- crear[!duplicated(crear),]

dicestil <- c("modernisme"="Q1122677",
              "modernista"="Q1122677",
              "noucentisme"="Q1580216",
              "noucentista"="Q1580216",
              "historicisme"="Q51879601",
              "historicista"="Q51879601",
              "romànic"="Q46261",
              "gòtic"="Q176483",
              "gòtic tardà"="Q10924220",
              "renaixentista"="Q236122",
              "renaixement"="Q236122",
              "barroc"="Q840829",
              "neoclàssic"="Q54111",
              "neoclassicisme"="Q54111",
              "neogòtic"="Q186363",
              "historicisme neogòtic"="Q186363",
              "neomudèjar"="Q614624",
              "neoromànic"="Q744373",
              "arquitectura popular"="Q930314",
              "popular"="Q930314",
              "obra popular"="Q930314",
              "eclecticisme"="Q2479493",
              "eclèctic"="Q2479493",
              "racionalisme"="Q2535546",
              "racionalista"="Q2535546",
              "arquitectura del ferro"="Q900557")

estilbcn <- sapply(dadesbcn, function(x) {c(nom=x$nom, id=x$id, estil=x$estil[1])})
estilbcn <- as.data.frame(t(estilbcn))
estilbcn$url <- paste0("https://ajuntament.barcelona.cat/informaciourbanistica/cerca/ca/fitxa/",
                       estilbcn$id,
                      "/--/--/cp/")
estilbcn$qestil <- dicestil[tolower(estilbcn$estil)]
estilbcn <- estilbcn[!is.na(estilbcn$qestil),]

crear <- merge(crear, estilbcn, by="id")

#lapply(dadesbcn, function(x) {x$estil})

quick <- function(dades, url=dades$url, qid=dades$item, altres=c(""), descr=TRUE) {
  curl <- cometes(url)
  instr <- list()
  instr <- afegeix(instr, c(qid, "P149", dades$qestil, 
                            "S248", "Q116698266", "S854", curl))  
  return(instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
