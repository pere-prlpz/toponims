# prepara dades del catàleg de Barcelona per treure-li feina i pes a l'app.

library(sf)
library(oce)
library(stringr)

# llegir fitxer
#ruta <- "C:\\Users\\Pere\\Downloads\\0703060100_BCN_Patrimoni_Arqui"
ruta <- "~/varis/0703060100_BCN_Patrimoni_Arqui"
forma <- read_sf(file.path(ruta,"0703060100_BCN_Patrimoni_Arqui_POLIGONS_V.shp"))

# diccionaris i dades
diccprot <- c(CPA_01_PL="Q1019352",
              CPA_02_PL="Q11910250",
              CPA_03_PL="Q107542530")
districtes <- 
  structure(list(num = 1:10, 
                 item = c("Q941385", "Q64124", "Q753075", 
                          "Q959944", "Q1765582", "Q852697", "Q1771488", "Q1641049", "Q1650230", 
                          "Q250935"), 
                 label = c("Ciutat Vella", "Eixample", "Sants-Montjuïc", 
                           "Districte de les Corts", "Sarrià - Sant Gervasi", "Gràcia", 
                           "Horta-Guinardó", "Nou Barris", "Sant Andreu", "Sant Martí"
                 )), 
            row.names = 1:10, class = "data.frame")

mintitol <- function(nom) {
  nom <- str_to_title(nom)
  nom <- gsub(" D'", " d'", nom)
  nom <- gsub(" De ", " de ", nom)
  nom <- gsub(" Del ", " del ", nom)
  nom <- gsub(" Dels ", " dels ", nom)
  nom <- gsub(" El "," el ", nom)
  nom <- gsub(" Els "," els ", nom)
  nom <- gsub(" La "," la ", nom)
  nom <- gsub(" Les "," les ", nom)
  nom <- gsub(" A "," a ", nom)
  nom <- gsub(" I "," i ", nom)
}

llegeixbcn <- function(id) {
  print(paste("id:", id))
  registre <- forma[forma$IDENTIFICA==id,]
  nom <- mintitol(registre$DENOMIN)
  totcoord <- do.call(rbind, lapply(registre$geometry, as.matrix))
  coordutm <- apply(totcoord, 2, median)
  coord <- unlist(utm2lonlat(coordutm[1],coordutm[2], zone=31))
  qprot <- diccprot[registre$NIVELL]
  if (length(qprot>1)) {
    qprot <- qprot[!is.na(qprot)]
    qprot <- unique(qprot)
  }
  url <- paste0("https://ajuntament.barcelona.cat/informaciourbanistica/cerca/ca/fitxa/",
                id,
                "/--/--/cp/")
  districte <- districtes$item[as.numeric(registre$DISTRICTE)]
  estil <- registre$ESTIL
  resultat <- list(nom=nom[1], id=id, coord=coord, qprot=qprot[1], 
                   url=url, districte=districte[1],
                   qtipus="Q41176", estil=estil)
  return(resultat)
}

sort(table(forma$IDENTIFICA), decreasing=TRUE)
ids <- unique(forma$IDENTIFICA)

dadesbcn <- lapply(ids, llegeixbcn)
names(dadesbcn) <- ids

save(dadesbcn, file="~/varis/dades_cataleg_bcn.RData")
