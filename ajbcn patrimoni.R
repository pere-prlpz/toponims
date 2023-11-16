library(sf)
library(oce)
library(stringr)

# llegir fitxer
ruta <- "C:\\Users\\Pere\\Downloads\\0703060100_BCN_Patrimoni_Arqui"
forma <- read_sf(file.path(ruta,"0703060100_BCN_Patrimoni_Arqui_POLIGONS_V.shp"))

# diccionaris i dades
diccprot <- c(CPA_03_PL="Q107542530")
districtes <- read.csv("~/varis/districtes.txt")
districtes <- districtes[order(districtes$num),]

mintitol <- function(nom) {
  nom <- str_to_title(nom)
  nom <- gsub(" D'", " d'", nom)
  nom <- gsub(" El "," el ", nom)
  nom <- gsub(" Els "," els ", nom)
  nom <- gsub(" La "," la ", nom)
  nom <- gsub(" Les "," les ", nom)
}

# monument a buscar ################
id <- "2945" 

# buscar i entendre dades del monument
registre <- forma[forma$IDENTIFICA==id,]
nom <- mintitol(registre$DENOMIN)
totcoord <- do.call(rbind, lapply(registre$geometry, as.matrix))
coordutm <- apply(totcoord, 2, median)
coord <- unlist(utm2lonlat(coordutm[1],coordutm[2], zone=31))
qprot <- diccprot[registre$NIVELL]
url <- paste0("https://ajuntament.barcelona.cat/informaciourbanistica/cerca/ca/fitxa/",
              id,
              "/--/--/cp/")
districte <- districtes$item[as.numeric(registre$DISTRICTE)]

dades <- list(nom=nom[1], id=id, coord=coord, qprot=qprot[1], 
              url=url, districte=districte[1],
              qtipus="Q41176")

#afegir "de"
de <- function(nom) {
  if (grepl("^els? ", nom)) {
    denom <- paste0("d",nom)
  } else {
    if (grepl("^[AEIOUÀÈÉÍÒÓÚ]", nom)) {
      denom <- paste0("d'",nom)
    } else {
      denom <- paste0("de ",nom)
    }
  }
  denom <- gsub("^d'Els ", "dels ", denom)
  denom <- gsub("^d'El ", "del ", denom)
  return(denom)
}

#minúscules
minart <- function(nom) {
  nom <- gsub("^El ","el ", nom)
  nom <- gsub("^Els ","els ", nom)
  nom <- gsub("^La ","la ", nom)
  nom <- gsub("^Les ","les ", nom)
  return(nom)
}


# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}
# instruccions pel quickstatement
quick <- function(dades, url=dades$url, qid="LAST", altres=c(""), descr=TRUE) {
  curl <- cometes(url)
  if (qid=="LAST") {
    instr <- list(c("CREATE"))
    Lca <- "Lca"
    Len <- "Len"
  } else {
    instr <- list()
    Lca <- "Aca"
    Len <- "Aen"
  }
  instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
  instr <- afegeix(instr, c(qid, Len, cometes(dades$nom)))
  if (descr) {
    instr <- afegeix(instr,c(qid, "Dca", cometes("Edifici de Barcelona")))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes("Building in Barcelona")))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$districte, 
                            "S248", "Q116698266", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P625", 
                            paste0("@", paste0(dades$coord[2:1], 
                                               collapse="/")),
                            "S248", "Q116698266", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P17", "Q29")) 
  instr <- afegeix(instr, c(qid, "P973", curl, "P407", "Q7026"))
  instr <- afegeix(instr, c(qid, "P11557", cometes(dades$id), 
                            "S248", "Q116698266", "S854", curl))
  if (!is.na(dades$qtipus)) {
    instr <- afegeix(instr, c(qid, "P31", dades$qtipus, 
                              "S248", "Q116698266", "S854", curl))  
  }
  if (length(dades$qprot)>0) {
    if (!is.na(dades$qprot)) {
      instr <- afegeix(instr, c(qid, "P1435", dades$qprot, 
                                "S248", "Q116698266", "S854", curl))  
    }
  }
  if (length(dades$qestil)>0) {
    if (!is.na(dades$qestil)) {
      instr <- afegeix(instr, c(qid, "P149", dades$qestil, 
                                "S248", "Q116698266", "S854", curl))  
    }
  }
  return(instr)
}

cat(enc2utf8(
  paste(c(unlist(
    quick(dades)), 
    sep="\t", collapse="\n"),"\n")))
