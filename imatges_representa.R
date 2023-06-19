# posar P180 a les imatges que es facin servir amb P18

# ús de les meves imatges a Wikidata

require(XML)
require(xml2)
dades <- read_xml("https://glamtools.toolforge.org/glamorous.php?doit=1&username=Pere+prlpz&use_globalusage=1&ns0=1&show_details=1&projects[wikidata]=1&format=xml")
#dades <- read_xml("https://glamtools.toolforge.org/glamorous.php?doit=1&username=Pere+prlpz&use_globalusage=1&show_details=1&projects[wikidata]=1&format=xml")
dades1 <- xmlParse(dades)
dades2 <- xmlToList(dades1)

dfuna <- function(x) {
  qids <- character(0)
  for (i in 1:(length(x)-1)) {
    if (x[[i]]$.attrs == "wikidata.wikipedia" ) {
      qids <- c(qids, x[[i]]$namespace$page)
    }
  }
  if (length(qids)>0) {
    df <- data.frame(qids, nom=x$.attrs["name"], url=x$.attrs["url_page"])  
  } else {
    df <- data.frame(quids=character(0), nom=character(0), url=character(0))
  }
  return(df)
}

dfuna(dades2$details[[1]])

dftots <- lapply(dades2$details, dfuna)
usats <- do.call(rbind, dftots)
head(usats)
tail(usats)

# llegir representa
# descarregar CSV de https://w.wiki/6SMR
representen <- read.csv("~/varis/query_fotos_representen.csv")
representen$url <- gsub("http://commons.wikimedia.org/wiki/Special:FilePath/", 
                        "//commons.wikimedia.org/wiki/File:", 
                        representen$image, fixed = TRUE)
representen$url <- URLdecode(representen$url)
representen$url <- gsub(" ","_", representen$url, fixed = TRUE)


# unim
units <- merge(usats, representen)

#falta separar les que els falta el representa i posar-lo.
head(units)

units$qidrep <- gsub("http://www.wikidata.org/entity/","",units$item)
units$mid <- gsub("https://commons.wikimedia.org/entity/","",units$file)

possibles <- units[,c("mid","qids")]
existents <- units[,c("mid","qidrep")]
names(possibles) <- c("mid","qid")
names(existents) <- c("mid","qid")
existents <- existents[existents$qid != "",]
posar <- possibles[!do.call(paste, possibles) %in% do.call(paste, existents),]
posar <- posar[!duplicated(posar),]
posar <- posar[grepl("Q[0-9]+", posar$qid),]

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}


quick <- function(dades) {
  instr <- list(c(dades$mid,"P180",dades$qid))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return(instr)
}

instruccions <- paste(unlist(lapply(1:nrow(posar), function(i) quick(posar[i,]))), sep="\t", collapse="\n")
#cat(instruccions)
cat(enc2utf8(instruccions), file="~/varis/instruccions.txt")

