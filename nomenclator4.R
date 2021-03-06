library(httr)
library(rjson)

treuvar <- function(var, bind) {
  sapply(bind, function(x) (x[[var]]$value))
}

desllista <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(unlist(x))
}

# nuclis existents
url <- "https://query.wikidata.org/sparql?query=%23Nuclis%20de%20poblaci%C3%B3%20de%20Catalunya%0ASELECT%20DISTINCT%20%3Fitem%20%3Fname%20%3Fcoord%20%3Flat%20%3Flon%20%3Fmun%20%3Fnmun%0AWHERE%20%7B%0Ahint%3AQuery%20hint%3Aoptimizer%20%22None%22%20.%0A%3Fitem%20wdt%3AP131*%20wd%3AQ5705%20.%0A%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ486972%20.%0A%3Fitem%20wdt%3AP131%20%3Fmun%20.%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%22%20.%0A%3Fmun%20rdfs%3Alabel%20%3Fnmun%0A%7D%0A%7D%0AORDER%20BY%20ASC%20(%3Fname)"
cont <- fromJSON(rawToChar(content(GET(url))))
nomsvars <- cont$head$vars
llista <- lapply(nomsvars, treuvar, bind=cont$results$bindings)
names(llista) <- nomsvars
llista <- lapply(llista, desllista)
df <- as.data.frame(llista, stringsAsFactors = FALSE)
df$lat <- as.numeric(df$lat)
df$lon <- as.numeric(df$lon)
nuclis <- df

# nuclis propers sense divisió territorial
url <- "https://query.wikidata.org/sparql?query=%23Nuclis%20de%20poblaci%C3%B3%20d%27Espanya%20sense%20P131%20(intent)%0ASELECT%20%3Fitem%20%3Fname%20%3Fcoord%20%3Flat%20%3Flon%0AWHERE%20%7B%0A%3Fitem%20wdt%3AP17%20wd%3AQ29%20.%0A%3Fitem%20wdt%3AP31%2Fwdt%3AP279*%20wd%3AQ486972%20.%0A%20%20MINUS%20%7B%3Fitem%20wdt%3AP131%20%5B%5D%7D%0A%20%20OPTIONAL%20%7B%0A%3Fitem%20wdt%3AP625%20%3Fcoord%20.%0A%3Fitem%20p%3AP625%20%3Fcoordinate%20.%0A%3Fcoordinate%20psv%3AP625%20%3Fcoordinate_node%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLatitude%20%3Flat%20.%0A%3Fcoordinate_node%20wikibase%3AgeoLongitude%20%3Flon%20.%0A%20%20%7D%0A%20%20FILTER%20(%3Flon%20%3E%200.15)%0A%20%20FILTER%20(%3Flat%20%3E%2040.49)%0ASERVICE%20wikibase%3Alabel%20%7B%0Abd%3AserviceParam%20wikibase%3Alanguage%20%22ca%2Cca%2Cen%2Ces%2Can%2Ceu%2Cpl%2Csv%2Cceb%22%20.%0A%3Fitem%20rdfs%3Alabel%20%3Fname%0A%7D%0A%7D%0A%23ORDER%20BY%20ASC%20(%3Fname)%0A"
cont <- fromJSON(rawToChar(content(GET(url))))
nomsvars <- cont$head$vars
llista <- lapply(nomsvars, treuvar, bind=cont$results$bindings)
names(llista) <- nomsvars
llista <- lapply(llista, desllista)
df <- as.data.frame(llista, stringsAsFactors = FALSE)
df$lat <- as.numeric(df$lat)
df$lon <- as.numeric(df$lon)
nuclis131 <- df

#save(nuclis, nuclis131, file="C:\\Users\\Pere\\Documents\\DADES\\pere\\varis\\nuclis_wd.RData")

#load(file="C:\\Users\\Pere\\Documents\\DADES\\pere\\varis\\nuclis_wd.RData", verbose = TRUE)

#uneixo
colnames(nuclis)[!colnames(nuclis) %in% colnames(nuclis131)]
nuclis131[,colnames(nuclis)[!colnames(nuclis) %in% colnames(nuclis131)]] <- NA
nuclis <- rbind(nuclis, nuclis131, stringsAsFactors=FALSE)

# nomenclàtor
library(readxl)
index_nomenclator_2009 <- read_excel("~/DADES/pere/varis/index_nomenclator_2009.xls")
conceptes_pob <- c("barri", "cap", "diss.", "e.m.d.", "nucli")
nomred <- 
  index_nomenclator_2009[index_nomenclator_2009$Concepte %in% conceptes_pob, 1:13]
nomred <- nomred[!is.na(nomred$`UTM X`),]

# canvi coordenades nomenclàtor
library(rgdal)
coords <- as.matrix(nomred[, c("UTM X","UTM Y")])*100
#sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=31N +datum=WGS84"))
sputm <- 
  SpatialPoints(
    coords, 
    proj4string=
      CRS("+init=epsg:23031 +proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo.df <- as.data.frame(spgeo)
names(spgeo.df) <- c("lon", "lat")
nomred <- cbind(nomred, spgeo.df)

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

nomred$nom <- sapply(nomred$Topònim, arreglanom)

#Conversió municipis WD (noms IEC actuals) a nomenclàtor (noms oficials antics) 
actnom <- function(name) {
  name[name=="La Granada"] <- "la Granada"
  name[name=="La Sénia"] <- "la Sénia"
  name[name=="Calonge i Sant Antoni"] <- "Calonge"
  name[name=="el Figueró i Montmany"] <- "Figaró-Montmany"
  name[name=="Roda de Berà"] <- "Roda de Barà"
  name[name=="Navars"] <- "Navàs"
  name[name=="l'Esquirol"] <- "Santa Maria de Corcó"
  return(name)  
}

# nom dels nuclis existents
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

# treu el/la
treuart <- function(nom) {
  gsub("^(el|la|els|les|l') ","",nom)
}


nuclis$nomnet <- sapply(nuclis$name, treupar)

# noms per enganxar
nuclis$nomrel <- treuart(tolower(nuclis$nomnet))
nomred$nomrel <- treuart(tolower(nomred$nom))
nuclis$munrel <- tolower(actnom(nuclis$nmun))
nomred$munrel <- tolower(nomred$`Municipi 1`)
nomred$munrel <- gsub(" *","",nomred$munrel, fixed = TRUE)
nomred$ref <- 1:nrow(nomred)
units <- merge(nuclis, nomred, by=c("nomrel", "munrel"), all=FALSE)

quedenwd <- nuclis[!nuclis$item %in% units$item,]
quedennom <- nomred[!nomred$ref %in% units$ref,]

#comprovo els dels noms repetits (sense que consti municipi igual)
unitsnom <- merge(quedenwd, quedennom, by=c("nomrel"), all=FALSE)

# distància segons coordenades
dist <- function(lat.x, lon.x, lat.y, lon.y) {
  sqrt(((6371*(lat.x-lat.y)*pi/180)^2+(6371*cos(lat.x/180*pi)*(lon.x-lon.y)*pi/180)^2))
}

# busco repetits mirant nom i distància (7km)
unitsnom$dist <- with(unitsnom, dist(lat.x, lon.x, lat.y, lon.y))
unitsnom <- unitsnom[unitsnom$dist<7 | !is.na(unitsnom$dist),] #segurs i possibles
table(quedenwd$item %in% unitsnom$item)
table(quedennom$ref %in% unitsnom$ref)
table(is.na(unitsnom$dist))
quedennom <- quedennom[!quedennom$ref %in% unitsnom$ref,]
unitsnom <- unitsnom[unitsnom$dist<7 & !is.na(unitsnom$dist),] #només segurs
quedenwd <- quedenwd[!quedenwd$item %in% unitsnom$item,]

# elimino només per distància (800 m)
mdist <- outer(1:nrow(quedennom), 1:nrow(quedenwd),
               function(i,j) {dist(quedenwd$lat[j], quedenwd$lon[j],
                                   quedennom$lat[i],quedennom$lon[i])})
minnom <- apply(mdist, 1, min, na.rm=TRUE)
minwd <- apply(mdist, 2, min, na.rm=TRUE)
quedennom <- quedennom[minnom>.8,]
quedenwd <- quedenwd[minwd>.8,]

crear <- quedennom[quedennom$Concepte %in% c("nucli","diss.", "barri"),]

# importar taula de municipis
library(readr)
municipis <- read_delim("~/DADES/pere/varis/municipis.tsv",
                        "\t", escape_double = FALSE, trim_ws = TRUE)
#arreglo errors
municipis <- municipis[!(municipis$name=="Vilademuls"& municipis$comarca=="Alt Empordà"),]

# afegir municipis als que s'han de crear
municipis$munrel <- tolower(actnom(municipis$name))
crear <- merge(crear, municipis, by="munrel", all.x=TRUE)
crear$munrel2 <- tolower(crear$`Municipi 2`)
crear$munrel2 <- gsub(" *","",crear$munrel2, fixed = TRUE)
crear <- merge(crear, municipis, by.x="munrel2", by.y="munrel", all.x=TRUE)
crear$item.x <- gsub("http://www.wikidata.org/entity/", "",
                     crear$item.x, fixed=TRUE)
crear$item.y <- gsub("http://www.wikidata.org/entity/", "",
                     crear$item.y, fixed=TRUE)
# tipus
tipus <- data.frame(Concepte=c("nucli", "diss.", "barri"),
                    iconc=c("Q486972","Q16557344","Q123705"),
                    dconc=c("Nucli de població",
                            "Disseminat",
                            "Barri"),
                    dconcen=c("Populated place",
                              "Populated place",
                              "Neighborhood"),
                    stringsAsFactors = FALSE)
crear <- merge(crear, tipus)

# llengua
crear$llengua <- ifelse(crear$`Comarca 1`=="VAR", "oc", "ca")

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
  return(denom)
}

cometes <- function(text) {
  paste0('"',text,'"')
}

# preparar quickstatemens
quick <- function(fila) {
  instr <- list(c("CREATE"))
  instr[[2]] <- c("LAST", "Lca", cometes(fila$nom)) 
  instr[[3]] <- c("LAST", "Dca", 
                  paste0('"',fila$dconc, " ",
                         "del municipi ", de(fila$name.x),
                         " (",fila$comarca.x,")",'"'))
  instr[[4]] <- c("LAST", "P31", fila$iconc)
  instr[[5]] <- c("LAST", "P131", fila$item.x, "S248", "Q11938912")
  instr[[6]] <- c("LAST", "P625", 
                  paste0("@", fila$lat,"/", fila$lon),
                  "S248", "Q11938912")
  instr[[7]] <- c("LAST", "P17", "Q29")
  instr[[8]] <- c("LAST", "Len", cometes(fila$nom)) 
  instr[[9]] <- c("LAST", "Den", 
                  paste0('"',fila$dconcen,
                         " in ", fila$name.x,
                         " (",fila$comarca.x,")",'"'))
  instr[[10]] <- c("LAST", "Leu", cometes(fila$nom)) 
  instr[[11]] <- c("LAST", "Loc", cometes(fila$nom)) 
  instr[[12]] <- c("LAST", "Lfr", cometes(fila$nom)) 
  instr[[13]] <- c("LAST", "Lpt", cometes(fila$nom)) 
  instr[[14]] <- c("LAST", "P1448", 
                   paste0(fila$llengua,":",
                          cometes(fila$nom)), 
                   "S248", "Q11938912")
  if (!is.na(fila$item.y)) {
    instr[[15]] <- c("LAST", "P131", fila$item.y, "S248", "Q11938912")
  }
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla
#cat(enc2utf8(paste(instruccions, collapse="\n")),
#    file="~/DADES/pere/varis/instruccions.txt")

#Comprovació municipis
unique(crear[is.na(crear$item.x), "Municipi 1"])
unique(crear[is.na(crear$item.x), "munrel"])

#resum
#units pel nom i municipi
nrow(units)
#units per nom i posició: REPASSAR
table(unitsnom$dist<7 & !is.na(unitsnom$dist))
#no creats per nom semblant REPASSAR
table(is.na(unitsnom$dist))

#CONFIRMAR QUE ELS QUE NO TENEN COORDENADES S'ELIMINEN SI COINCIDEIX EL NOM
#NO ELIMINAR SI HI HA MUNICIPI I NO COINCIDEIX
#IDEA: ELIMINAR PER NOM I COMARCA
#REPASSAR ELS QUE ESTAN A MÉS DE 3KM COINCIDINT NOM (O NOM I MUNICIPI)
