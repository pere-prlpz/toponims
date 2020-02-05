# nuclis existents
library(readr)
nuclis <- read_delim("C:\\Users\\Pere\\Documents\\DADES\\pere\\varis\\nuclis_wd.tsv",
                     "\t", escape_double = FALSE, trim_ws = TRUE)

# nomenclàtor
library(readxl)
index_nomenclator_2009 <- read_excel("~/DADES/pere/varis/index_nomenclator_2009.xls")
nomred <- index_nomenclator_2009[index_nomenclator_2009$Concepte=="nucli", 1:13]
nomred <- nomred[!is.na(nomred$`UTM X`),]

# canvi coordenades nomenclàtor
library(rgdal)
coords <- as.matrix(nomred[, c("UTM X","UTM Y")])*100
#sputm <- SpatialPoints(coords, proj4string=CRS("+proj=utm +zone=31N +datum=WGS84"))
sputm <- SpatialPoints(coords, 
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

# nom dels existents
treupar <- function(nom) {
  trossos <- strsplit(nom," (", fixed = TRUE)
  return(trossos[[1]][1])
}

nuclis$nomnet <- sapply(nuclis$name, treupar)

# noms per enganxar
nuclis$nomrel <- tolower(nuclis$nomnet)
nomred$nomrel <- tolower(nomred$nom)
nuclis$munrel <- tolower(nuclis$nmun)
nomred$munrel <- tolower(nomred$`Municipi 1`)
nomred$munrel <- gsub(" *","",nomred$munrel, fixed = TRUE)
units <- merge(nuclis, nomred, by=c("nomrel", "munrel"), all.y=TRUE)

# distància
dist <- function(lat.x, lon.x, lat.y, lon.y) {
  sqrt(((6371*(lat.x-lat.y)*pi/180)^2+(6371*cos(lat.x/180*pi)*(lon.x-lon.y)*pi/180)^2))
}

# més allunyats
with(units, units[dist(lat.x, lon.x, lat.y, lon.y)>5 & !is.na(lat.x),])

# busco on afegir coordenades
afegircoor <- units[is.na(units$lat.x) & !is.na(units$lat.y) & !is.na(units$item), ]
afegircoor$item <- gsub("http://www.wikidata.org/entity/", "",
                        afegircoor$item , fixed=TRUE)

# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr[[1]] <- c(fila$item, "P625", 
                  paste0("@", fila$lat.y,"/", fila$lon.y))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(afegircoor), function(i) {quick(afegircoor[i,])}))
cat(paste(instruccions, collapse="\n")) #pantalla


