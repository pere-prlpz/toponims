#scripts per importar el fitxer de noms geogràfics de l'ICGC i pujar-lo a Wikidata
rm(list=ls())

#carregar de Wikidata tots els elements
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

# Muntanyes dels Pirineus Orientals
totpunts <- getquery("SELECT DISTINCT ?muntanya ?muntanyaLabel ?lat ?lon ?altitud 
WHERE {
  ?muntanya wdt:P31 wd:Q8502.
  ?muntanya wdt:P131* wd:Q12709.
  ?muntanya p:P625 ?coordinate .
  ?muntanya wdt:P625 ?coord.
?coordinate psv:P625 ?coordinate_node .
?coordinate_node wikibase:geoLatitude ?lat .
?coordinate_node wikibase:geoLongitude ?lon .
OPTIONAL {?muntanya wdt:P2044 ?altitud.}
  SERVICE wikibase:label {
bd:serviceParam wikibase:language 'ca' . } 
}")

library(exifr)
ruta <- "~/DADES/pere/varis"
nomimatge <- "Panoràmica_des_de_Mont-Lluís_-_20220723_160810.jpg"
pvexif <- read_exif(file.path(ruta, nomimatge))
latpvr <- pvexif$GPSLatitude*pi/180 # assumeixo N i E
lonpvr <- pvexif$GPSLongitude*pi/180
totpunts <- within(totpunts, {
  latr <- lat*pi/180
  lonr <- lon*pi/180
  distpvr <- acos(cos(pi/2-latr)*cos(pi/2-latpvr)+
                    sin(pi/2-latr)*sin(pi/2-latpvr)*cos(lonr-lonpvr))
  rumbr <- asin(sin(pi/2-latr)*sin(lonr-lonpvr)/sin(distpvr))
  rumbr <- ifelse(latr>latpvr, rumbr, pi-rumbr)
})
# head(totpunts)
# #totpunts$distpvr*6731
# #summary(totpunts$distpvr*6731)
# summary(totpunts$rumbr)
# hist(totpunts$rumbr)
# hist(totpunts$rumbr, breaks="scott")
# plot(distpvr~rumbr, data=totpunts)
# plot(latr~lonr, data=totpunts)
# with(totpunts, plot(distpvr*sin(rumbr),distpvr*cos(rumbr), asp=1)

library(jpeg)
nomimatgered <- "Panoràmica_des_de_Mont-Lluís_-_20220723_160810_red25.jpg"
fons <- readJPEG(file.path(ruta, nomimatgered))
himatge <- pvexif$ExifImageHeight
wimatge <- pvexif$ExifImageWidth
aspimatge <- himatge/wimatge

coneguts <- data.frame(nom=c("Puig de la Tossa", "Cambra d'Ase"),
                       pos=c(967, 3384)/4976)
units <- merge(coneguts, totpunts, by.x="nom", by.y="muntanyaLabel", all.x=TRUE)
units <- units[,c("rumbr","pos")]
units <- units[order(units$pos),]
units$rumbr <- units$rumbr + ifelse(units$rumbr<units$rumbr[1], pi, 0)
model <- lm(pos~rumbr, data=units)
totpunts$pos <- predict(model, data.frame(rumbr=totpunts$rumbr))
summary(totpunts$pos)
totpunts$pos <- totpunts$pos %% (model$coefficients["rumbr"]*2*pi)

plot(0,0, t="n", xlim=c(0,1), ylim=c(0, aspimatge))#, asp=1)
rasterImage(fons,0,0,1,aspimatge)
abline(v=totpunts$pos)


plot(0,0, t="n", xlim=c(.5,.7), ylim=c(0, aspimatge))#, asp=1)
rasterImage(fons,0,0,1,aspimatge)
abline(v=totpunts$pos)
with(totpunts, text(pos,0,muntanyaLabel, 
                    srt=90, adj=c(0,0), cex=.7, col="orange"))

plot(0,0, t="n", xlim=c(.5,.7), ylim=c(-0.02, aspimatge))#, asp=1)
rasterImage(fons,0,0,1,aspimatge)
abline(v=totpunts$pos)
with(totpunts, text(pos,-0.02,muntanyaLabel, 
                    srt=90, adj=c(0,0), cex=.8, col="orange"))

summary(totpunts$pos)
summary(totpunts$distpvr*6731)
puntsprop <- totpunts[totpunts$distpvr*6731<30,]

plot(0,0, t="n", xlim=c(.15,.3), ylim=c(-0.02, aspimatge))#, asp=1)
rasterImage(fons,0,0,1,aspimatge)
abline(v=puntsprop$pos)
with(puntsprop, text(pos,-0.02,muntanyaLabel, 
                    srt=90, adj=c(0,0), cex=.8, col="orange"))

plot(0,0, t="n", xlim=c(.46,.57), ylim=c(-0.02, aspimatge))#, asp=1)
rasterImage(fons,0,0,1,aspimatge)
abline(v=puntsprop$pos)
with(puntsprop, text(pos,-0.02,muntanyaLabel, 
                     srt=90, adj=c(0,0), cex=.8, col="orange"))

