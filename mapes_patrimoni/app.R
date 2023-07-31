#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## Previs

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

## Per carregar municipis si ja existeixen
if (file.exists("idescat.RData")) { # PER FER: path adaptable
  load(file="idescat.RData", verbose=TRUE)
} else {
  idescat <- getquery("SELECT ?lloc ?llocLabel ?idescat 
  WHERE {
  ?lloc wdt:P4335 ?idescat.
  SERVICE wikibase:label {bd:serviceParam wikibase:language 'ca' .}
  }")
  idescatAlias <- getquery('SELECT ?lloc ?llocLabel ?idescat ?alias
  WHERE {
  ?lloc wdt:P4335 ?idescat.
  ?lloc skos:altLabel ?alias.
  FILTER(LANG(?alias) = "ca").
  SERVICE wikibase:label {bd:serviceParam wikibase:language "ca".}
  }')
  save(idescat, idescatAlias, file="idescat.RData")
}


## funcions per llegir pàgina mapa patrimoni culturlal

library(stringr)

treucarac <- function(carac, pag) {
  i0 <- grep(paste0('<div class="patrimonial-element__',carac,'">'), pag)
  if (length(i0)==0) {
    return(NA)
  }
  i1 <- grep("</div>", pag)
  i1 <- min(i1[i1>i0])
  dada <- pag[i0:i1]
  dada <- dada[grep('<div class="patrimonial-element__item">', dada)]
  dada <- gsub('^.*<div class="patrimonial-element__item">(.*)</div>.*$', "\\1", dada)
  dada <- gsub(",",".",dada)
  return(dada)  
}

unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

llegeix <- function(url) {
  pag <- readLines(url, encoding="UTF-8")
  tit <- pag[grepl("<title>.*</title>", pag)]
  nom <- gsub("^.*<title>(.*)\\|.*</title>.*$", "\\1", tit)
  nom <- str_trim(nom)
  nom <- unescape_html(gsub(" $","",nom))
  mun <- pag[grepl('<a href=".*/municipi/.*">.*</a>', pag)]
  mun <- gsub('<a href=".*/municipi/.*">(.*)</a>', "\\1", mun)
  mun <- unescape_html((str_trim(mun)))
  nom <- gsub(paste0("\\. ",mun), "", nom)
  qmun <- idescat$lloc[tolower(idescat$llocLabel)==tolower(mun)]
  diumasia <- any(grepl("[Mm]as((over)?ia)?\\b",pag))
  if (length(qmun)==0) {
    qmun <- idescatAlias$lloc[tolower(idescatAlias$alias)==tolower(mun)]
    if (length(qmun)==1) {
      mun <- idescat$llocLabel[idescat$lloc==qmun]
    }
  }
  caracs <- c("height", "latitude", "longitude")
  lcaracs <- lapply(caracs, treucarac, pag)
  names(lcaracs) <- caracs
  itipologia <- grep('<div class="field__label">Tipologia</div>', pag)
  tipologia <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[itipologia+1])
  tipologia <- str_trim(gsub('^.*<div class="field__item">', "", tipologia))
  iest <- grep('<div class="field__label">Estat de conservació</div>', pag)
  cons <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[iest+1])
  estcons <- c("Bo"="Q56557591", "Regular"="Q106379705")
  qcons <- estcons[cons]
  if (is.na(qcons)) {
    print(paste("Estat de conservació desconegut:", cons))
  }
  iest <- grep('<div class="field__label">Any</div>', pag)
  if (length(iest)==1) {
    any <- gsub('^.*<div class="field__item">(.*)(</div>)?.*$', "\\1", pag[iest+1])
    any <- gsub('</div>', "", any)
    segle <- NA
  } else {
    any <- NA
    iest <- grep('<div class="field__label">Segle</div>', pag)
    if (length(iest)==1) {
      segle <- gsub('^.*<div class="field__item">(.*)</div>.*$', "\\1", pag[iest+1])
    } else {
      segle <- NA
    }
  }
  resultat <- c(list(nom=nom, municipi=mun, qmun=qmun, qcons=qcons),
                lcaracs,
                any=any, segle=segle, tipologia=tipologia,
                diumasia=diumasia)
  return(resultat)
}


##################################################################
# Funcions per carregar
##############################################################

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

# afegir cometes
cometes <- function(text) {
  paste0('"',text,'"')
}

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- paste(vector, collapse="\t")
  return(llista)
}

datas <- function(any=NA, segle=NA) {
  if (!is.na(any)) {
    any <- gsub("^c. ", "", any)
    any <- gsub(" c.", "", any)
    any <- gsub("?", "", any, fixed=TRUE)
    if (grepl("-",any)) {
      any <- strsplit(any,"-")[[1]][1]
      return(paste0("+",any,"-00-00T00:00:00Z/8"))
    } else {
      return(paste0("+",any,"-00-00T00:00:00Z/9"))
    }
  } else if (!is.na(segle)) {
    inicial <- (0:20)*100+1
    names(inicial) <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", 
                        "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", 
                        "XX", "XXI")
    inici <- inicial[segle]
    if (!is.na(inici)) {
      return(paste0("+",inici,"-00-00T00:00:00Z/7"))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}


quick <- function(dades, url, qid="LAST", altres=c(""), descr=TRUE) {
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
  if (grepl("aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q18870689", 
                              "S248", "Q9028374", "S854", curl))  
  } else if (grepl("pont|viaducte|passera|aqüeducte", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q12280", 
                              "S248", "Q9028374", "S854", curl))
    terme <-  c("ca"="pont", "en"="bridge")
  } else if (grepl("font", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q483453", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="font", "en"="fountain")
  } else if ((grepl("^mas((over)?ia)? ", tolower(dades$nom)))|
             (dades$diumasia & dades$tipologia=="Edifici")) {
    instr <- afegeix(instr, c(qid, "P31", "Q585956", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="masia", "en"="masia")
  } else if (grepl("^(casa |can |ca n'|cal |ca l'|cases |habitatge )", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q3947", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="casa", "en"="house")
  } else if (grepl("^(cova|coves|gruta) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q35509", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="cova", "en"="cave")
  } else if (grepl("^avencs? ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q1435994", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="avenc", "en"="pit cave")
  } else if (grepl("^ba[lu]m(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q35509", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="balma", "en"="rock shelter")
  } else if (grepl("^barrac(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2932238", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="barraca de vinya", "en"="dry stone hut")
  } else if (grepl("^mol(í|ins) de vent", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q38720", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="molí de vent", "en"="windmill")
  } else if (grepl("^mol(í|ins) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q185187", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="molí", "en"="mill")
  } else if (grepl("museu ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q33506", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="museu", "en"="museum")
  } else if (grepl("^monuments? ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q4989906", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="monument", "en"="monument")
  } else if (grepl("^min(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q820477", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="mina", "en"="mine")
  } else if (grepl("^pedrer(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q188040", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="pedrera", "en"="quarry")
  } else if (grepl("^bass(a|es) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q3253281", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="bassa", "en"="pond")
  } else if (grepl("^gorg(a|ues)? ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2385513", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="gorg", "en"="stream pond")
  } else if (grepl("^resclosa ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q1066997", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="resclosa", "en"="weir")
  } else if (grepl("^forns? de calç ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q59772", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="forn de calç", "en"="lime kiln")
  } else if (grepl("^cementiri ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q39614", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="cementiri", "en"="cemetery")
  } else if (grepl("^xemeneia ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2962545", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="xemeneia", "en"="chimney")
  } else if (grepl("^creu ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q2309609", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="creu", "en"="cross")
  } else if (grepl("^escultura ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q860861", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="escultura", "en"="sculpture")
  } else if (grepl("^pous? de (gel|glaç|neu) ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q3666499", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="pou de gel", "en"="ice cellar")
  } else if (grepl("^bòbila", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q198632", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="bòbila", "en"="brickworks")
  } else if (grepl("^nau industrial", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q9049015", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="nau industrial", "en"="industrial building")
  } else if (grepl("^estació d'aforament", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q505774", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="estació d'aforament", "en"="stream gauge")
  } else if (grepl("^rellotge de sol ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q80793", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="rellotge de sol", "en"="sundial")
  } else if (grepl("^plaça", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q174782", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="plaça", "en"="square")
  } else if (grepl("^carrer ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q79007", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="carrer", "en"="street")
  } else if (grepl("^barri ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q123705", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="barri", "en"="neighborhood")
  } else if (grepl("^pèrgol(a|es)[- ]", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q264458", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="pèrgola", "en"="pergola")
  } else if (grepl("^sureres ", tolower(dades$nom))) {
    instr <- afegeix(instr, c(qid, "P31", "Q5688661", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="sureda", "en"="cork oak woodland")
  } else if (grepl("^xaragalls? ", tolower(dades$nom)) ) { #& dades$tipologia=="Zona d'interès"
    instr <- afegeix(instr, c(qid, "P31", "Q17300700", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="xaragall", "en"="badlands")
  } else if(dades$tipologia=="Edifici") {
    instr <- afegeix(instr, c(qid, "P31", "Q41176", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="edifici", "en"="building")
  } else if(dades$tipologia=="Element urbà") {
    instr <- afegeix(instr, c(qid, "P31", "Q13397636", 
                              "S248", "Q9028374", "S854", curl))
  } else if(dades$tipologia=="Element arquitectònic") {
    instr <- afegeix(instr, c(qid, "P31", "Q391414", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="element arquitectònic", "en"="architectural element")
  } else if(dades$tipologia=="Conjunt arquitectònic") {
    instr <- afegeix(instr, c(qid, "P31", "Q1497375", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="conjunt arquitectònic", "en"="architectural ensemble")
    } else if(dades$tipologia=="Conjunt arquitectònic") {
    instr <- afegeix(instr, c(qid, "P31", "Q1497375", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="conjunt arquitectònic", "en"="architectural ensemble")
} else if(dades$tipologia=="Espècimen botànic") {
    instr <- afegeix(instr, c(qid, "P31", "Q811534", 
                              "S248", "Q9028374", "S854", curl))
    terme <- c("ca"="arbre singular", "en"="remarkable tree")
} else {
    terme <- c("ca"="lloc", "en"="place")
    print("Instància desconeguda")
  }
  instr <- afegeix(instr, c(qid, Lca, cometes(dades$nom)))
  instr <- afegeix(instr, c(qid, Len, cometes(dades$nom))) 
  instr <- afegeix(instr, c(qid, "Aca", cometes(paste0(dades$nom," (",dades$municipi,")"))))
  if (descr & all(!is.na(terme))) {
    instr <- afegeix(instr,c(qid, "Dca", 
                             cometes(paste(terme["ca"],
                                           de(dades$municipi)))))
    instr <- afegeix(instr, c(qid, "Den", 
                              cometes(paste(terme["en"], 'in', 
                                            dades$municipi,
                                            '(Catalonia)'))))
  }
  instr <- afegeix(instr, c(qid, "P131", dades$qmun, 
                            "S248", "Q9028374", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P625", 
                            paste0("@", dades$latitude,"/", dades$longitude),
                            "S248", "Q9028374", "S854", curl))  
  instr <- afegeix(instr, c(qid, "P17", "Q29")) 
  if (!is.na(dades$height)) {
    instr <- afegeix(instr, c(qid, "P2044", 
                              paste0(gsub(" ?m$","",dades$height), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(dades$qcons)) {
    instr <- afegeix(instr, c(qid, "P5816", dades$qcons, 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["llum"])) {
    instr <- afegeix(instr, c(qid, "P2787", 
                              paste0(gsub(" ?m$","",altres["llum"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["llarg"])) {
    instr <- afegeix(instr, c(qid, "P2043", 
                              paste0(gsub(" ?m$","",altres["llarg"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["ample"])) {
    instr <- afegeix(instr, c(qid, "P2049", 
                              paste0(gsub(" ?m$","",altres["ample"]), "U11573"), 
                              "S248", "Q9028374", "S854", curl))  
  }
  if (!is.na(altres["ulls"])) {
    instr <- afegeix(instr, c(qid, "P1314", 
                              altres["ulls"], 
                              "S248", "Q9028374", "S854", curl))  
  }
  dcrea <- datas(dades$any, dades$segle)
  if (!is.na(dcrea)) {
    instr <- afegeix(instr, c(qid, "P571", 
                              dcrea, 
                              "S248", "Q9028374", "S854", curl))  
  }
  instr <- afegeix(instr, c(qid, "P973", curl, "P407", "Q7026"))
  return(instr)
}

totinstr <- function(url, qid="LAST", altres=c(""), descr=TRUE) {
  cat(enc2utf8(paste(unlist(quick(llegeix(url), url, qid, altres, descr)), sep="\t", collapse="\n")))
}


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Mapes patrimoni"),

    # Introducció de dades
    sidebarLayout(
        sidebarPanel(
            textInput("url", "url mapa de patrimoni")
        ),

        # Resultat
        mainPanel(
          "Codi per copiar a QuickStatement:",
           verbatimTextOutput("instr"),
          "Dades llegides:",
           verbatimTextOutput(("dades"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  llegit <- reactive(if (nchar(input$url)>10) {llegeix(input$url)
    } else {
      list()
    })

    output$instr <- renderPrint({
      if (length(llegit())>0) {
        cat(enc2utf8(paste(unlist(quick(llegit(), input$url)), sep="\t", collapse="\n")))
      } else {
        cat("Sense dades")
      }
      # if (nchar(input$url)>10) {totinstr(input$url)
      # } else {
      #     cat("Sense dades")
      #   }
    })

    output$dades <- renderPrint({
      llegit()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
