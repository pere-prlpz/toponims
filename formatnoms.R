#Scripts per formatejar noms i ids per les llistes de masies.
#Fa servir els data frames caregats amb edificis_ng.R.

noms <- scan("clipboard", what=character(), sep="\n")
noms <- gsub(".*\t","", noms)
mun <- unique(idescat[idescat$llocLabel %in% noms,])
cat(paste0("[[",noms,"]]", collapse=", "))
cat(paste0("wd:",mun$lloc, collapse=" "))

dput(noms)
tot <- c("Campins", "Cànoves i Samalús", "Fogars de Montclús", "Gualba", 
          "Llinars del Vallès", "Montseny", "Sant Antoni de Vilamajor", 
          "Sant Celoni", "Sant Esteve de Palautordera", "Sant Pere de Vilamajor", 
          "Santa Maria de Palautordera", "Vallgorguina", "Vilalba Sasserra")
oest <- c("Cànoves i Samalús",
          "Llinars del Vallès", "Sant Antoni de Vilamajor", 
          "Sant Pere de Vilamajor", 
          "Vilalba Sasserra")
est <- tot[!(tot %in% oest)]

altm <- c("Arenys de Mar", "Arenys de Munt", "Calella", "Canet de Mar", 
         "Malgrat de Mar", "Montgat", "Palafolls", "Pineda de Mar", "Sant Cebrià de Vallalta", 
         "Sant Iscle de Vallalta", "Sant Pol de Mar", "Santa Susanna", 
         "Tordera")

#ngcatv10cs0f1r011[ngcatv10cs0f1r011$Toponim %in% altm,]
tot <- unique(ngcatv10cs0f1r011$NomMun1[ngcatv10cs0f1r011$CodiCom1=="APE"&
                                   ngcatv10cs0f1r011$CodiGeo %in% c("10000","10100")])
noms <- tot[!(tot %in% est)]

dput(tot)
est <- c("Avinyonet del Penedès", "el Pla del Penedès", 
  "Gelida",  "la Granada", 
   "Torrelavit", "les Cabanyes", "Olesa de Bonesvalls", 
  "Puigdàlber", "Sant Cugat Sesgarrigues", 
  "Mediona", "Sant Llorenç d'Hortons", "Olèrdola", 
  "Subirats", "Sant Pere de Riudebitlles", "Sant Quintí de Mediona", 
  "Sant Sadurní d'Anoia", "Santa Fe del Penedès", "Vilafranca del Penedès")
oest <- c( "Santa Margarida i els Monjos", 
  "Font-rubí", "Castellet i la Gornal", 
  "Castellví de la Marca",
  "Pacs del Penedès", "Pontons",
  "Sant Martí Sarroca", "Torrelles de Foix", 
  "Vilobí del Penedès")
table(tot %in% est, tot %in% oest)
oest[oest %in% est]
est <- sort(est)
oest <- sort(oest)
noms <- est
noms <- oest

table(ngcatv10cs0f1r011$CodiCom1[ngcatv10cs0f1r011$CodiGeo=="10301"])
sort(table(ngcatv10cs0f1r011$CodiCom1[ngcatv10cs0f1r011$CodiGeo=="10301"]))

tot <- unique(ngcatv10cs0f1r011$NomMun1[ngcatv10cs0f1r011$CodiCom1=="ANO"&
                                          ngcatv10cs0f1r011$CodiGeo %in% c("10000","10100")])
est <- c( "Orpí", "Cabrera d'Anoia", 
         "Capellades", "Carme",  "Castellolí", 
          "el Bruc", "els Hostalets de Pierola", 
          "la Llacuna", "la Pobla de Claramunt", 
         "la Torre de Claramunt", "Masquefa",  "Òdena", "Piera", 
         "Santa Margarida de Montbui",  
         "Vallbona d'Anoia","Vilanova del Camí", "Igualada")
nord <- c("Argençola", "Bellprat", "Calaf", "Orpí", "Cabrera d'Anoia", 
         "Capellades", "Carme", "Castellfollit de Riubregós", "Castellolí", 
         "Copons", "Calonge de Segarra", "el Bruc", "els Hostalets de Pierola", 
         "els Prats de Rei", "Jorba", "la Llacuna", "la Pobla de Claramunt", 
         "la Torre de Claramunt", "Masquefa", "Montmaneu", "Òdena", "Piera", 
         "Pujalt", "Rubió", "Sant Martí de Tous", "Sant Martí Sesgueioles", 
         "Sant Pere Sallavinera", "Santa Margarida de Montbui", "Santa Maria de Miralles", 
         "Vallbona d'Anoia", "Veciana", "Vilanova del Camí", "Igualada")
table(tot %in% sud, tot %in% nord)
oest[sud %in% nord]

table(ngcatv10cs0f1r011$NomMun1[ngcatv10cs0f1r011$CodiGeo=="10301" 
                                & ngcatv10cs0f1r011$CodiCom1=="ANO"])
