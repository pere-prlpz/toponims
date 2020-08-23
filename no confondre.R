# posar la declaració de no confondre a parells d'elements de Wikidata

# posar aquí els parells
parells <- scan(what="character",
                text="Q11909742 Q17617928
                Q11912807 Q11912806
                Q11910585 Q11910589
                Q17389718 Q17602272
                Q28125697 Q17588061
                Q21849328 Q21849291
                Q47185494 Q47180688
                Q11909725 Q11909721
                Q11909760 Q11909763
                Q1278218 Q11682913
                Q1278218 Q9007519
                Q11682913 Q9007519
                Q11909742 Q17617928
                Q18004676 Q11936069
                Q18004676 Q18004677
                Q11936069 Q18004677
                ")


crear <- matrix(parells, ncol=2, byrow=TRUE)
crear <- as.data.frame(crear, stringsAsFactors = FALSE)
names(crear) <-c("primer", "segon")

# funció per preparar quickstatements
afegeix <- function(llista, vector) {
  llista[[1+length(llista)]] <- vector
  return(llista)
}


# preparar quickstatemens
quick <- function(fila) {
  instr <- list()
  instr <- afegeix(instr, c(fila$primer, "P1889", fila$segon))
  instr <- afegeix(instr, c(fila$segon, "P1889", fila$primer))
  instr <- sapply(instr, FUN=paste, collapse="\t")
  return (instr)
}

instruccions <- unlist(lapply(1:nrow(crear), function(i) {quick(crear[i,])})) #1:nrow(crear)

# sortides per triar
cat(paste(instruccions, collapse="\n")) #pantalla
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/DADES/pere/varis/instruccions.txt")
cat(enc2utf8(paste(instruccions, collapse="\n")), file="~/pere/diversos/instruccions.txt")
