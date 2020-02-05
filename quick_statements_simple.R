# entitats de població sense P31
library(readr)
entitats_cawiki_sense_p31 <- read_delim("~/DADES/pere/varis/entitats_cawiki_sense_p31.tsv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

items <- entitats_cawiki_sense_p31$title
instruccions <- paste(items, "P31", "Q486972", sep="\t")
cat(paste(instruccions, collapse="\n")) #pantalla
