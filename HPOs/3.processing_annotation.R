# S'ha descarregat l'arxiu phenotype.hpoa de la pàgina web de Human Phenotype Ontology.
# Utilitzant gawk s'han seleccionat només aquelles columnes que contenen el ID de les malalties
# (de OMIM, Orphanet o Dechipher) i el seu nom. S'han eliminat totes aquelles files duplicades.
# En alguns noms de malalties apareixen els dígits del ID a l'inici del nom. Aquests dígits s'han
# eliminat. Finalment, s'ha guardat l'arxiu obtingut amb el nom de annotation_filtered.txt i es
# carrega a R
ann <- read.table("./HPOs/annotation_filtered.txt", sep = "\t", quote = "")
colnames(ann) <- c("DB_ID", "DB_Name")
nrow(ann)
head(ann)

# Es llegeix també l'arxiu ptg_subset.RData guardat anteriorment
ptg_subset <- readRDS("./HPOs/ptg_subset.RData")
head(ptg_subset)

# Es guarden els ID de malalties que estan presents en el data frame ptg_subset (sense comptar
# els duplicats)
ID_subset <- unique(as.character(ptg_subset$DiseaseID))

# Es seleccionen només aquells ID de malalties del data frame ann que estan presents en el 
# data frame ptg_subset
ann_subset <- c()
for (i in 1:nrow(ann)) {
  if (as.character(ann$DB_ID[i]) %in% ID_subset) {
    ann_subset <- rbind(ann_subset, ann[i,])
  }
}
colnames(ann_subset) <- c("DB_ID", "DB_Name")
nrow(ann_subset)
ann_subset <- ann_subset[order(ann_subset$DB_ID),]
rownames(ann_subset) <- NULL
head(ann_subset)

# Hi ha alguns ID de malalties per als quals hi apareix més d'un nom (més d'una fila). S'ha vist
# que en general els noms es diferencien en que alguns afegeixen una poca informació extra. 
# Com que no sembla que aquesta informació sigui rellevant (poden ser simplement unes sigles) i
# per tal de simplificar, es decideix seleccionar el nom més curt per a cada ID.
# Per a fer-ho, es comença guardant en un vector els IDs únics
ann_ID_unique <- unique(as.character(ann_subset$DB_ID))

# Per a cada ID únic:
ann_filtered <- c()
for (i in 1:length(ann_ID_unique)) {
  # Es guarda una taula amb les files que contenen aquest ID
  equal_ID <- subset(ann_subset, ann_subset$DB_ID == ann_ID_unique[i])
  # Si l'ID només està en una fila, es guarda la fila en el nou data frame
  if (nrow(equal_ID) == 1) {
    ann_filtered <- rbind(ann_filtered, equal_ID)
  }
  # Si l'ID està en varies files i, per tant, té més d'un nom diferent, es calcula la llargada
  # de cada nom i es guarda el més curt en el nou data frame
  else {
    l <- c()
    for (r in 1:nrow(equal_ID)) {
      l <- c(l, nchar(as.character(equal_ID$DB_Name[r])))
    }
    n <- which.min(l)
    ann_filtered <- rbind(ann_filtered, equal_ID[n,])
  }
}
colnames(ann_filtered) <- c("DB_ID", "DB_Name")
rownames(ann_filtered) <- NULL
nrow(ann_filtered)
head(ann_filtered)

# Es guarda el data frame resultant com un objecte de R
saveRDS(ann_filtered, "./HPOs/annotation_subset.RData")



