# S'ha descarregat l'arxiu phenotype_to_genes.txt de la web de Human Phenotype Ontology.
# Mitjançant gawk s'ha guardat en un nou arxiu anomenat phenotype_to_genes_filtered.txt
# només les columnes que contenen el ID del terme HPO, el symbol del gen i el ID de la
# malaltia o condició amb la que ha estat relacionat. Es llegeix aquest arxiu a R
ptg <- read.table("./HPOs/phenotype_to_genes_filtered.txt", sep = "\t", quote = "")
colnames(ptg) <- c("hpoID", "Gene", "DiseaseID")
nrow(ptg)
head(ptg)

# Es llegeix l'arxiu hpo_list_subset.RData que s'havia guardat anteriorment
hpo <- readRDS("./HPOs/hpo_list_subset.RData")
head(hpo)

# Es seleccionen aquells HPO ID del data frame ptg que es trobin en la llista de termes HPO 
# seleccionats aleatòriament i es filtra el data frame
n <- which(ptg$hpoID %in% hpo)
ptg_subset <- ptg[n,]
rownames(ptg_subset) <- NULL
nrow(ptg_subset)
head(ptg_subset)

# Es guarda el data frame obtingut com un objecte de R
saveRDS(ptg_subset, "./HPOs/ptg_subset.RData")













