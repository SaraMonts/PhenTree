# S'ha descarregat l'arxiu hp.obo de la pàgina web de Human Phenotype Ontology.
# Mitjançant gawk s'ha filtrat l'arxiu hp.obo per a quedar-nos només el ID i el nom de cada terme
# i s'ha guardat en l'arxiu hp_filtered.txt.
# Es carrega aquest arxiu a R
hpo <- read.table("./HPOs/hp_filtered.txt", sep = "_", quote = "")

# El data frame que s'obté està format per una sola columna en la que les files senars són els
# ID dels termes i les files parell són els seus corresponents noms
head(hpo)
nrow(hpo)

# Per a separar els ID i els noms en dues columnes diferents es generen dos nous vectors
# en els quals es van afegint els ID o noms segons correspongui, eliminant els prefixos "id: "
# i "name: "
id <- c()
name <- c()
for (i in 1:nrow(hpo)) {
  if (i %% 2 != 0) id <- c(id, substring(as.character(hpo$V1[i]), 5))
  else if (i %% 2 == 0) name <- c(name, substring(as.character(hpo$V1[i]), 7))
}

# Es combinen els dos vectors en un data frame
hpo.df <- data.frame(ID = id, Name = name)
head(hpo.df)
tail(hpo.df)
nrow(hpo.df)

# S'eliminen tots aquells termes en que el seu nom comença per la paraula "obsolete"
library(gdata)
n <- which(startsWith(as.character(hpo.df$Name), "obsolete"))
hpo.df <- hpo.df[-n,]
nrow(hpo.df)

# Aquest data frame es converteix en una llista amb el següent format:
#   -  Nom: nom del terme (ID)
#   -  Valor: ID
hpo_list <- list()
for (i in 1:nrow(hpo.df)) {
  name <- paste0(as.character(hpo.df$Name[i]), " (", as.character(hpo.df$ID[i]), ")")
  id <- as.character(hpo.df$ID[i])
  hpo_list[[name]] <- id
}
head(hpo_list)

# S'ha vist que l'aplicació triga en obrir-se degut a que la llista és molt llarga i triga en
# carregar-se. Per a evitar-ho, es selecciona aleatòriament un subset de només 5000 termes a 
# mode de "proof of concept". El primer terme anomenat "All" i amb ID "HP:0000001" es vol mantenir segur
# per a que sigui el valor per defecte
set.seed(010516)
n <- sample(2:length(hpo_list), 4999, replace = FALSE)
n <- c(1, sort(n))
hpo_list_subset <- hpo_list[n]
length(hpo_list_subset)
head(hpo_list_subset)

# Es guarda la llista reduïda com a un objecte R
saveRDS(hpo_list_subset, file = "./HPOs/hpo_list_subset.RData")





