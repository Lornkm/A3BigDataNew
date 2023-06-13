setwd("C:/Users/loria/Documents/Cours/Big Data/A3BigDataNew")
accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)

habitants_regions <- read.csv('Regions.csv', sep=';')
habitants_departements <- read.csv('population-departements-france.csv', sep=';')


#PRÉPARATION DES DONNÉES

# Traitements données id_code_insee
accidents$id_code_insee = as.integer(accidents$id_code_insee)
# Remplace les NA par la moyenne
accidents$id_code_insee[is.na(accidents$id_code_insee)] <- as.integer(mean(accidents$id_code_insee, na.rm = TRUE))
# Transforme les données float en int
accidents$id_code_insee = as.integer(accidents$id_code_insee)


# Traitements données age
accidents$age = as.integer(accidents$age)
accidents$age[is.na(accidents$age)] <- as.integer(mean(accidents$age, na.rm = TRUE))
accidents$age = as.integer(accidents$age)

# Traitements données an_nais
accidents$an_nais = as.integer(accidents$an_nais)
accidents$an_nais[is.na(accidents$an_nais)] <- as.integer(mean(accidents$an_nais, na.rm = TRUE))
accidents$an_nais = as.integer(accidents$an_nais)


# Variables multimodales en nombres

# Obtient les niveaux uniques de la variable
print(unique(accidents$descr_grav))
levels <- unique(accidents$descr_grav)  
for (i in 1:length(levels)) {
  accidents$descr_grav[accidents$descr_grav == levels[i]] <- i
}
print(unique(accidents$descr_grav))

# Obtient les niveaux uniques de la variable
levels <- unique(accidents$descr_cat_veh)  
print(unique(accidents$descr_cat_veh))
for (i in 1:length(levels)) {
  accidents$descr_cat_veh[accidents$descr_cat_veh == levels[i]] <- i
}
print(unique(accidents$descr_cat_veh))


# Remplacer les valeurs NULL par NA
accidents[is.null(accidents)] <- NA
# Identifier les colonnes numériques
num_cols <- sapply(accidents, is.numeric)
# Calculer la moyenne de chaque colonne numérique en ignorant les valeurs manquantes
col_means <- colMeans(accidents[, num_cols], na.rm = TRUE)
# Imputer les valeurs manquantes par la moyenne de chaque colonne numérique
accidents_imputés <- accidents
for (i in seq_along(col_means)) {
  col_name <- names(col_means)[i]
  accidents_imputés[is.na(accidents_imputés[, col_name]), col_name] <- col_means[i]
}



# VISUALISATION DES DONNÉES


