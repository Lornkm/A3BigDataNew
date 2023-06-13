setwd("C:/Users/loria/Documents/Cours/Big Data/A3BigDataNew")
accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)

habitants <- read.csv('Regions.csv', sep=';')
print(habitants)

habitants <- read.csv('population-departements-france.csv', sep=';')
print(habitants)

habitants <- read.csv('population-regions-france.csv', sep=';')
print(habitants)

#PRÉPARATION DES DONNÉES

#traitements données id_code_insee
accidents$id_code_insee = as.integer(accidents$id_code_insee)
#remplace les NA par la moyenne
accidents$id_code_insee[is.na(accidents$id_code_insee)] <- as.integer(mean(accidents$id_code_insee, na.rm = TRUE))
#transforme les données float en int
accidents$id_code_insee = as.integer(accidents$id_code_insee)


#traitements données age
accidents$age = as.integer(accidents$age)
accidents$age[is.na(accidents$age)] <- as.integer(mean(accidents$age, na.rm = TRUE))
accidents$age = as.integer(accidents$age)

#traitements données an_nais
accidents$an_nais = as.integer(accidents$an_nais)
accidents$an_nais[is.na(accidents$an_nais)] <- as.integer(mean(accidents$an_nais, na.rm = TRUE))
accidents$an_nais = as.integer(accidents$an_nais)


#variables multimodales en nombres
accidents$descr_cat_veh <- as.numeric(factor(accidents$descr_cat_veh))
accidents$descr_agglo <- as.numeric(factor(accidents$descr_agglo))
accidents$descr_athmo <- as.numeric(factor(accidents$descr_athmo))
accidents$descr_lum <- as.numeric(factor(accidents$descr_lum))
accidents$descr_etat_surf <- as.numeric(factor(accidents$descr_etat_surf))
accidents$description_intersection <- as.numeric(factor(accidents$description_intersection))
accidents$descr_dispo_secu <- as.numeric(factor(accidents$descr_dispo_secu))
accidents$descr_grav <- as.numeric(factor(accidents$descr_grav))
accidents$descr_motif_traj <- as.numeric(factor(accidents$descr_motif_traj))
accidents$descr_type_col <- as.numeric(factor(accidents$descr_type_col))


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


