accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)

#transforme les données char en int

#traitements données id_code_insee
accidents$id_code_insee = as.integer(accidents$id_code_insee)
#remplace les NA par la moyenne
accidents$id_code_insee[is.na(accidents$id_code_insee)] <- as.integer(mean(accidents$id_code_insee, na.rm = TRUE))
#transforme les données float en int
accidents$id_code_insee = as.integer(accidents$id_code_insee)
print(accidents$id_code_insee)


#traitements données age
#même méthode pour l'age
accidents$age = as.integer(accidents$age)
accidents$age[is.na(accidents$age)] <- as.integer(mean(accidents$age, na.rm = TRUE))
accidents$age = as.integer(accidents$age)
print(age)

#traitements données an_nais
#même méthode pour l'année
accidents$an_nais = as.integer(accidents$an_nais)
accidents$an_nais[is.na(annee_naissance)] <- as.integer(mean(accidents$an_nais, na.rm = TRUE))
accidents$an_nais = as.integer(accidents$an_nais)
print(accidents$an_nais)



accidents$descr_cat_veh <- as.numeric(factor(accidents$descr_cat_veh))
print(accidents$descr_cat_veh[1:20])


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
