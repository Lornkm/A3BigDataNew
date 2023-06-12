accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)


print(accidents$ville)

#traitements données id_code_insee
id_code_insee = accidents$id_code_insee
#transforme les données char en int
id_code_insee = as.integer(id_code_insee)
#remplace les NA par la moyenne
id_code_insee[is.na(id_code_insee)] <- as.integer(mean(id_code_insee, na.rm = TRUE))
#transforme les données float en int
id_code_insee = as.integer(id_code_insee)
print(id_code_insee)
print(class(id_code_insee))


#traitements données age
#même méthode pour l'age
age = accidents$age
age = as.integer(age)
age[is.na(age)] <- as.integer(mean(age, na.rm = TRUE))
age = as.integer(age)
print(age)

#traitements données an_nais
#même méthode pour l'année
annee_naissance = accidents$an_nais
annee_naissance = as.integer(annee_naissance)
annee_naissance[is.na(annee_naissance)] <- as.integer(mean(annee_naissance, na.rm = TRUE))
annee_naissance = as.integer(annee_naissance)
print(annee_naissance)


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
