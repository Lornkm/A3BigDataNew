
accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)
accidents$descr_cat_veh <- as.numeric(factor(accidents$descr_cat_veh))
accidents$ville <- as.numeric(factor(accidents$ville))
print(accidents$descr_cat_veh[1:20])





# Remplacer les valeurs NULL par NA
accidents[is.null(accidents)] <- NA

# Identifier les colonnes numériques
num_cols <- sapply(accidents, is.numeric)

# Calculer la moyenne de chaque colonne numérique en ignorant les valeurs manquantes
col_means <- colMeans(accidents[, num_cols], na.rm = TRUE)

# Imputer les valeurs manquantes par la moyenne de chaque colonne numérique
for (i in seq_along(col_means)) {
  col_name <- names(col_means)[i]
    accidents[is.na(accidents[, col_name]), col_name] <- col_means[i]
}