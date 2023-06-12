
accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)
accidents$descr_cat_veh <- as.numeric(factor(accidents$descr_cat_veh))
accidents$ville <- as.numeric(factor(accidents$ville))
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
for (i in seq_along(col_means)) {
  col_name <- names(col_means)[i]
    accidents[is.na(accidents[, col_name]), col_name] <- col_means[i]
}