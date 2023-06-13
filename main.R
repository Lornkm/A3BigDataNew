accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)

accidents$date <- as.POSIXct(accidents$date, format = "%Y-%m-%d %H:%M")

#transforme les données char en int

#traitements données id_code_insee
accidents$id_code_insee = as.integer(accidents$id_code_insee)
#remplace les NA par la moyenne
accidents$id_code_insee[is.na(accidents$id_code_insee)] <- as.integer(mean(accidents$id_code_insee, na.rm = TRUE))
#transforme les données float en int
accidents$id_code_insee = as.integer(accidents$id_code_insee)
#print(accidents$id_code_insee)


#traitements données age
accidents$age = as.integer(accidents$age)
accidents$age[is.na(accidents$age)] <- as.integer(mean(accidents$age, na.rm = TRUE))
accidents$age = as.integer(accidents$age)
#print(age)

#traitements données an_nais
accidents$an_nais = as.integer(accidents$an_nais)
accidents$an_nais[is.na(accidents$an_nais)] <- as.integer(mean(accidents$an_nais, na.rm = TRUE))
accidents$an_nais = as.integer(accidents$an_nais)
#print(accidents$an_nais)

#traitements données place
accidents$place = as.integer(accidents$place)
accidents$place[is.na(accidents$place)] <- as.integer(mean(accidents$place, na.rm = TRUE))
accidents$place = as.integer(accidents$place)
#print(accidents$an_nais)


#variables multimodales en nombres
print(unique(accidents$descr_grav))
# Obtenir les niveaux uniques de la variable
levels <- unique(accidents$descr_grav)  # Obtient les niveaux uniques de la variable
for (i in 1:length(levels)) {
  accidents$descr_grav[accidents$descr_grav == levels[i]] <- i
}
print(unique(accidents$descr_grav))
# Obtenir les niveaux uniques de la variable
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
for (i in seq_along(col_means)) {
  col_name <- names(col_means)[i]
    accidents[is.na(accidents[, col_name]), col_name] <- col_means[i]
}

#print(accidents$place)


# Mettre le bon âge
print(accidents$age[1:20])
accidents$age <- accidents$age - 14
print(accidents$age[1:20])

# Liste des longitudes des arrondissements de Paris
longitudes_paris <- c(
  "PARIS 01" = 2.336293, "PARIS 02" = 2.344107, "PARIS 03" = 2.359361, "PARIS 04" = 2.354313, "PARIS 05" = 2.350498,
  "PARIS 06" = 2.331834, "PARIS 07" = 2.312376, "PARIS 08" = 2.318825, "PARIS 09" = 2.337460, "PARIS 10" = 2.360728,
  "PARIS 11" = 2.378985, "PARIS 12" = 2.419807, "PARIS 13" = 2.362272, "PARIS 14" = 2.326062, "PARIS 15" = 2.297116,
  "PARIS 16" = 2.260346, "PARIS 17" = 2.307485, "PARIS 18" = 2.348161, "PARIS 19" = 2.382815, "PARIS 20" = 2.400237
)

# Liste des latitudes des arrondissements de Paris
latitudes_paris <- c(
  "PARIS 01" = 48.862630, "PARIS 02" = 48.867903, "PARIS 03" = 48.863054, "PARIS 04" = 48.854228, "PARIS 05" = 48.844406,
  "PARIS 06" = 48.848968, "PARIS 07" = 48.856082, "PARIS 08" = 48.872527, "PARIS 09" = 48.876896, "PARIS 10" = 48.876132,
  "PARIS 11" = 48.858416, "PARIS 12" = 48.835156, "PARIS 13" = 48.828388, "PARIS 14" = 48.829567, "PARIS 15" = 48.841807,
  "PARIS 16" = 48.860642, "PARIS 17" = 48.887337, "PARIS 18" = 48.892569, "PARIS 19" = 48.882777, "PARIS 20" = 48.859059
)

# Liste des longitudes des arrondissements de Marseille
longitudes_marseille <- c(
  "MARSEILLE 01" = 5.379781, "MARSEILLE 02" = 5.377063, "MARSEILLE 03" = 5.366441, "MARSEILLE 04" = 5.401624, "MARSEILLE 05" = 5.407209,
  "MARSEILLE 06" = 5.390855, "MARSEILLE 07" = 5.363502, "MARSEILLE 08" = 5.370926, "MARSEILLE 09" = 5.383839, "MARSEILLE 10" = 5.367585,
  "MARSEILLE 11" = 5.396632, "MARSEILLE 12" = 5.420401, "MARSEILLE 13" = 5.431525, "MARSEILLE 14" = 5.393014, "MARSEILLE 15" = 5.353567,
  "MARSEILLE 16" = 5.283032
)

# Liste des latitudes des arrondissements de Marseille
latitudes_marseille <- c(
  "MARSEILLE 01" = 43.296174, "MARSEILLE 02" = 43.309610, "MARSEILLE 03" = 43.311040, "MARSEILLE 04" = 43.315924, "MARSEILLE 05" = 43.309362,
  "MARSEILLE 06" = 43.294650, "MARSEILLE 07" = 43.282942, "MARSEILLE 08" = 43.267554, "MARSEILLE 09" = 43.254660, "MARSEILLE 10" = 43.273957,
  "MARSEILLE 11" = 43.282455, "MARSEILLE 12" = 43.307135, "MARSEILLE 13" = 43.347762, "MARSEILLE 14" = 43.382531, "MARSEILLE 15" = 43.366408,
  "MARSEILLE 16" = 43.362874
)

# Liste des longitudes des arrondissements de Lyon
longitudes_lyon <- c(
  "LYON 01" = 4.835570, "LYON 02" = 4.826008, "LYON 03" = 4.843208, "LYON 04" = 4.829523, "LYON 05" = 4.826172,
  "LYON 06" = 4.874227, "LYON 07" = 4.844024, "LYON 08" = 4.889882, "LYON 09" = 4.805214
)

# Liste des latitudes des arrondissements de Lyon
latitudes_lyon <- c(
  "LYON 01" = 45.767290, "LYON 02" = 45.754732, "LYON 03" = 45.763754, "LYON 04" = 45.771109, "LYON 05" = 45.759888,
  "LYON 06" = 45.771568, "LYON 07" = 45.746302, "LYON 08" = 45.729645, "LYON 09" = 45.757429
)


# Remplacement des longitudes de la ville de Paris
accidents$longitude <- ifelse(accidents$ville %in% names(longitudes_paris), longitudes_paris[accidents$ville], accidents$longitude)
# Remplacement des latitudes de la ville de Paris
accidents$latitude <- ifelse(accidents$ville %in% names(latitudes_paris), latitudes_paris[accidents$ville], accidents$latitude)

# Remplacement des longitudes des villes de Marseille
accidents$longitude <- ifelse(accidents$ville %in% names(longitudes_marseille), longitudes_marseille[accidents$ville], accidents$longitude)
# Remplacement des latitudes des villes de Marseille
accidents$latitude <- ifelse(accidents$ville %in% names(latitudes_marseille), latitudes_marseille[accidents$ville], accidents$latitude)

# Remplacement des longitudes des villes de Lyon
accidents$longitude <- ifelse(accidents$ville %in% names(longitudes_lyon), longitudes_lyon[accidents$ville], accidents$longitude)
# Remplacement des latitudes des villes de Lyon
accidents$latitude <- ifelse(accidents$ville %in% names(latitudes_lyon), latitudes_lyon[accidents$ville], accidents$latitude)



# Calculer le nombre d'accidents par conditions atmosphériques
atmospheric_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(atmospheric = accidents$descr_athmo), FUN = length)

# Créer un graphique à barres pour les accidents par conditions atmosphériques
barplot(atmospheric_accidents$count, names.arg = atmospheric_accidents$atmospheric,
        xlab = "Conditions atmosphériques", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents en fonction des conditions atmosphériques")



# Calculer le nombre d'accidents par description de la surface
surface_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(surface = accidents$descr_etat_surf), FUN = length)

# Créer un graphique à barres pour les accidents par description de la surface
barplot(surface_accidents$count, names.arg = surface_accidents$surface,
        xlab = "Description de la surface", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents en fonction de la description de la surface")



# Calculer le nombre d'accidents par gravité
severity_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(severity = accidents$descr_grav), FUN = length)

# Créer un graphique à barres pour les accidents par gravité
barplot(severity_accidents$count, names.arg = severity_accidents$severity,
        xlab = "Gravité", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents selon la gravité")





# Extraire l'heure de la colonne date
accidents$hour <- as.numeric(format(accidents$date, "%H"))+1

# Calculer le nombre d'accidents par heure
hourly_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(hour = accidents$hour), FUN = length)

# Créer un graphique à barres pour les accidents par heure
barplot(hourly_accidents$count, names.arg = hourly_accidents$hour,
        xlab = "Heure", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents par tranches d'heure")



# Calculate the number of accidents by city
city_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(city = accidents$ville), FUN = length)

# Order the data by the number of accidents in descending order
city_accidents <- city_accidents[order(-city_accidents$count),]

# Select the top 20 cities with the most accidents
top_100_cities <- head(city_accidents, 100)

# Create a barplot of the number of accidents by city
barplot(top_100_cities$count, names.arg = top_100_cities$city,
        xlab = "Code postal", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents par ville", las = 2, cex.names = 0.8)








