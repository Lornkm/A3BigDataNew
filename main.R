accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)

<<<<<<< HEAD

accidents$date <- as.POSIXct(accidents$date, format = "%Y-%m-%d %H:%M")

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
levels <- unique(accidents$descr_grav) 
for (i in 1:length(levels)) {
  accidents$descr_grav[accidents$descr_grav == levels[i]] <- i
}
accidents$descr_grav <- as.numeric(accidents$descr_grav)
#print(unique(accidents$descr_grav))

# Obtenir les niveaux uniques de la variable
levels <- unique(accidents$descr_cat_veh)
print(unique(accidents$descr_cat_veh))
for (i in 1:length(levels)) {
  accidents$descr_cat_veh[accidents$descr_cat_veh == levels[i]] <- i
}
accidents$descr_cat_veh <- as.numeric(accidents$descr_cat_veh)

#print(unique(accidents$descr_cat_veh))


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


<<<<<<< HEAD
# Create a new column for month and week
accidents$month <- format(accidents$date, "%Y-%m")
accidents$week <- strftime(accidents$date, format = "%Y-%U")

# Count the number of accidents per month
monthly_accidents <- aggregate(list(accidents = accidents$date), by = list(month = accidents$month), length)
print(monthly_accidents)

# Count the number of accidents per week
weekly_accidents <- aggregate(list(accidents = accidents$date), by = list(week = accidents$week), length)
print(weekly_accidents)

=======
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
>>>>>>> 399d66b7f2335f19e396c403a66ea22cf285b2a9


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



# Create a factor variable for severity with labels
accidents$severity <- factor(accidents$descr_grav, levels = c(1, 2, 3, 4),
                             labels = c("Indemne", "Tué", "Blessé hospitalisé", "Blessé léger"))

# Calculate the number of accidents by severity
severity_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(severity = accidents$severity), FUN = length)

# Create a bar chart for accidents by severity
barplot(severity_accidents$count, names.arg = severity_accidents$severity,
        xlab = "Gravité", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents selon la gravité")



<<<<<<< HEAD

=======
>>>>>>> 399d66b7f2335f19e396c403a66ea22cf285b2a9
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


<<<<<<< HEAD



# Define the age groups
age_groups <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

# Create a new column for the age group
accidents$age_group <- cut(accidents$age, breaks = c(0, 17, 24, 34, 44, 54, 64, Inf), labels = age_groups, right = TRUE)

# Create a histogram of the number of accidents by age group
hist(as.numeric(accidents$age_group), breaks = seq(0.5, length(age_groups) + 0.5),
     xaxt = "n", xlab = "Tranche d'âge", ylab = "Nombre d'accidents",
     main = "Nombre d'accidents par tranche d'âge")

# Add custom labels to the x-axis
axis(1, at = seq_along(age_groups), labels = age_groups)



# Extract the month from the date column
accidents$month <- format(accidents$date, "%m")

# Calculate the number of accidents by month
monthly_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(month = accidents$month), FUN = length)

# Order the data by month
monthly_accidents <- monthly_accidents[order(monthly_accidents$month),]

# Create a barplot of the number of accidents by month
barplot(monthly_accidents$count, names.arg = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"),
        xlab = "Mois", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents par mois", las = 1, cex.names=0.6)



# Load the necessary libraries
library(leaflet)

# Extract the department number from the id_code_insee column
accidents$department <- substr(accidents$id_code_insee, 1, 2)

# Count the number of accidents by department
accidents_by_department <- aggregate(list(accidents = accidents$date), by = list(department = accidents$department), length)

# Create a color palette for the number of accidents
colors <- colorFactor(palette = "Reds", domain = accidents_by_department$accidents)

# Create a leaflet map
leaflet() %>%
  addTiles() %>%
  addCircles(data = accidents_by_department, lat = ~latitude, lng = ~longitude,
             color = ~colors(accidents), radius = 5000,
             label = ~paste(department, accidents))


#leaflet
#fichier final : ajout numéro dep, reg,
#rapport, pres en pdf, code R, CSV


=======
>>>>>>> 399d66b7f2335f19e396c403a66ea22cf285b2a9

# Définir les limites des groupes d'âge
limits <- c(0, 20, 40, 60, 80, 100, Inf)

# Appliquer la découpe et créer une nouvelle variable "age_group" regroupant les âges
accidents$age_group <- cut(accidents$age, breaks = limits, labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100+"), right = FALSE)
# Afficher les premières lignes pour vérification
print(accidents$age_group[1:20])

# Test du chi2 sur descr_grav
tableau_croise <- table(accidents$descr_grav, accidents$age_group)
print(tableau_croise)
result_chi2 <- chisq.test(tableau_croise)
# Afficher les résultats du test
print(result_chi2)
mosaicplot(tableau_croise)