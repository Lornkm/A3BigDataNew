accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)


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


# Create a new column for month and week
accidents$month <- format(accidents$date, "%Y-%m")
accidents$week <- strftime(accidents$date, format = "%Y-%U")


# Count the number of accidents per month
monthly_accidents <- aggregate(list(accidents = accidents$date), by = list(month = accidents$month), length)
print(monthly_accidents)

# Count the number of accidents per week
weekly_accidents <- aggregate(list(accidents = accidents$date), by = list(week = accidents$week), length)
print(weekly_accidents)



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



#install.packages(c("leaflet", "dplyr"))
library(leaflet)
library(maps)
library(dplyr)

france_map <- map("france", fill = TRUE, col = "transparent", plot = FALSE)

# Extraire le code du département de la colonne id_code_insee
accidents$department <- substr(accidents$id_code_insee, 1, nchar(accidents$id_code_insee) - 3)

# Lire le fichier de mappage des départements
department_map <- read.csv("link_region_dep.csv", stringsAsFactors = FALSE)

# Créer un vecteur nommé pour mapper les codes des départements aux noms
department_map <- setNames(department_map$nom_departement, department_map$code_departement)

# Mettre à jour la colonne department avec les noms des départements
accidents$department <- department_map[accidents$department]

# Compter le nombre d'accidents par département
accidents_by_department <- accidents %>%
  group_by(department) %>%
  summarise(nombre_accidents = n())
  print(accidents_by_department, n=1000)

# Créer une palette de couleurs personnalisée pour le nombre d'accidents
colors <- c("orange", "red", "green", "blue", "purple", "black")

# Assigner des couleurs à chaque département en fonction du nombre d'accidents
department_colors <- accidents_by_department %>%
  mutate(color = cut(nombre_accidents,
                     breaks = c(-Inf, 500, 1000, 1500, 2000, 2500, Inf),
                     labels = colors))

print(department_colors)

print(france_map$names)



department_colors$department <- iconv(department_colors$department, to = "ASCII//TRANSLIT")
department_colors$department <- gsub("'", "", department_colors$department)
department_colors$department <- tolower(department_colors$department)

france_map$names <- iconv(france_map$names, to = "ASCII//TRANSLIT")
france_map$names <- gsub("'", "", france_map$names)
france_map$names <- tolower(france_map$names)

department_colors$label <- paste(department_colors$department, department_colors$nombre_accidents, sep = " : ")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = france_map,
              fillColor = department_colors$color[match(france_map$names, department_colors$department)],
              fillOpacity = 0.7,
              label = ~paste(france_map$names, department_colors$nombre_accidents[match(france_map$names, department_colors$department)], sep = " : "),
              color = "white",
              weight = 1) %>%
  addLegend(position = "bottomright",
            title = "Nombre d'accidents",
            colors = colors,
            labels = c("<500", "500-1000", "1000-1500", "1500-2000", "2000-2500", ">2500"))


##########################################################################################

# Installer et charger les packages nécessaires
library(maps)
library(leaflet)
library(dplyr)

france_map <- map("france", fill = TRUE, col = "transparent", plot = FALSE)

# Extraire le code du département de la colonne id_code_insee
accidents$department <- substr(accidents$id_code_insee, 1, nchar(accidents$id_code_insee) - 3)

# Lire le fichier de mappage des régions
region_map <- read.csv("link_region_dep.csv", stringsAsFactors = FALSE)

# Créer un vecteur nommé pour mapper les codes des départements aux noms des régions
region_map <- setNames(region_map$nom_region, region_map$code_departement)

# Mettre à jour la colonne department avec les noms des régions
accidents$region <- region_map[accidents$department]

# Compter le nombre d'accidents par région
accidents_by_region <- accidents %>%
  group_by(region) %>%
  summarise(nombre_accidents = n())

accidents_by_dep <- accidents %>%
  group_by(department) %>%
  summarise(region = first(region)) %>%
  left_join(accidents_by_region, by = "region")

# Ajouter une colonne des noms des départements dans le tableau accidents_by_dep
department_map <- read.csv("link_region_dep.csv", stringsAsFactors = FALSE)
department_map <- setNames(department_map$nom_departement, department_map$code_departement)

accidents_by_dep$department_name <- department_map[accidents_by_dep$department]

colors <- c("orange", "red", "green", "blue", "purple", "black")

# Assigner des couleurs à chaque région en fonction du nombre d'accidents
region_colors <- accidents_by_dep %>%
  mutate(color = cut(nombre_accidents,
                     breaks = c(-Inf, 3000, 6000, 9000, 12000, 15000, Inf),
                     labels = colors))
print(region_colors)



region_colors$department_name <- iconv(region_colors$department_name, to = "ASCII//TRANSLIT")
region_colors$department_name <- gsub("'", "", region_colors$department_name)
region_colors$department_name <- tolower(region_colors$department_name)

france_map$names <- iconv(france_map$names, to = "ASCII//TRANSLIT")
france_map$names <- gsub("'", "", france_map$names)
france_map$names <- tolower(france_map$names)

region_colors$label <- paste(region_colors$region, "(", region_colors$department_name, ")", region_colors$nombre_accidents, sep = " ", "accidents")


leaflet() %>%
  addTiles() %>%
  addPolygons(data = france_map,
              fillColor = region_colors$color[match(france_map$names, region_colors$department_name)],
              label = ~region_colors$label[match(france_map$names, region_colors$department_name)],
              fillOpacity = 0.7,
              color = "white",
              weight = 1) %>%
  addLegend(position = "bottomright",
            title = "Nombre d'accidents",
            colors = colors,
            labels = c("<3000", "3000-6000", "6000-9000", "9000-12000", "12000-15000", ">15000"))





