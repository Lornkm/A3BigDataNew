source('traitements_donnees.R')

# HISTOGRAMMES

# Compter le nombre d'accidents par mois
monthly_accidents <- aggregate(list(accidents = accidents$date), by = list(month = accidents$month), length)

# Compter le nombre d'accidents par semaine
weekly_accidents <- aggregate(list(accidents = accidents$date), by = list(week = accidents$week), length)



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



# Créer une variable de facteur pour la gravité avec des étiquettes
accidents$severity <- factor(accidents$descr_grav, levels = c(1, 2, 3, 4),
                             labels = c("Indemne", "Tué", "Blessé hospitalisé", "Blessé léger"))

# Calculer le nombre d'accidents par gravité
severity_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(severity = accidents$severity), FUN = length)

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



# Calculer le nombre d'accidents par ville
city_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(city = accidents$ville), FUN = length)

# Trier les données par nombre d'accidents par ordre décroissant
city_accidents <- city_accidents[order(-city_accidents$count),]

# Sélectionner les 20 villes avec le plus d'accidents
top_100_cities <- head(city_accidents, 100)

# Créer un barplot du nombre d'accidents par ville
barplot(top_100_cities$count, names.arg = top_100_cities$city,
        xlab = "Code postal", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents par ville", las = 2, cex.names = 0.8)


# Définir les groupes par tranche d'age
age_groups <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

# Créer une nouvelle colonne pour les ages
accidents$age_group <- cut(accidents$age, breaks = c(0, 17, 24, 34, 44, 54, 64, Inf), labels = age_groups, right = TRUE)

# Créer un histogramme du nombre d'accidents par tranche d'âge
hist(as.numeric(accidents$age_group), breaks = seq(0.5, length(age_groups) + 0.5),
     xaxt = "n", xlab = "Tranche d'âge", ylab = "Nombre d'accidents",
     main = "Nombre d'accidents par tranche d'âge")
# Ajouter des étiquettes personnalisées à l'axe des x
axis(1, at = seq_along(age_groups), labels = age_groups)



# Extraire le mois de la colonne de date
accidents$month <- format(accidents$date, "%m")

# Calculer le nombre d'accidents par mois
monthly_accidents <- aggregate(list(count = accidents$Num_Acc), by = list(month = accidents$month), FUN = length)

# Trier les données par mois
monthly_accidents <- monthly_accidents[order(monthly_accidents$month),]

# Créer un barplot du nombre d'accidents par mois
barplot(monthly_accidents$count, names.arg = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"),
        xlab = "Mois", ylab = "Nombre d'accidents",
        main = "Nombre d'accidents par mois", las = 1, cex.names=0.6)



# CONSTRUIRE UN JEU DE DONNEES AVEC LE NOMBRE D'ACCIDENTS SELON LA GRAVITE POUR 100.000 HABITANTS PAR REGION
# Nom du jeu de données final : dataframe_grav_reg_centmille

# Création d'un dataframe 0 de 4 lignes et 17 colonnes
dataframe_grav_reg <- data.frame(matrix(0, nrow = 4, ncol = length(habitants_regions$CODREG)))

for(i in 1:length(accidents$id_code_insee_trunc)){
  
  # Sauvegarde de la valeur de gravite et du code de département du fichier de données
  gravite = accidents$descr_grav[i]
  code_dep = accidents$id_code_insee_trunc[i]
  
  
  # Si la valeur vaut NA alors cela correspond à la Corse
  if(is.na(code_dep) == TRUE){
    code_dep=as.character("2A")
  }
  # Si la valeur vaut 97, récupérer l'id_code_insee initial pour garder les 3 premiers chiffres
  if(code_dep == 97){
    print(accidents$id_code_insee[i])
    code_dep = as.integer(as.integer(accidents$id_code_insee[i])/100)
  }
  
  for(j in 1:length(link$code_departement)){
    code_dep_link = link$code_departement[j]
    # Si les codes de département sont identiques alors
    if(code_dep == code_dep_link){
      # Récupérer l'indice du département
      indice_link = which(link$code_departement == code_dep_link)
      # Avec l'indice récupérer la région qui correspond au département
      corresp_reg = link$code_region[indice_link]
      # Récupérer l'indice de la région dans le fichier habitants_regions
      indice_reg = which(habitants_regions$CODREG == corresp_reg)
      
      # Ajouter 1 à la région et à la gravité qui correspond
      dataframe_grav_reg[[indice_reg]][as.integer(gravite)] = dataframe_grav_reg[[indice_reg]][as.integer(gravite)] +1
    }
  }
}


# Création d'un nouveau dataframe pour 100.000 habitants par région
dataframe_grav_reg_centmille <- data.frame(matrix(0, nrow = 4, ncol = length(habitants_regions$CODREG)))

for(i in 1:ncol(dataframe_grav_reg)){
  for(j in 1:nrow(dataframe_grav_reg)){
    # Pour chaque cellule, convertir pour 100.000 habitants par région
    dataframe_grav_reg_centmille[[i]][j] = as.integer((((dataframe_grav_reg[[i]][j])*100000)/habitants_regions$PTOT[i]))
  }
}

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



################################################################################################

library(leaflet)
library(maps)
library(dplyr)

france_map <- map("france", fill = TRUE, col = "transparent", plot = FALSE)

# Extract department code from id_code_insee column
accidents$department <- substr(accidents$id_code_insee, 1, nchar(accidents$id_code_insee) - 3)

# Read department mapping file
department_map <- read.csv("link_region_dep.csv", stringsAsFactors = FALSE)

# Create named vector to map department codes to names
department_map <- setNames(department_map$nom_departement, department_map$code_departement)

# Update department column with department names
accidents$department <- department_map[accidents$department]

unique(accidents$descr_grav)



# Count number of accidents by department
accidents_by_department <- accidents %>%
  group_by(department) %>%
  summarise(nombre_accidents = n(),
            nombre_accidents_graves = sum(descr_grav %in% c("2", "4")),
            part_accidents_graves = nombre_accidents_graves / nombre_accidents)
print(accidents_by_department, n=1000)

# Create custom color palette for proportion of severe accidents
colors <- c("green", "yellowgreen", "yellow", "orange", "orangered", "red")

# Assign colors to each department based on proportion of severe accidents
department_colors <- accidents_by_department %>%
  mutate(color = cut(part_accidents_graves,
                     breaks = c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, Inf),
                     labels = colors))
department_colors

department_colors$department <- iconv(department_colors$department, to = "ASCII//TRANSLIT")
department_colors$department <- gsub("'", "", department_colors$department)
department_colors$department <- tolower(department_colors$department)

france_map$names <- iconv(france_map$names, to = "ASCII//TRANSLIT")
france_map$names <- gsub("'", "", france_map$names)
france_map$names <- tolower(france_map$names)

department_colors$label <- paste(department_colors$department, round(department_colors$part_accidents_graves * 100), "%", sep = " : ")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = france_map,
              fillColor = department_colors$color[match(france_map$names, department_colors$department)],
              fillOpacity = 0.7,
              label = ~paste(france_map$names, round(department_colors$part_accidents_graves[match(france_map$names, department_colors$department)] * 100), "%", sep = " : "),
              color = "white",
              weight = 1) %>%
  addLegend(position = "bottomright",
            title = "Part d'accidents graves",
            colors = colors,
            labels = c("<10%", "10-20%", "20-30%", "30-40%", "40-50%", ">50%"))



################################################################################################


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
  summarise(nombre_accidents = n(),
            nombre_accidents_graves = sum(descr_grav %in% c("2", "4")),
            part_accidents_graves = nombre_accidents_graves / nombre_accidents)

accidents_by_dep <- accidents %>%
  group_by(department) %>%
  summarise(region = first(region)) %>%
  left_join(accidents_by_region, by = "region")

# Ajouter une colonne des noms des départements dans le tableau accidents_by_dep
department_map <- read.csv("link_region_dep.csv", stringsAsFactors = FALSE)
department_map <- setNames(department_map$nom_departement, department_map$code_departement)

accidents_by_dep$department_name <- department_map[accidents_by_dep$department]

colors <- c("green", "yellow", "orange", "orangered", "red")

# Assigner des couleurs à chaque région en fonction du nombre d'accidents
region_colors <- accidents_by_dep %>%
  mutate(color = cut(part_accidents_graves,
                     breaks = c(-Inf, 0.3, 0.35, 0.4, 0.45, Inf),
                     labels = colors))

region_colors$department_name <- iconv(region_colors$department_name, to = "ASCII//TRANSLIT")
region_colors$department_name <- gsub("'", "", region_colors$department_name)
region_colors$department_name <- tolower(region_colors$department_name)

france_map$names <- iconv(france_map$names, to = "ASCII//TRANSLIT")
france_map$names <- gsub("'", "", france_map$names)
france_map$names <- tolower(france_map$names)

region_colors$label <- paste(region_colors$region, "(", region_colors$department_name, ")", round(region_colors$part_accidents_graves * 100), "%", sep = " ")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = france_map,
              fillColor = region_colors$color[match(france_map$names, region_colors$department_name)],
              fillOpacity = 0.7,
              label = ~region_colors$label[match(france_map$names, region_colors$department_name)],
              color = "white",
              weight = 1) %>%
  addLegend(position = "bottomright",
            title = "Part d'accidents graves",
            colors = colors,
            labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%"))








