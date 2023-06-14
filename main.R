accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)

habitants_regions <- read.csv('Regions.csv', sep=';')
habitants_departements <- read.csv('population-departements-france.csv')
link <- read.csv('link_region_dep.csv', sep=',')


# TRAITEMENTS DONNÉES

# Traitements données id_code_insee
accidents$id_code_insee = as.integer(accidents$id_code_insee)
# Transforme les données float en int
accidents$id_code_insee = as.integer(accidents$id_code_insee)


# Troncature de id_code_insee
id_code_insee_trunc = as.integer((accidents$id_code_insee)/1000)
accidents$id_code_insee_trunc <- id_code_insee_trunc

# Traitements données age
accidents$age = as.integer(accidents$age)
accidents$age[is.na(accidents$age)] <- as.integer(mean(accidents$age, na.rm = TRUE))
# Mettre le bon âge
accidents$age <- accidents$age - 14


# Traitements données an_nais
accidents$an_nais = as.integer(accidents$an_nais)
accidents$an_nais[is.na(accidents$an_nais)] <- as.integer(mean(accidents$an_nais, na.rm = TRUE))
accidents$an_nais = as.integer(accidents$an_nais)


# Traitements données place
accidents$place = as.integer(accidents$place)
# Remplace la valeur NULL par 0
accidents$place[is.na(accidents$place)] = 0
accidents$place = as.integer(accidents$place)

# Traitements données date
accidents$date <- as.POSIXct(accidents$date, format = "%Y-%m-%d %H:%M")

#variables multimodales en nombres
print(unique(accidents$descr_grav))
# Obtenir les niveaux uniques de la variable
levels <- unique(accidents$descr_grav) 
for (i in 1:length(levels)) {
  accidents$descr_grav[accidents$descr_grav == levels[i]] <- i
}
accidents$descr_grav <- as.numeric(accidents$descr_grav)

print(unique(accidents$descr_grav))
# Obtenir les niveaux uniques de la variable
levels <- unique(accidents$descr_cat_veh)
print(unique(accidents$descr_cat_veh))
for (i in 1:length(levels)) {
  accidents$descr_cat_veh[accidents$descr_cat_veh == levels[i]] <- i
}
accidents$descr_cat_veh <- as.numeric(accidents$descr_cat_veh)


# Remplacer les valeurs NULL par NA
accidents[is.null(accidents)] <- 0

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




dataframe_grav_reg <- data.frame(matrix(0, nrow = 4, ncol = length(habitants_regions$CODREG)))

for(i in 1:length(accidents$id_code_insee_trunc)){
  
  gravite = accidents$descr_grav[i]
  code_dep = accidents$id_code_insee_trunc[i]
  
  if(is.na(code_dep) == TRUE){
    code_dep=as.character("2A")
  }
  if(code_dep == 97){
    code_dep = as.integer((accidents$id_code_insee[i])/100)
  }
  
  for(j in 1:length(link$code_departement)){
    code_dep_link = link$code_departement[j]
    if(code_dep == code_dep_link){
      indice_link = which(link$code_departement == code_dep_link)
      corresp_reg = link$code_region[indice_link]
      indice_reg = which(habitants_regions$CODREG == corresp_reg)
      print(indice_reg)
      print("a")
      print(as.integer(gravite))
      dataframe_grav_reg[[indice_reg]][as.integer(gravite)] = dataframe_grav_reg[[indice_reg]][as.integer(gravite)] +1
      print(dataframe_grav_reg[[indice_reg]][as.integer(gravite)])
    }
  }
}

print(dataframe_grav_reg)


print(as.integer(54785/100))
print(class(accidents$id_code_insee_trunc))
