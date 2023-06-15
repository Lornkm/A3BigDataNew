source('main.R')

# Importation des données du fichier arrondissement et main
source('arrondissement_PARIS_LYON_MARSEILLE.R')

# TRAITEMENTS DONNÉES

# Mettre les valeurs sous format adéquat

# Traitements données id_code_insee
accidents$id_code_insee[i] = as.integer(accidents$id_code_insee[i])

# accidents$id_code_insee = as.integer(accidents$id_code_insee)

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

# Nouvelle colonne : troncature de id_code_insee
id_code_insee_trunc = as.integer((accidents$id_code_insee)/1000)
accidents$id_code_insee_trunc <- id_code_insee_trunc

# Nouvelle colonne : mois et année
accidents$month <- format(accidents$date, "%Y-%m")
accidents$week <- strftime(accidents$date, format = "%Y-%U")

# Remplacer les valeurs NULL par 0 au cas où
accidents[is.null(accidents)] <- 0



# Remplacer les coordonnées pour les villes avec des arrrondissements


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



# Variables multimodales en nombres


# Obtenir les niveaux uniques de la variable
levels <- unique(accidents$descr_grav) 
for (i in 1:length(levels)) {
  accidents$descr_grav[accidents$descr_grav == levels[i]] <- i
}
accidents$descr_grav <- as.numeric(accidents$descr_grav)


# Obtenir les niveaux uniques de la variable
levels <- unique(accidents$descr_cat_veh)
print(unique(accidents$descr_cat_veh))
for (i in 1:length(levels)) {
  accidents$descr_cat_veh[accidents$descr_cat_veh == levels[i]] <- i
}
accidents$descr_cat_veh <- as.numeric(accidents$descr_cat_veh)




