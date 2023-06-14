source('traitements_donnees.R')

# HISTOGRAMMES

# Compter le nombre d'accidents par mois
monthly_accidents <- aggregate(list(accidents = accidents$date), by = list(month = accidents$month), length)
print(monthly_accidents)

# Compter le nombre d'accidents par semaine
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
    code_dep = as.integer((accidents$id_code_insee[i])/100)
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

print(dataframe_grav_reg)

# Création d'un nouveau dataframe pour 100.000 habitants par région
dataframe_grav_reg_centmille <- data.frame(matrix(0, nrow = 4, ncol = length(habitants_regions$CODREG)))

for(i in 1:ncol(dataframe_grav_reg)){
  for(j in 1:nrow(dataframe_grav_reg)){
    # Pour chaque cellule, convertir pour 100.000 habitants par région
    dataframe_grav_reg_centmille[[i]][j] = as.integer((((dataframe_grav_reg[[i]][j])*100000)/habitants_regions$PTOT[i]))
  }
}






