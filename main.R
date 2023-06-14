setwd("C:/Users/loria/Documents/Cours/Big Data/A3BigDataNew")
accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)
habitants_regions <- read.csv('Regions.csv', sep=';')
habitants_departements <- read.csv('population-departements-france.csv')
link <- read.csv('link_region_dep.csv', sep=',')


#PRÉPARATION DES DONNÉES

# Traitements données id_code_insee
accidents$id_code_insee = as.integer(accidents$id_code_insee)
# Remplace les NA par la moyenne
accidents$id_code_insee[is.na(accidents$id_code_insee)] <- as.integer(mean(accidents$id_code_insee, na.rm = TRUE))
# Transforme les données float en int
accidents$id_code_insee = as.integer(accidents$id_code_insee)


# Traitements données age
accidents$age = as.integer(accidents$age)
accidents$age[is.na(accidents$age)] <- as.integer(mean(accidents$age, na.rm = TRUE))
accidents$age = as.integer(accidents$age)

# Traitements données an_nais
accidents$an_nais = as.integer(accidents$an_nais)
accidents$an_nais[is.na(accidents$an_nais)] <- as.integer(mean(accidents$an_nais, na.rm = TRUE))
accidents$an_nais = as.integer(accidents$an_nais)

# Troncature de id_code_insee
accidents$id_code_insee = floor((accidents$id_code_insee)/1000)

# Variables multimodales en nombres

# Obtient les niveaux uniques de la variable
print(unique(accidents$descr_grav))
levels <- unique(accidents$descr_grav)  
for (i in 1:length(levels)) {
  accidents$descr_grav[accidents$descr_grav == levels[i]] <- i
}
print(unique(accidents$descr_grav))

# Obtient les niveaux uniques de la variable
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
accidents_imputés <- accidents
for (i in seq_along(col_means)) {
  col_name <- names(col_means)[i]
  accidents_imputés[is.na(accidents_imputés[, col_name]), col_name] <- col_means[i]
}



# VISUALISATION DES DONNÉES

dataframe_grav_dep <- data.frame(matrix(0, nrow = 4, ncol = 101))
for(code_dep in link$code_departement){ 
  print("a")
  dep_grav <- c(0, 0, 0, 0)
  print("b")
  for (j in 1:length(accidents$id_code_insee)){
    if(code_dep == accidents$id_code_insee[j]){
      print("d")
      gravite = accidents$descr_grav[j]
      print("e")
      dep_grav[gravite] = dep_grav[gravite] + 1
      
    }
  }
  
  dataframe_grav_dep[code_dep] <- vecteur
}
print(dataframe_grav_dep)
dataframe_grav_dep <- dataframe_grav_dep[, -c(2, 3, 4, 5, 6, 7, 8, 9,10)]

vect_test <- rep(0, time = length(accidents$id_code_insee))
compteur = 1
for(code_dep in accidents$id_code_insee){
  for(code_dep_link in link$code_departement){
    if(code_dep_link == code_dep){
      indice = which(link$code_departement == code_dep_link)
      vect_test[compteur] = link$code_region[indice]
      compteur = compteur +1
    }
  }
}

accidents$regions <- vect_test


dataframe_grav_reg <- data.frame(matrix(0, nrow = 4, ncol = length(habitants_regions$CODREG)))

for(i in 1:length(accidents$id_code_insee)){
  gravite = accidents$descr_grav[i]
  code_dep = accidents$id_code_insee[i]
  for(j in 1:length(link$code_departement)){
    code_dep_link = link$code_departement[j]
    if(code_dep == 972 || code_dep == 971 || code_dep == 973){
      
    }
    if(code_dep == code_dep_link){
      indice_link = which(link$code_departement == code_dep_link)
      corresp_reg = link$code_region[indice_link]
      indice_reg = which(habitants_regions$CODREG == corresp_reg)
      dataframe_grav_reg[[indice_reg]][as.integer(gravite)] = dataframe_grav_reg[[indice_reg]][as.integer(gravite)] +1
    
    }
  }
}

print(dataframe_grav_reg)




