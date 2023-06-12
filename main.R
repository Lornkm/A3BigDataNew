accidents = read.csv('stat_acc_V3.csv', sep=";")

print(accidents$ville)

#traitements données id_code_insee
id_code_insee = accidents$id_code_insee
#transforme les données char en int
id_code_insee = as.integer(id_code_insee)
#remplace les NA par la moyenne
id_code_insee[is.na(id_code_insee)] <- as.integer(mean(id_code_insee, na.rm = TRUE))
#transforme les données float en int
id_code_insee = as.integer(id_code_insee)
print(id_code_insee)
print(class(id_code_insee))


#traitements données age
#même méthode pour l'age
age = accidents$age
age = as.integer(age)
age[is.na(age)] <- as.integer(mean(age, na.rm = TRUE))
age = as.integer(age)
print(age)

#traitements données an_nais
#même méthode pour l'année
annee_naissance = accidents$an_nais
annee_naissance = as.integer(annee_naissance)
annee_naissance[is.na(annee_naissance)] <- as.integer(mean(annee_naissance, na.rm = TRUE))
annee_naissance = as.integer(annee_naissance)
print(annee_naissance)

