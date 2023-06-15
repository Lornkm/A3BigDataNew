source('traitements_donnees.R')
source('visualisation.R')

#TEST DU KI2

# Définir les groupes par tranche d'age
age_groups <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

# Créer une nouvelle colonne pour les ages
accidents$age_group <- cut(accidents$age, breaks = c(0, 17, 24, 34, 44, 54, 64, Inf), labels = age_groups, right = TRUE)

# Afficher les premières lignes pour vérification
print(accidents$age_group[1:20])

# Test du chi2 sur descr_grav  et âge
tableau_croise_age_grav <- table(accidents$descr_grav, accidents$age_group)
print(tableau_croise_age_grav)
result_khi2_age_grav <- chisq.test(tableau_croise_age_grav)
# Afficher les résultats du test
print(result_khi2_age_grav)

mosaicplot(tableau_croise_age_grav)

# Créer un sous-ensemble de données en excluant les lignes où place = 0
accidents_sans_zero <- accidents[accidents$place != 0, ]
# Les lignes où place = 0 sont encore présentes dans le dataframe original "accidents"

# Tableau croisé entre la gravité (grav) et la place utilisée (place)
tableau_croise <- table(accidents_sans_zero$descr_grav, accidents_sans_zero$place)
print(tableau_croise)
# Effectuer un test du khi-deux
test_khi_deux <- chisq.test(tableau_croise)

# Afficher les résultats du test du khi-deux
print(test_khi_deux)

# Représentation en mosaïque
mosaicplot(tableau_croise, las = 1, main = "Tableau croisé : Gravité vs Place utilisée")


#tableau croisé entre gravité et dispositif de sécurité
tableau_croise_secu_grav <- table(accidents$descr_grav, accidents$descr_dispo_secu)
print(tableau_croise_secu_grav)
result_khi2_secu_grav <- chisq.test(tableau_croise_secu_grav)
print(result_khi2_secu_grav)
mosaicplot(tableau_croise_secu_grav, las=1, main = "Mosaïque entre la gravité et le dispositif de sécurité")


#tableau croisé entre gravité et la description de la catégorie du véhicule
tableau_croise_catveh_grav <- table(accidents$descr_grav, accidents$descr_cat_veh)
print(tableau_croise_catveh_grav)
result_khi2_catveh_grav <- chisq.test(tableau_croise_catveh_grav)
print(result_khi2_catveh_grav)
mosaicplot(tableau_croise_catveh_grav, las = 1, main = "Mosaïque entre la gravité et la catégorie du véhicule")



# Calculer la corrélation de Pearson
correlation_catveh_grav <- cor(accidents$descr_grav, accidents$descr_cat_veh, method = "pearson")
print(correlation_catveh_grav)

correlation_secu_grav <- cor(accidents$descr_grav, accidents$descr_dispo_secu, method = "pearson")
print(correlation_secu_grav)

correlation_place_grav <- cor(accidents_sans_zero$descr_grav, accidents_sans_zero$place, method = "pearson")
print(correlation_place_grav)

# REGRESSIONS LINEAIRES

# Régression linéaire entre les accidents et les semaines
week_year <- seq(1, 53)
# Calculer la somme ajouter des accidents par semaine
weekly_accidents_reglin <- cumsum(weekly_accidents$accidents)
modele_accident_semaine <- lm(weekly_accidents_reglin ~ week_year)
summary(modele_accident_semaine)
# F value de 58 597 : le modèle est statistiquement significatif
anova(modele_accident_semaine)
plot(modele_accident_semaine)
cor.test(x = week_year, y = weekly_accidents_reglin, method = "pearson")

# Régression linéaire entre les accidents et les mois
month_year <- seq(1, 12)
# Calculer la somme ajouter des accidents par mois
monthly_accidents_reglin <- cumsum(monthly_accidents$accidents)
modele_accident_mois <- lm(monthly_accidents_reglin ~ month_year)
summary(modele_accident_mois)
# F value de 10 714 : le modèle est statistiquement significatif
anova(modele_accident_mois)
plot(modele_accident_mois)
