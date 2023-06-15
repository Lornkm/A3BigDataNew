source('traitements_donnees.R')
source('visualisation.R')

#TEST DU KI2

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

mosaicplot(tableau_croise)


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
