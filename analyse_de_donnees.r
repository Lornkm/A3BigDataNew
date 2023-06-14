
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

