
accidents <- read.csv("stat_acc_V3.csv",sep = ";", header = TRUE)

# Remplacer les valeurs NULL par NA
accidents[is.null(accidents)] <- NA

# Identifier les observations contenant des valeurs manquantes
na_rows <- complete.cases(accidents)

# Supprimer les observations contenant des valeurs manquantes
accidents_clean <- accidents[na_rows, ]
