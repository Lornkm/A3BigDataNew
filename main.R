
accidents <- read.csv('/Users/noedetre/Documents/ISEN/projet_Big_Data/A3BigDataNew/stat_acc_V3.csv', sep = ";", header = TRUE)
accidents$descr_cat_veh <- as.numeric(factor(accidents$descr_cat_veh))
print(accidents$descr_cat_veh[1:20])



# Remplacer les valeurs NULL par NA
accidents[is.null(accidents)] <- NA

# Identifier les observations contenant des valeurs manquantes
na_rows <- complete.cases(accidents)

print("lrkn << ndtr")