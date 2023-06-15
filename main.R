# Lecture des fichiers csv
library(readr)
accidents <- read.csv('Cours/Big Data/A3BigDataNew/stat_acc_V3.csv', sep = ";", header = TRUE)
habitants_regions <- read.csv('Cours/Big Data/A3BigDataNew/Regions.csv', sep=';')
habitants_departements <- read.csv('Cours/Big Data/A3BigDataNew/population-departements-france.csv')
link <- read.csv("Cours/Big Data/A3BigDataNew/link_region_dep.csv")
