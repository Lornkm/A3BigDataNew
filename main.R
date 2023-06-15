# Lecture des fichiers csv
library(readr)
accidents <- read.csv('stat_acc_V3.csv', sep = ";", header = TRUE)
habitants_regions <- read.csv('Regions.csv', sep=';')
habitants_departements <- read.csv('population-departements-france.csv')
link <- read.csv("link_region_dep.csv")