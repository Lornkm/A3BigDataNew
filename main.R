
accidents <- read.csv("stat_acc_V3.csv",sep = ";", header = TRUE)

na_rows <- complete.cases(accidents)

mydata_clean <- mydata[na_rows, ]
