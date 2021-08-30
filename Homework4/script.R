install.packages("genie")
install.packages("C:/Users/José Manuel Pérez/Desktop/IdR_1.0.tar.gz", 
                 repos = NULL, type = "source", lib="C:/Tools/R/R-3.5.1/library")
library(genie)
library(IdR)

setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/IntelligentDataAnalysis/Labs/Homework4/")

load(file = "IdR_1.0/IdR/data/nhlv1.rda")
summary.network(nhlv1, verbose = TRUE)

dump.netG(nhlv1, "nhlv1")



