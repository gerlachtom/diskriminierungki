# Analyse Script
print("Hier werden Pakete eingebunden")
# Data Cleaning ----
print("Hier wird der Datensatz aufbereitet. Thema am 09.11.2018")
# Skalenberechnung ----
print("Hier werden später Skalen berechnet. Thema am 09.11.2018")
# Analyse ----
print("Hier werden später statistische Analysen durchgeführt. Thema ab dem 16.11.2018")
# Graphik erstellung ---- 
print("Hier werden später Grafiken erstellt. Thema ab dem 16.11.2018")

#Bibliotheken laden ----
install.packages("tidyverse")
install.packages("lubridate")
install.packages("psych")
install.packages("esquisse")
install.packages("ggthemes")
install.packages("ggplot2")

install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")
library(tidyverse)
source("surveymonkey.R")

#Datensatz laden ----
filename <- "data/Smart Identification.csv"
raw <- load_surveymonkey_csv(filename) 
View(raw)

raw.short1 <- raw [,c(-1:-9)] 
raw.short2 <- raw.short1 [,c(-3:-14)]
raw.short <- raw.short2 [,c(-4:-85)]


#Codebook einmal generieren, danach ausklammern, sonst überschreibt es sich ----  
#generate_codebook(raw.short, "codebook.csv")

View(raw.short)

codeb <- read_codebook("codebook_final.csv")
names(raw.short) <- codeb$variable
View(raw.short)                       

#Faktoren zuweisen ----

skala.zustimmung <- c("Stimme gar nicht zu",
                      "Stimme nicht zu",
                      "Stimme eher nicht zu",
                      "Stimme eher zu",
                      "Stimme zu",
                      "Stimme völlig zu")

skala.nutzerfaktoren <- c("Mehrmals täglich",
                          "Einmal täglich",
                          "Mehrmals wöchentlich",
                          "Einmal wöchentlich",
                          "Seltener als einmal wöchentlich",
                          "nie")
