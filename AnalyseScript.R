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

##### FEEDBACK: Das ginge auch in einer Zeile: raw.short <- raw[-1:-9, usw., usw., ] Dann sind die Zahlen allerdings anders :-) ----
raw.short1 <- raw [,c(-1:-9)] 
raw.short2 <- raw.short1 [,c(-3:-14)]
raw.short <- raw.short2 [,c(-4:-85)]


#Codebook einmal generieren, danach ausklammern, sonst überschreibt es sich ----  
#generate_codebook(raw.short, "codebook.csv")

View(raw.short)

codeb <- read_codebook("codebook_final.csv")
names(raw.short) <- codeb$variable
View(raw.short)                       

raw.short$geschlecht <- as.factor(raw.short$geschlecht)

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
                          "Nie")

raw.short$nutzung <- ordered(raw.short$nutzung, levels = skala.nutzerfaktoren)

raw.short$wahrnehmung1 <- ordered(raw.short$wahrnehmung1, levels = skala.zustimmung)
raw.short$wahrnehmung2 <- ordered(raw.short$wahrnehmung2, levels = skala.zustimmung)
raw.short$wahrnehmung3 <- ordered(raw.short$wahrnehmung3, levels = skala.zustimmung)
raw.short$wahrnehmung4 <- ordered(raw.short$wahrnehmung4, levels = skala.zustimmung)
raw.short$wahrnehmung5 <- ordered(raw.short$wahrnehmung5, levels = skala.zustimmung)

raw.short$einordnung1 <- ordered(raw.short$einordnung1, levels =skala.zustimmung)
raw.short$einordnung2 <- ordered(raw.short$einordnung2, levels =skala.zustimmung)
raw.short$einordnung3 <- ordered(raw.short$einordnung3, levels =skala.zustimmung)

raw.short$targeting1 <- ordered(raw.short$targeting1, levels = skala.zustimmung)
raw.short$targeting2 <- ordered(raw.short$targeting2, levels = skala.zustimmung)
raw.short$targeting3 <- ordered(raw.short$targeting3, levels = skala.zustimmung)
raw.short$targeting4 <- ordered(raw.short$targeting4, levels = skala.zustimmung)

raw.short$genderbezug1 <- ordered(raw.short$genderbezug1, levels = skala.zustimmung)
raw.short$genderbezug2 <- ordered(raw.short$genderbezug2, levels = skala.zustimmung)
raw.short$genderbezug3 <- ordered(raw.short$genderbezug3, levels = skala.zustimmung)
raw.short$genderbezug4 <- ordered(raw.short$genderbezug4, levels = skala.zustimmung)

raw.short$diskri1 <- ordered(raw.short$diskri1, levels = skala.zustimmung)
raw.short$diskri2 <- ordered(raw.short$diskri2, levels = skala.zustimmung)
raw.short$diskri3 <- ordered(raw.short$diskri3, levels = skala.zustimmung)
raw.short$diskri4 <- ordered(raw.short$diskri4, levels = skala.zustimmung)


#### Skalen berechnen

library(psych)

schluesselliste <- list(NUTZUNG = c("nutzung"),
                        WAHRNEHMUNG = c("wahrnehmung1", "wahrnehmung2", "wahrnehmung3", "wahrnehmung4", "wahrnehmung5"),
                        EINORDNUNG = c("einordnung1", "einordnung2", "einordnung3"),
                        TARGETING = c("targeting1", "targeting2", "targeting3", "targeting4"),
                        GENDERBEZUG = c("genderbezug1", "genderbezug2", "genderbezug3", "genderbezug4"),
                        DISKRI = c("diskri1", "diskri2", "diskri3", "diskri4")
                        )
scores <- scoreItems(schluesselliste, raw.short, missing = TRUE, min = 1, max = 6)

data <- bind_cols(raw.short, as.tibble(scores$scores))
data <- data %>%
  select (-starts_with("nutzung", ignore.case = F)) %>%
  select (-starts_with("wahrnehmung", ignore.case = F)) %>%
  select (-starts_with("einordnung", ignore.case = F)) %>%
  select (-starts_with("targeting", ignore.case = F)) %>%
  select (-starts_with("genderbezug", ignore.case = F)) %>%
  select (-starts_with("diskri", ignore.case = F))

saveRDS(data, "data/Smart Identification2.rds")
View(data)

##### FEEDBACK: Gefällt mir eigentlich ganz gut. Die vielen Kontrollausgaben mit View() sollten Sie noch entfernen, das kann schnell irritieren, wenn jemand den ganzen Code auf einmal ausführt. ----
##### Schauen Sie aber bitte nochmal, welche Nutzerfaktoren aktuell noch im Fragebogen sind, sie haben insgesamt sehr großzügig Spalten entfernt. KUT und den Bildungsstand sollten Sie auf keinen Fall löschen.


          