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
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("psych")
#install.packages("esquisse")
#install.packages("ggthemes")
#install.packages("ggplot2")

#install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")
library(tidyverse)
source("surveymonkey.R")


#Datensatz laden ----
filename <- "data/SmartIdentification.csv"
raw <- load_surveymonkey_csv(filename) 

##### FEEDBACK: Das ginge auch in einer Zeile: raw.short <- raw[-1:-9, usw., usw., ] Dann sind die Zahlen allerdings anders :-) ----
raw.short1 <- raw [,c(-1:-9)] 
raw.short2 <- raw.short1 [,c(-4)]  
raw.short3 <- raw.short2 [,c(-5)]
raw.short <- raw.short3 [,c(-14:-95)]


#Codebook einmal generieren, danach ausklammern, sonst überschreibt es sich ----  
#generate_codebook(raw.short, "codebook.csv")



codeb <- read_codebook("codebook_final.csv")
names(raw.short) <- codeb$variable
                     

raw.short$geschlecht <- as.factor(raw.short$geschlecht)
raw.short$bildung <- as.factor(raw.short$bildung)
raw.short$job <- as.factor(raw.short$job)
raw.short$alter <- as.numeric(raw.short$alter)

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

raw.short$kut1 <- ordered(raw.short$kut1, levels = skala.zustimmung)
raw.short$kut2 <- ordered(raw.short$kut2, levels = skala.zustimmung)
raw.short$kut3 <- ordered(raw.short$kut3, levels = skala.zustimmung)
raw.short$kut4 <- ordered(raw.short$kut4, levels = skala.zustimmung)
raw.short$kut5 <- ordered(raw.short$kut5, levels = skala.zustimmung)
raw.short$kut6 <- ordered(raw.short$kut6, levels = skala.zustimmung)
raw.short$kut7 <- ordered(raw.short$kut7, levels = skala.zustimmung)
raw.short$kut8 <- ordered(raw.short$kut8, levels = skala.zustimmung)

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


#### Skalen berechnen ----

library(psych)

schluesselliste <- list(NUTZUNG = c("nutzung"),
                        KUT = c("kut1", "kut2", "kut3", "kut4", "kut5", "kut6", "kut7", "kut8"),
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
  select (-starts_with ("kut", ignore.case = F)) %>%
  select (-starts_with("wahrnehmung", ignore.case = F)) %>%
  select (-starts_with("einordnung", ignore.case = F)) %>%
  select (-starts_with("targeting", ignore.case = F)) %>%
  select (-starts_with("genderbezug", ignore.case = F)) %>%
  select (-starts_with("diskri", ignore.case = F))

new_df <- transform(data, altersgruppe=cut(data$alter, breaks=c(-Inf, median(data$alter), Inf), labels=c("jung", "alt")))

saveRDS(data, "data/SmartIdentification2.rds")

###T-Tests ----

#### Unterschiedshypothese 1: Alter und Wahrnehmung  ----
## Hypothese: Ältere und jüngere Menschen unterscheiden sich in der Häufigkeit der Wahrnehmung von geschlechtsspezifischen Beiträgen.
## H0: Ältere und jüngere Menschen unterscheiden sich nicht in der Häufigkeit der Wahrnehmung von geschlechtsspezifischen Beiträgen.
## Unverbundener T-Test. UV: Alter, AV: Wahrnehmung:
t.test(filter(new_df, altersgruppe=="jung")$WAHRNEHMUNG,
       filter(new_df, altersgruppe=="alt")$WAHRNEHMUNG)
## Ergebnis: H0 verwerfen. Es gibt keinen signifikanten Unterschied zwischen älteren und jüngeren Menschen in der Häufigkeit der 
#            Wahrnehmung von geschlechtsspezifischen Beiträgen (t(271.66) = 1.9324, p = 0.054).

#### Unterschiedshypothese 2: Geschlecht und Bewertung von Targeting  ----
## Hypothese: Männer und Frauen unterscheiden sich in der Beurteilung von zielgerichteten Beiträgen.
## H0: Männer und Frauen unterscheiden sich nicht in der Beurteilung von zielgerichteten Beiträgen.
## Unverbundener T-Test. UV: Geschlecht, AV: Targeting:
t.test(filter(data, geschlecht=="Männlich")$TARGETING,
       filter(data, geschlecht=="Weiblich")$TARGETING)
## Ergebnis: Es gibt einen statistisch signifikanten Unterschied zwischen der Beurteilung von zielgerichteten Beiträgen zwischen
##           Männern und Frauen (t(221.49) = -2.2364, p = .02632*). Dieser Unterschied liegt mit 95% Sicherheit zwischen "stimme eher
#            nicht zu" und "stimme eher zu".



#### Unterschiedshypothese 3: Geschlecht und Empfindung von Diskriminierung  ----
## Hypothese: Männer und Frauen unterscheiden sich in der Empfindung von geschlechtsspezifischer Diskriminierung.
## H0: Männer und Frauen unterscheiden sich nicht in der Empfindung von geschlechtsspezifischer Diskriminierung.
## Unverbundener T-Test. UV: Geschlecht, AV: Diskriminierung:
t.test(filter(data, geschlecht=="Männlich")$DISKRI,
       filter(data, geschlecht=="Weiblich")$DISKRI)
## Ergebnis: Es gibt einen statistisch signifikanten Unterschied zwischen der Empfindung von geschlechtsspezifischer Diskriminierung
#            zwischen Männern und Frauen (t(241.24) = -5.8542, p = 1.558e-08***). Dieser Unterschied liegt mit 95% Sicherheit zwischen
#            "stimme nicht zu" und "stimme eher nicht zu"

#### Zusammenhangshypothese 1: Nutzung und Wahrnehmung
## H1: Es besteht ein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Wahrnehmung von geschlechtsspezifischen Werbebeiträgen.
## H0: Es besteht kein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Wahrnehmung von geschlechtsspezifischen Werbebeiträgen.
cor.test(data = data, ~ NUTZUNG+WAHRNEHMUNG)

## Ergebnis: Es besteht ein signifikanter Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Wahrnehmung von
#            geschlechtsspezifischen Werbebeiträgen (r(276) = -3.6957, p = .0264*). Dieser Zusammenhang liegt mit 95% Sicherheit
#            zwischen

#### Zusammenhangshypothese 2:  Targeting und Diskriminierung 
## H1: Es besteht ein Zusammenhang zwischen der Beurteilung zielgerichteter Beiträge und dem Empfinden der Diskriminierung bei gesponserten Werbebeiträgen.
## H0: Es besteht kein Zusammenhang zwischen der Beurteilung zielgerichteter Beiträge und dem Empfinden der Diskriminierung bei gesponsterten Werbebeiträgen.
cor.test(data = data, ~ TARGETING+DISKRI)

##Ergebnis: Es besteht ein signifikanter Zusammenhang zwischen der Beurteilung zielgerichteter Beiträge und der Emmpfindung von 
#           Diskriminierung bei gesponsorten Werbebeiträgen (r(276) = 4.3383, p = 2.016e-05***). Dieser Zusammenhang liegt mit 95% Sicherheit
#           zwischen

#### Zusammenhangshypothese 3: Nutzung und Einordnung
## H1: Es besteht ein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Einordnung von gesponserten Beiträgen.
## H0: Es besteht kein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Einordnung von gesponserten Beiträgen.
cor.test(data = data, ~ NUTZUNG+EINORDNUNG)

##Ergebnis: Es besteht kein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Einordnung von gesponsorten Beiträgen
#           r((276) = -0.74439, p = .4573).
    