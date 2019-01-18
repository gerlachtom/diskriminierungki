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
#install.packages("jmv")
#install.packages("likert")

#install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")
library(tidyverse)
library(jmv)
source("surveymonkey.R")
library(likert)


#Datensatz laden ----
filename <- "data/SmartIdentification_all.csv"
raw <- load_surveymonkey_csv(filename) 

raw.short1 <- raw [,c(-1:-9)] 
raw.short2 <- raw.short1 [,c(-4)]  
raw.short3 <- raw.short2 [,c(-5)]

raw.short <- raw.short3 [c(-274:-280),c(-14:-95)]


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


schluesselliste <- list(NUTZUNG = c("-nutzung"),
                        KUT = c("kut1", "-kut2", "kut3", "kut4", "-kut5", "kut6", "-kut7", "-kut8"),
                        WAHRNEHMUNG = c("wahrnehmung1", "wahrnehmung2", "wahrnehmung3", "wahrnehmung4", "-wahrnehmung5"),
                        EINORDNUNG = c("einordnung1", "einordnung2", "einordnung3"),
                        TARGETING = c("targeting1", "-targeting2", "-targeting3", "targeting4"),
                        GENDERBEZUG = c("genderbezug1", "-genderbezug2", "genderbezug3", "-genderbezug4"),
                        DISKRI = c("diskri1", "-diskri2", "diskri3", "diskri4")
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
## Hypothese: Ältere und jüngere Menschen unterscheiden sich in der Häufigkeit der Wahrnehmung von zielgerichteten Werbebeiträgen.
## H0: Ältere und jüngere Menschen unterscheiden sich nicht in der Häufigkeit der Wahrnehmung von zielgerichteten Werbebeiträgen.
## Unverbundener T-Test. UV: Alter, AV: Wahrnehmung:
t.test(filter(new_df, altersgruppe=="jung")$WAHRNEHMUNG,
       filter(new_df, altersgruppe=="alt")$WAHRNEHMUNG)
## Ergebnis: H0 verwerfen. Es gibt einen signifikanten Unterschied zwischen älteren und jüngeren Menschen in der Häufigkeit der 
#            Wahrnehmung von geschlechtsspezifischen Beiträgen (t(266.28) = 2.501, p = 0.012*).

#### Unterschiedshypothese 2: Geschlecht und Bewertung von Targeting  ----
## Hypothese: Männer und Frauen unterscheiden sich in der Beurteilung von zielgerichteten Beiträgen.
## H0: Männer und Frauen unterscheiden sich nicht in der Beurteilung von zielgerichteten Beiträgen.
## Unverbundener T-Test. UV: Geschlecht, AV: Targeting:
t.test(filter(data, geschlecht=="Männlich")$TARGETING,
       filter(data, geschlecht=="Weiblich")$TARGETING)
## Ergebnis: Es gibt einen statistisch signifikanten Unterschied zwischen der Beurteilung von zielgerichteten Beiträgen zwischen
##           Männern und Frauen (t(206.73) = -2.49, p = .013*). Dieser Unterschied liegt mit 95% Sicherheit zwischen "stimme eher
#            nicht zu" und "stimme eher zu".

#Grafik erstellen zu Unterschiedshyptohese 2----

library(ggplot2)

#data %>% 
#  filter(geschlecht != "keine Angabe") %>% 
 # group_by(geschlecht) %>% 
  #summarise(mean_TARGETING, sem_TARGETING = stderr(TARGETING))
#ggplot() +
 # aes(x = geschlecht, y = TARGETING) +
  #geom_boxplot(fill = '#0c4c8a') +
  #labs(title = 'Frauen nehmen Targeting in Sozialen Netzwerken häufiger wahr als Männer',
   #    x = 'Geschlecht',
    #   y = 'Targeting',
     #  caption = 'n=273, Punkte sind Ausreißer',
      # subtitle = 'Boxplot von Targeting') +
#  theme_gray()

data %>% 
  filter(geschlecht != "keine Angabe") %>% 
  group_by(geschlecht) %>% 
ggplot() +
  aes(x = geschlecht, y = TARGETING) +
  geom_boxplot(fill = '#0c4c8a') +
  labs(title = 'Frauen nehmen Targeting in Sozialen Netzwerken häufiger wahr als Männer',
       x = 'Geschlecht',
       y = 'Targeting',
       caption = 'n=273, Punkte sind Ausreißer',
       subtitle = 'Boxplot von Geschlecht und Targeting') +
  theme_gray()


#### Unterschiedshypothese 3: Geschlecht und Empfindung von Diskriminierung  ----
## Hypothese: Männer und Frauen unterscheiden sich in der Empfindung von geschlechtsspezifischer Diskriminierung.
## H0: Männer und Frauen unterscheiden sich nicht in der Empfindung von geschlechtsspezifischer Diskriminierung.
## Unverbundener T-Test. UV: Geschlecht, AV: Diskriminierung:
t.test(filter(data, geschlecht=="Männlich")$DISKRI,
       filter(data, geschlecht=="Weiblich")$DISKRI)
## Ergebnis: Es gibt einen statistisch signifikanten Unterschied zwischen der Empfindung von geschlechtsspezifischer Diskriminierung
#            zwischen Männern und Frauen (t(238.11) = -6.14, p = 3.408e-09***). Dieser Unterschied liegt mit 95% Sicherheit zwischen
#            "stimme nicht zu" und "stimme eher nicht zu"

#Graphik erstellen zu Unterschiedshypothese 3----
library(ggplot2)

#Farben verfügbar machen
rwthcolor <- hcictools::rwth.colorpalette()

data %>% 
  filter(geschlecht != "Keine Angabe") %>% 
  group_by(geschlecht) %>% 
ggplot() +
  aes(x = geschlecht, y = DISKRI, fill = geschlecht) +
  scale_fill_manual(values = c(rwthcolor$blue, rwthcolor$red)) +
  geom_boxplot() +
  labs(title = 'Frauen nehmen geschlechtsspezifische Diskriminierung häufiger wahr als Männer',
       fill = "Geschlecht",
    x = 'Geschlecht',
    y = 'Diskriminierungsempfinden [1-6]',
    caption = 'n=272, Punkte sind Ausreißer',
    subtitle = 'Boxplot zum Diskriminierungsempfinden abhängig vom Geschlecht') +
  theme_gray()

ggsave("Boxplot Unterschiedshypothese 3.pdf", width = 8, height = 6)

#### Zusammenhangshypothese 1: Nutzung und Wahrnehmung
## H1: Es besteht ein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Wahrnehmung von geschlechtsspezifischen Werbebeiträgen.
## H0: Es besteht kein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Wahrnehmung von geschlechtsspezifischen Werbebeiträgen.
cor.test(data = data, ~ NUTZUNG+WAHRNEHMUNG)

## Ergebnis: Es besteht ein signifikanter Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Wahrnehmung von geschlechtsspezifischen Werbebeiträgen (r(271) = .41, p = 1.34e-12***). 
#Dieser Korrelationskoeffizient liegt mit 95% Sicherheit zwischen 0.31 und 0.51 Punkten.Je häufiger soziale Netzwerke genutzt werden, desto häufiger werden geschlechtsspezifische Werbebotschaften wahrgenommen.

#### Zusammenhangshypothese 2:  Targeting und Diskriminierung 
## H1: Es besteht ein Zusammenhang zwischen der Beurteilung zielgerichteter Beiträge und dem Empfinden der Diskriminierung bei gesponserten Werbebeiträgen.
## H0: Es besteht kein Zusammenhang zwischen der Beurteilung zielgerichteter Beiträge und dem Empfinden der Diskriminierung bei gesponsterten Werbebeiträgen.
cor.test(data = data, ~ TARGETING+DISKRI)

##Ergebnis: Es besteht ein signifikanter Zusammenhang zwischen der Beurteilung zielgerichteter Beiträge und der Empfindung von 
#           Diskriminierung bei gesponsorten Werbebeiträgen (r(271) = -.39, p = 1.77e-11***). Der Korrelationskoeffizient liegt mit 95% Sicherheit zwischen -0.49 und -0.28.

#### Zusammenhangshypothese 3: Nutzung und Einordnung
## H1: Es besteht ein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Einordnung von gesponserten Beiträgen.
## H0: Es besteht kein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Einordnung von gesponserten Beiträgen.
cor.test(data = data, ~ NUTZUNG+EINORDNUNG)

##Ergebnis: Es besteht kein Zusammenhang zwischen der Nutzung sozialer Netzwerke und der Einordnung von gesponsorten Beiträgen
#           r((271) = .05, p = .42).


#### Deskriptive Statistik für die unabhängigen Variablen----

data %>%  select(geschlecht, alter, NUTZUNG, KUT, WAHRNEHMUNG, EINORDNUNG, TARGETING, GENDERBEZUG, DISKRI) -> data_iv
data_iv %>%  psych:: describe()

####Korrelationsanalyse
jmv:: corrMatrix(data_iv, vars = c( "alter", "NUTZUNG", "KUT", "WAHRNEHMUNG", "EINORDNUNG", "TARGETING", "GENDERBEZUG", "DISKRI"))

####Lineare Regression für Wahrnehmung

jmv::linReg(data_iv, dep="WAHRNEHMUNG", covs =c("EINORDNUNG", "TARGETING"), 
            blocks=c("EINORDNUNG", "TARGETING"),
            stdEst = TRUE, anova = TRUE, qqPlot = T, r2Adj=T, collin = T)

library(ggplot2)

ggplot(data = data) +
  aes(x = NUTZUNG, y = WAHRNEHMUNG) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()
ggplot(data = data) +
  aes(x = NUTZUNG, y = WAHRNEHMUNG) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

####Lineare Regression für Nutzung

jmv::linReg(data_iv, dep="NUTZUNG", covs =c("WAHRNEHMUNG"), 
            blocks=c("WAHRNEHMUNG"),
            stdEst = TRUE, anova = TRUE, qqPlot = T, r2Adj=T, collin = T)

####Lineare Regression für Diskriminierungsempfinden

jmv::linReg(data_iv, dep="DISKRI", covs =c("GENDERBEZUG"), 
            blocks=c("GENDERBEZUG"),
            stdEst = TRUE, anova = TRUE, qqPlot = T, r2Adj=T, collin = T)

####Lineare Regression für KUT

jmv::linReg(data_iv, dep="KUT", covs =c("TARGETING", "DISKRI"), 
            blocks=c("TARGETING", "DISKRI"),
            stdEst = TRUE, anova = TRUE, qqPlot = T,r2Adj=T, collin = T)

#### Lineare Regression für Targeting

jmv::linReg(data_iv, dep="TARGETING", covs =c("DISKRI"), 
            blocks=c("DISKRI"),
            stdEst = TRUE, anova = TRUE, qqPlot = T,r2Adj=T, collin = T)


#Deskriptive Statistik: Beschreibung der Stichprobe----
library(ggplot2)

ggplot(data = data) +
  aes(x = alter) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  labs(title = "Das Alter aller Probanden der Stichprobe",
       caption = "n = 273 
       bins = 30",
    x = "Alter",
    y = "Häufigkeit") +
  theme_minimal()
 +
  theme_minimal()



#Likert Diagramme zu den einzelnen Konstrukten----

raw.short$wahrnehmung1 <- factor(raw.short$wahrnehmung1, labels = skala.zustimmung)
raw.short$wahrnehmung2 <- factor(raw.short$wahrnehmung2, labels = skala.zustimmung)
raw.short$wahrnehmung3 <- factor(raw.short$wahrnehmung3, labels = skala.zustimmung)
raw.short$wahrnehmung4 <- factor(raw.short$wahrnehmung4, labels = skala.zustimmung)
raw.short$wahrnehmung5 <- factor(raw.short$wahrnehmung5, labels = skala.zustimmung)

raw.short$diskri1 <-factor(raw.short$diskri1, labels = skala.zustimmung)
raw.short$diskri2 <-factor(raw.short$diskri2, labels = skala.zustimmung)
raw.short$diskri3 <-factor(raw.short$diskri3, labels = skala.zustimmung)
raw.short$diskri4 <-factor(raw.short$diskri4, labels = skala.zustimmung)

raw.short$targeting1 <- factor(raw.short$targeting1, labels = skala.zustimmung)
raw.short$targeting2 <- factor(raw.short$targeting2, labels = skala.zustimmung)
raw.short$targeting3 <- factor(raw.short$targeting3, labels = skala.zustimmung)
raw.short$targeting4 <- factor(raw.short$targeting4, labels = skala.zustimmung)

raw.short$nutzung <- factor(raw.short$nutzung, labels = skala.nutzerfaktoren)

pl <- raw.short %>% 
  select(wahrnehmung1, wahrnehmung2, wahrnehmung3, wahrnehmung4, wahrnehmung5) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm Wahrnehmung von gesponserten Werbebeiträgen", y= "Prozent",
       x= "Wahrnehmung",
       fill = "Antwort")
pl

colnames(raw.short)[which(names(raw.short) == "diskri1")] <- "geschlechterdiskriminierend"
colnames(raw.short)[which(names(raw.short) == "diskri2")] <- "geschlechtsneutral"
colnames(raw.short)[which(names(raw.short) == "diskri3")] <- "persönlich"
colnames(raw.short)[which(names(raw.short) == "diskri4")] <- "Diskriminierungspotenzial"

pl1 <- raw.short %>% 
  select(geschlechterdiskriminierend, geschlechtsneutral, persönlich, Diskriminierungspotenzial) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm Diskriminierungsempfinden durch gesponserte Werbebeiträge", y= "Prozent",
       x= "Diskriminierungsempfinden",
       fill = "Antwort")

pl1

#colnames(raw.short)[which(names(raw.short) == "targeting1")] <- "nützlich"
#colnames(raw.short)[which(names(raw.short) == "targeting2")] <- "störend"
#colnames(raw.short)[which(names(raw.short) == "targeting3")] <- "beängstigend"
#colnames(raw.short)[which(names(raw.short) == "targeting4")] <- "Produktfindungshilfe"
#pl2 <- raw.short %>% 
  select(nützlich, störend, beängstigend, Produktfindungshilfe) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot() +
  labs(title = "Likert Diagramm Beurteilung von Targeting", y= "Prozent",
       x= "Targeting",
       fill = "Antwort")
pl2


pl3 <- raw.short %>%
  select(nutzung) %>% 
  as.data.frame() %>% 
  likert() %>% 
  plot()+
  labs(title = "Likert Diagramm Nutzung", y= "Prozent",
       x="Nutzung",
       fill ="Antwort")
pl3



    