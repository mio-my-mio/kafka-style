library("stylo")
library("tidyverse")
library("stringr")
library("readtext")
library("tm")
library("tidytext")
library("quanteda")
library("quanteda.textplots")
library("quanteda.textstats")
library("stringi")
library("udpipe")

#---------------------------------------------------------------------------------------------------------------

#300 MFW, Plot width & heigth 10, font size
stylo()

#---------------------------------------------------------------------------------------------------------------

pfad_kafkawerke <- "C:\\Users\\Diana\\Documents\\kafka\\kafkawerke"
pfad_kafkabriefe <-"C:\\Users\\Diana\\Documents\\kafka\\kafkabriefe"
pfad_kafkagesamt <- "C:\\Users\\Diana\\Documents\\kafka\\kafkagesamt"
pfad_anderewerke <- "C:\\Users\\Diana\\Documents\\kafka\\anderewerke"
pfad_anderebriefe <- "C:\\Users\\Diana\\Documents\\kafka\\anderebriefe"
pfad_anderegesamt <- "C:\\Users\\Diana\\Documents\\kafka\\anderegesamt"

kafkawerke <- readtext(file.path(pfad_kafkawerke, "*.txt"))
kafkabriefe <- readtext(file.path(pfad_kafkabriefe, "*.txt"))
kafkagesamt <- readtext(file.path(pfad_kafkagesamt, "*.txt"))
anderewerke <- readtext(file.path(pfad_anderewerke, "*.txt"))
anderebriefe <- readtext(file.path(pfad_anderebriefe, "*.txt"))
anderegesamt <- readtext(file.path(pfad_anderegesamt, "*.txt"))

modalpartikel <- c("ja", "aber", "auch", "bloß", "denn", "doch", "eben", "eigentlich", 
                   "etwa", "halt", "ja", "mal", "nur", "schon", "vielleicht", "wohl",
                   "fein", "ganz", "gerade", "gleich", "einfach", "erst", "immerhin", 
                   "schließlich", "überhaupt", "ruhig")

#Liste von Wikipedia

anzahl <- function(text, partikel) {
  sum(str_count(str_to_lower(text), paste0("\\b(", paste(partikel, collapse = "|"), ")\\b" )))
}
gesamt <- function(text) {
  str_count(text, "\\w+")
}

anzahl_kafkawerke <- sum(map_int(kafkawerke$text, anzahl, modalpartikel))
anzahl_kafkabriefe <- sum(map_int(kafkabriefe$text, anzahl, modalpartikel))
anzahl_kafkagesamt <- sum(map_int(kafkagesamt$text, anzahl, modalpartikel))
anzahl_anderewerke <- sum(map_int(anderewerke$text, anzahl, modalpartikel))
anzahl_anderebriefe <- sum(map_int(anderebriefe$text, anzahl, modalpartikel))
anzahl_anderegesamt <- sum(map_int(anderegesamt$text, anzahl, modalpartikel))

#Ergebnisse: Kafkawerke: 14.995 Kafkabriefe: 2279 Kafkagesamt: 17.274 
#Ergebnisse: Anderewerke: 4349 Anderebriefe: 3953 Anderegesamt: 8302

gesamt_kafkawerke <- sum(map_int(kafkawerke$text, gesamt))
gesamt_kafkabriefe <- sum(map_int(kafkabriefe$text, gesamt))
gesamt_kafkagesamt <- sum(map_int(kafkagesamt$text, gesamt))
gesamt_anderewerke <- sum(map_int(anderewerke$text, gesamt))
gesamt_anderebriefe <- sum(map_int(anderebriefe$text, gesamt))
gesamt_anderegesamt <- sum(map_int(anderegesamt$text, gesamt))

#Ergebnisse: Kafkawerke: 317.857 Kafkabriefe: 43.643 Kafkagesamt: 391.500 
#Ergebnisse: Anderewerke: 182.216 Anderebriefe: 135.275 Anderegesamt: 317.491


#--------------- Vergleich Kafka - Andere -----------------------------------------------------------------


wortzahl_insgesamt <- gesamt_kafkagesamt + gesamt_anderegesamt
modalpartikel_insgesamt <- anzahl_kafkagesamt + anzahl_anderegesamt

#Ergebnisse: Gesamte Wortzahl: 678.991 Gesamte Anzahl Modalpartikel: 25.576

 
erwartete_häufigkeit_k <- (25576*361500)/678991
erwartete_häufigkeit_a <- (25576*317491)/678991

#Berechnung mit Zahlen statt Variablen, da sonst Ganzzahlüberlauf

#Erwartete Häufigkeit für Kafka: 13616.86 Erwartete Häufigkeit für Andere: 11959.14

differenz_kafka <- anzahl_kafkagesamt - erwartete_häufigkeit_k
differenz_andere <- anzahl_anderegesamt - erwartete_häufigkeit_a

#Modalpartikel sind in Kafka +3657.142 überrepräsentiert
#Modalpartikel sind bei anderen Autoren -3657.142 unterrepräsentiert



#---------------- Vergleich Kafka Werke - Briefe -----------------------------------------------------------


gesamt_kafkagesamt
modalpartikel_kafka <- anzahl_kafkawerke + anzahl_kafkabriefe

#Ergebnisse: Gesamte Wortzahl bei Kafka: 361.500 Gesamte Anzahl Modalpartikel bei Kafka: 17.274


erwartete_häufigkeit_w <- (17274*317857)/361500
erwartete_häufigkeit_b <- (17274*43643)/361500

#Berechnung mit Zahlen statt Variablen, da sonst Ganzzahlüberlauf
#Erwartete Häufigkeit für Werke: 15188.55 Erwartete Häufigkeit für Briefe: 2085.447

differenz_werke <- anzahl_kafkawerke - erwartete_häufigkeit_w
differenz_briefe <- anzahl_kafkabriefe - erwartete_häufigkeit_b

#Modalpartikel sind in Kafkas Werken -193.5527 unterrepräsentiert
#Modalpartikel sind in Kafkas Briefen 193.5527 überrepräsentiert



#---------------- Vergleich Andere Werke - Briefe -----------------------------------------------------------


gesamt_anderegesamt
anzahl_anderegesamt

#Ergebnisse: Gesamte Wortzahl bei Anderen: 317.491 Gesamte Anzahl Modalpartikel bei Anderen: 8.302


erwartete_häufigkeit_aw <- (8302*182216)/317491
erwartete_häufigkeit_ab <- (8302*135275)/317491

#Berechnung mit Zahlen statt Variablen, da sonst Ganzzahlüberlauf
#Erwartete Häufigkeit für Werke: 4764.725 Erwartete Häufigkeit für Briefe: 3537.275

differenz_anderewerke <- anzahl_anderewerke - erwartete_häufigkeit_aw
differenz_anderebriefe <- anzahl_anderebriefe - erwartete_häufigkeit_ab

#Modalpartikel sind in Anderen Werken -415.7248 unterrepräsentiert
#Modalpartikel sind in Anderen Briefen 415.7248 überrepräsentiert


#----------------Interpretation-------------------------------------------------------------------------------


#Kafka benutzt deutlich häufiger Modalpartikel als die anderen Autoren.
#Sowohl Kafka, als auch die zum Vergleich betrachteten Autoren nutzen mehr Modalpartikel in Briefen als in Werken.
#Der Unterschied zwischen Briefen und Werken ist dabei bei anderen bedeutend höher als bei Kafka.
#Dass trotz der höheren Wortzahl die Differenz bei Kafka geringer ist, weißt darauf hin, dass Kafka tatsächlich eine 
#geringere Variation in seiner Verwendung der Modalpartikel hatte, als die anderen Autoren.


#--------------In Verhältnis gesetzt---------------------------------------------------------------------------

verhältnis_k_alle <- anzahl_kafkagesamt/gesamt_kafkagesamt
verhältnis_a_alle <- anzahl_anderegesamt/gesamt_anderegesamt

verhältnis_k_briefe <- anzahl_kafkabriefe/gesamt_kafkabriefe
verhältnis_k_werke <- anzahl_kafkawerke/gesamt_kafkawerke

verhältnis_a_briefe <- anzahl_anderebriefe/gesamt_anderebriefe
verhältnis_a_werke <- anzahl_anderewerke/gesamt_anderewerke


#---------------Darstellung als Graph--------------------------------------------------------------------------

df1 <- data.frame(
  a = c("Kafkas Werke", "Kafkas Briefe", "Andere Werke", "Andere Briefe"),
  b = c(verhältnis_k_werke, verhältnis_k_werke, verhältnis_a_werke, verhältnis_a_briefe)
)

df2 <- data.frame(
  a = c("Kafka", "Andere Autoren"),
  b = c(verhältnis_k_alle, verhältnis_a_alle)
)  

ggplot(df1, aes(x = a, y = b, fill = a)) +
  geom_bar(stat = "identity") +
  labs(title = "Verhältnis der Modalpartikel", y = "Prozentualer Anteil", x = "Text") +
  theme_classic()

ggplot(df2, aes(x = a, y = b, fill = a)) +
  geom_bar(stat = "identity") +
  labs(title = "Verhältnis der Modalpartikel", y = "Prozentualer Anteil", x = "Autor") +
  theme_classic()
