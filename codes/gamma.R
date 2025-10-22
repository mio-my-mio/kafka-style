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
library("tokenizers")

#---------------------------------------------------------------------------------------------------------------

#-----Funktionen------------------------------------------------------------------------------------------------

zumdataframe <- function(pfad){
  werk <- readtext(file.path(pfad))
  saetze <- tokenize_sentence(werk$text) [[1]]
  satzlaenge <- sapply(saetze, function(s) {
    length(tokenize_words(s)[[1]])
  })
  df <- data.frame(satz = array(1:length(satzlaenge)), inhalt = saetze, satzlaenge = satzlaenge)
}

zumdf_moderne <- function(ex) {
  datein <- list.files("C:\\Users\\Diana\\Documents\\kafka\\m_vergleichscorpus", pattern = ex, full.names = TRUE)
  texte <- readtext(datein)
  saetze <- tokenize_sentence(paste(texte$text, collapse = " ")) [[1]]
  satzlaenge <- sapply(saetze, function(s) {
    length(tokenize_words(s)[[1]])
  })
  m_vergleichswerke <- data.frame(satz = array(1:length(satzlaenge)), inhalt = saetze, satzlaenge = satzlaenge)
}

zumdf_express <- function(ex) {
  datein <- list.files("C:\\Users\\Diana\\Documents\\kafka\\e_vergleichscorpus", pattern = ex, full.names = TRUE)
  texte <- readtext(datein)
  saetze <- tokenize_sentence(paste(texte$text, collapse = " ")) [[1]]
  satzlaenge <- sapply(saetze, function(s) {
    length(tokenize_words(s)[[1]])
  })
  m_vergleichswerke <- data.frame(satz = array(1:length(satzlaenge)), inhalt = saetze, satzlaenge = satzlaenge)
}


zumplot <- function(df){
  ggplot(data = df, aes(x=satz, y=satzlaenge)) +
    geom_point()
}

zudfm <- function(pfad) {
  c <- corpus(readtext(file.path(pfad)))
  c_tokens <- tokens(c, remove_punct = TRUE) %>%
    tokens_tolower()
  dfm <- dfm(c_tokens)
}

zutokens <- function(pfad) {
  c <- corpus(readtext(file.path(pfad)))
  c_tokens <- tokens(c, remove_punct = TRUE) %>%
    tokens_tolower()
}

differenzen <- function(df){
  unterschiede <- vector("numeric", length = nrow(df))
  for(i in 1:nrow(df)) {
      wert <- df$satzlaenge[i]
      naechster <- df$satzlaenge[i+1]
      differenz <- wert-naechster
      unterschiede[i] = differenz
    }
  df <- data.frame(satz = df$satz, differenz = unterschiede)
}

#absolute value der differenzen ja nein?
#wenn nicht abs dann einfach gespiegelt, wenn abs dann doppelt

differenz_haeufigkeiten <- function(df) {
  haeufigkeiten <- table(differenzen(df)$differenz)
  df <- as.data.frame(haeufigkeiten)
  colnames(df) <- c("differenz", "haeufigkeit")
  df <- df[order(df$haeufigkeit, decreasing = TRUE), ]
  df <- data.frame(rang = array(1:length(haeufigkeiten)), differenz = df$differenz, hauefigkeit = df$haeufigkeit)
}

#Berechnung der Standardabweichung (oder so in der Art, nicht quadriert + Wurzel)
#Durchschnitt überprüft mit geom_smooth in Diagramm, stimmt überein

std_abweichung <- function(df) {
  abweichungen <- vector("numeric", length = nrow(df))
  for(i in 1:nrow(df)) {
    durchschnitt <- sum(df$satzlaenge)/nrow(df)
    wert <- df$satzlaenge[i]
    abweichung <- (wert-durchschnitt)^2
    abweichungen[i] = abweichung
  }
  std_abw <- sqrt((sum(abweichungen))/nrow(df))
}


#------Dataframes Kafka-----------------------------------------------------------------------------------------


verwandlung_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dieverwandlung_full.txt")
derbau_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\derbau_full.txt")
heizer_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\heizer_full.txt")
landarzt_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\landarzt_full.txt")
betrachtung_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\betrachtung_full.txt")

prozess_sample1_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozess_sample1.txt")
prozess_sample2_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozess_sample2.txt")
prozess_sample3_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozess_sample3.txt")
prozess_sample4_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozess_sample4.txt")
prozessfull_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozessfull.txt")

schloss_sample1_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_sample1.txt")
schloss_sample2_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_sample2.txt")
schloss_sample3_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_sample3.txt")
schloss_sample4_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_sample4.txt")
schloss_sample5_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_sample5.txt")
schloss_sample6_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_sample6.txt")
dasschlossfull_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dasschloss_full.txt")

schloss_detail <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\schloss_detail.txt")

sampleamerika1_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_sample_1.txt")
sampleamerika2_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_sample_2.txt")
sampleamerika3_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_sample_3.txt")
sampleamerika4_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_sample_4.txt")
sampleamerika5_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_sample_5.txt")
amerikafull_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_full.txt")


#------Dataframes Vergleich-----------------------------------------------------------------------------------------


mann_novellen <- zumdf_express("^e_hmann1\\.txt$")
diekleinestadt <- zumdf_express("^e_hmann([2-9]|1[0-5])\\.txt$")
pippospano <- zumdf_express("^e_hmann(1[6-7])\\.txt$")
bergemeere <- zumdf_express("^e_doeblin([1-9]|1[0-1])\\.txt$")
freundinnenundgiftmord <- zumdf_express("^e_doeblin(1[2-3])\\.txt$")
zoeglingtoerleß <- zumdf_express("^e_musil([1-2])\\.txt$")
sternheim_geschichten <- zumdf_express("^e_sternheim([1-4])\\.txt$")
walser_geschichten <- zumdf_express("^e_walser1\\.txt$")
spaziergang <- zumdf_express("^e_walser([2-3])\\.txt$")
dergehuelfe <- zumdf_express("^e_walser([4-9]|1[0-2])\\.txt$")
dermoerder <- zumdf_express("^e_werfel([1-4])\\.txt$")

bahnwaerterthiel <- zumdf_moderne("^m_hauptmann([1-2])\\.txt$")
ketzervonsoana <- zumdf_moderne("^m_hauptmann([3-6])\\.txt$") 
griechischerfruehling <- zumdf_moderne("^m_hauptmann([7-9]|1[0-1])\\.txt$")
schnitzler_kurzgeschichten <- zumdf_moderne("^m_schnitzler([1-4])\\.txt$")
casanovasheimkehr <- zumdf_moderne("^m_schnitzler([5-8])\\.txt$")
gottgeschichten <- zumdf_moderne("^m_rilke([3-4])\\.txt$")
pragergeschichten <- zumdf_moderne("^m_rilke([1-2])\\.txt$")
dalmatinischereise <- zumdf_moderne("^m_bahr([1-5])\\.txt$")
brennendesgeheimnis <- zumdf_moderne("^m_zweig([1-3])\\.txt$")
dersaenger <- zumdf_moderne("^m_huch1\\.txt$")
hahnquakenbrueck <- zumdf_moderne("^m_huch2\\.txt")
derfallderuga <- zumdf_moderne("^m_huch([3-9]|10)\\.txt$")


#------Dataframes Weitere-----------------------------------------------------------------------------------------


dt_verwandlung <- differenz_haeufigkeiten(verwandlung_df)
dt_amerikafull <- differenz_haeufigkeiten(amerikafull_df)
dt_dasschlossfull <- differenz_haeufigkeiten(dasschlossfull_df)
dt_samplederprozess <- differenz_haeufigkeiten(samplederprozess_df)
dt_derbau <- differenz_haeufigkeiten(derbau_df)
dt_steppenwolf <- differenz_haeufigkeiten(steppenwolf_df)
dt_bergemeere <- differenz_haeufigkeiten(bergemeere)
dt_casanovasheimkehr <- differenz_haeufigkeiten(casanovasheimkehr)
dt_freundinnenundgiftmord <- differenz_haeufigkeiten(freundinnenundgiftmord)
dt_gottgeschichten <- differenz_haeufigkeiten(gottgeschichten)
dt_schnitzler_kurzgeschichten <- differenz_haeufigkeiten(schnitzler_kurzgeschichten)
dt_pragergeschichten <- differenz_haeufigkeiten(pragergeschichten)
dt_zoeglingtoerleß <- differenz_haeufigkeiten(zoeglingtoerleß)

d_amerika <- differenzen(amerikafull_df)
d_prozess <- differenzen(prozessfull_df)
d_dersaenger <- differenzen(dersaenger)
d_deruga <- differenzen(derfallderuga)

hist(d_amerika$differenz, breaks = 100)
hist(d_dersaenger$differenz, breaks = 100)
hist(d_deruga$differenz, breaks = 100)

hist(dalmatinischereise$satzlaenge, breaks = 100)

ausg_texte <- readtext(file.path("C:\\Users\\Diana\\Documents\\kafka\\ausgegl_vergleichscorpus", "*.txt"))
ausg_saetze <- tokenize_sentence(paste(ausg_texte$text, collapse = " ")) [[1]]
ausg_satzlaenge <- sapply(ausg_saetze, function(s) {
  length(tokenize_words(s)[[1]])
})
ausg_vergleichswerke <- data.frame(satz = array(1:length(ausg_satzlaenge)), inhalt = ausg_saetze, satzlaenge = ausg_satzlaenge)


std_abweichungen <- data.frame(name = c("amerika", "verwandlung", "prozess", "schloss", "bau", "heizer", 
                                        "landarzt", "betrachtung", "bergemeere", "casanova", "gottgeschichten", "zoeglingtoerleß", 
                                        "freundinnen und giftmord", "kurzgeschichten schnitzler",
                                        "prager geschichten", "dalmatinische reise", "griechische reise", 
                                        "bahnwaerter thiel", "ketzer von soana", "brennendes geheimnis", 
                                        "fall deruga", "hahn quakenbrueck", "der saenger" ),
                               std_abweichung = c(std_abweichung(amerikafull_df), std_abweichung(verwandlung_df),
                                                  std_abweichung(prozessfull_df), std_abweichung(dasschlossfull_df),
                                                  std_abweichung(derbau_df), std_abweichung(heizer_df), 
                                                  std_abweichung(landarzt_df), std_abweichung(betrachtung_df),
                                                  std_abweichung(bergemeere), std_abweichung(casanovasheimkehr),
                                                  std_abweichung(gottgeschichten), std_abweichung(zoeglingtoerleß),
                                                  std_abweichung(freundinnenundgiftmord), std_abweichung(schnitzler_kurzgeschichten), 
                                                  std_abweichung(pragergeschichten),
                                                  std_abweichung(dalmatinischereise), std_abweichung(griechischerfruehling),
                                                  std_abweichung(bahnwaerterthiel), std_abweichung(ketzervonsoana),
                                                  std_abweichung(brennendesgeheimnis), std_abweichung(derfallderuga),
                                                  std_abweichung(hahnquakenbrueck), std_abweichung(dersaenger)))


std_abweichungen <- data.frame(
  name = c(
    "amerika", "verwandlung", "prozess", "schloss", "bau", "heizer",
    "landarzt", "betrachtung", "bergemeere", "casanova", "gottgeschichten", "zoeglingtoerleß",
    "freundinnen und giftmord", "kurzgeschichten schnitzler", "praeger geschichten", 
    "dalmatinische reise", "griechische reise", "bahnwaerter thiel", "ketzer von soana", 
    "brennendes geheimnis",
    "mann novellen", "die kleine stadt", "pippospano", "sternheim geschichten",
    "walser geschichten", "spaziergang", "der gehuelpe", "der moerder", 
    "der fall deruga", "hahn quakenbrueck", "der saenger"
  ),
  std_abweichung = c(
    std_abweichung(amerikafull_df), std_abweichung(verwandlung_df),
    std_abweichung(prozessfull_df), std_abweichung(dasschlossfull_df),
    std_abweichung(derbau_df), std_abweichung(heizer_df),
    std_abweichung(landarzt_df), std_abweichung(betrachtung_df),
    std_abweichung(bergemeere), std_abweichung(casanovasheimkehr),
    std_abweichung(gottgeschichten), std_abweichung(zoeglingtoerleß),
    std_abweichung(freundinnenundgiftmord), std_abweichung(schnitzler_kurzgeschichten),
    std_abweichung(pragergeschichten), std_abweichung(dalmatinischereise),
    std_abweichung(griechischerfruehling), std_abweichung(bahnwaerterthiel),
    std_abweichung(ketzervonsoana), std_abweichung(brennendesgeheimnis),
    std_abweichung(mann_novellen), std_abweichung(diekleinestadt),
    std_abweichung(pippospano), std_abweichung(sternheim_geschichten),
    std_abweichung(walser_geschichten), std_abweichung(spaziergang),
    std_abweichung(dergehuelfe), std_abweichung(dermoerder),
    std_abweichung(derfallderuga), std_abweichung(hahnquakenbrueck),
    std_abweichung(dersaenger)
  )
)

?sd   

#-------Readability & Lexical Diversity --------------------------------------------------------------------------

#für Readability Corpus, für lex div TTR dfm, für lex div MATTR tokens
#mit MATTR tatsächlich deutlich höher als mit TTR

#"Flesch" -> Flesch-Reading-Ease, auch für deutsche Texte, nWS speziell für deutsche Texte, SMOG.de für deutche Texte

betrachtung_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\betrachtung_full.txt")

c_verwandlung <- corpus(readtext(file.path("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dieverwandlung_full.txt")))
dfm_verwandlung <- zudfm("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dieverwandlung_full.txt")
tokens_verwandlung <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dieverwandlung_full.txt")

c_prozess <- corpus(readtext(file.path(("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozessfull.txt"))))
dfm_prozess <- zudfm("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozessfull.txt")
tokens_prozess <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozessfull.txt")

c_schloss <- corpus(readtext(file.path(("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dasschloss_full.txt"))))
dfm_schloss <- zudfm("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dasschloss_full.txt")
tokens_schloss <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dasschloss_full.txt")

c_amerika <- corpus(readtext(file.path(("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_full.txt"))))
dfm_amerika <- zudfm("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_full.txt")
tokens_amerika <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_full.txt")

c_derbau <- corpus(readtext(file.path("C:\\Users\\Diana\\Documents\\kafka\\weitere\\derbau_full.txt")))
tokens_derbau <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\derbau_full.txt")

c_heizer <- corpus(readtext(file.path("C:\\Users\\Diana\\Documents\\kafka\\weitere\\heizer_full.txt")))
tokens_heizer <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\heizer_full.txt")

c_landarzt <- corpus(readtext(file.path("C:\\Users\\Diana\\Documents\\kafka\\weitere\\landarzt_full.txt")))
tokens_landarzt <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\landarzt_full.txt")

c_betrachtung <- corpus(readtext(file.path("C:\\Users\\Diana\\Documents\\kafka\\weitere\\betrachtung_full.txt")))
tokens_betrachtung <- zutokens("C:\\Users\\Diana\\Documents\\kafka\\weitere\\betrachtung_full.txt")



readability_full <- textstat_readability(c(c_verwandlung, c_prozess, c_schloss, c_amerika, c_derbau, c_heizer, c_landarzt,
                                           c_betrachtung), measure = c("Flesch", "nWS", "SMOG.de"))
lexdiv_full <- textstat_lexdiv(c(tokens_verwandlung, tokens_prozess, tokens_schloss, tokens_amerika, tokens_derbau, tokens_heizer,
                                 tokens_landarzt, tokens_betrachtung), measure = "MATTR")

readability_df <- as.data.frame(readability_full)
readability_mean <- data.frame(document = "mean", Flesch = sum(readability_df$Flesch)/nrow(readability_df), 
                               nWS = sum(readability_df$nWS)/nrow(readability_df),
                               SMOG.de = sum(readability_df$SMOG.de)/nrow(readability_df))
readability_df <- bind_rows(readability_df, readability_mean)

lexdiv_df <- as.data.frame(lexdiv_full)
lexdiv_mean <- data.frame(document = "mean", MATTR = sum(lexdiv_df$MATTR)/nrow(lexdiv_df))
lexdiv_df <- bind_rows(lexdiv_df, lexdiv_mean)


#FRE = 43.1 -> schwer, nWSTF = 9.1 -> 9. Klasse, SMOG(de) = 8.5 -> 8. Klasse



#-------t-Test zur Überprüfung des Unterschieds --------------------------------------------------------------------------

#Dataframe für alle Kafka + alle Vergleichswerke 
allekafka <- bind_rows(verwandlung_df, derbau_df, heizer_df, betrachtung_df, 
                       prozessfull_df, amerikafull_df, dasschlossfull_df) |> as_tibble()

e_vergleichswerke <- zumdf_express("\\.txt$")
m_vergleichswerke <- zumdf_moderne("\\.txt$")

allevergleichswerke <- bind_rows(m_vergleichswerke, e_vergleichswerke) |> as_tibble()


t.test(allekafka$satzlaenge, m_vergleichswerke$satzlaenge)

t.test(allekafka$satzlaenge, e_vergleichswerke$satzlaenge)

t.test(allekafka$satzlaenge, allevergleichswerke$satzlaenge)

t.test(allekafka$satzlaenge, ausg_vergleichswerke$satzlaenge)


hist(allekafka$satzlaenge, breaks = 100)
hist(m_vergleichswerke$satzlaenge, breaks = 100)
hist(e_vergleichswerke$satzlaenge, breaks = 100)
hist(allevergleichswerke$satzlaenge, breaks = 100)

#Corpus Expressionismus größer als Moderne, aber alle Autoren Expressionismus auch Vertreter der Moderne
#deshalb okay?

#evtl Verhältnis der Autoren untereinander anpassen, weniger text dafür besserer Querschnitt
#Vielleicht Trennung M und E nur für Stilometrie und für Test ausgeglichener Gesamtcorpus

#-------Abweichungen---------------------------------------------------------------------------------------------

std_abweichungen$name <- factor(std_abweichungen$name, levels = std_abweichungen$name)

sd_kafka <- std_abweichung(allekafka)
sd_andere <- std_abweichung(alle_anderen)

ggplot(std_abweichungen, aes(x = name, y = std_abweichung, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Standardabweichung", y = "Standardabweichung", x = "Werk") +
  theme_classic()

?geom_bar

#-------Punktdiagramme---------------------------------------------------------------------------------------------

#Kafka

zumplot(verwandlung_df)
zumplot(derbau_df)

zumplot(prozess_sample1_df)
zumplot(prozess_sample2_df)
zumplot(prozess_sample3_df)
zumplot(prozess_sample4_df)
zumplot(prozessfull_df) +
  geom_smooth(method = "loess", span = 0.1, level = 0.9999)

zumplot(schloss_sample1_df)
zumplot(schloss_sample2_df)
zumplot(schloss_sample3_df)
zumplot(schloss_sample4_df)
zumplot(schloss_sample5_df)
zumplot(schloss_sample6_df)

zumplot(dasschlossfull_df) +
  geom_smooth(method = "loess", span = 0.05, level = 0.9999)

zumplot(schloss_detail)

#duplicated(dasschlossfull_df$inhalt)
#view(dasschlossfull_df[duplicated(dasschlossfull_df$inhalt), ])

zumplot(sampleamerika1_df)
zumplot(sampleamerika2_df)
zumplot(sampleamerika3_df)
zumplot(sampleamerika4_df)
zumplot(sampleamerika5_df)
zumplot(amerikafull_df)


#Vergleich

zumplot(bergemeere)
zumplot(casanovasheimfahrt)
zumplot(freundinnenundgiftmord)
zumplot(gottgeschichten)
zumplot(pragergeschichten)
zumplot(schnitzler_kurzgeschichten)
zumplot(zoeglingtoerleß)
zumplot(dersaenger)


#----------Liniendiagramme--------------------------------------------------------------------------------------------


ggplot(data = verwandlung_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 1) +
  scale_y_continuous(limits = c(5, 100))

ggplot(data = amerikafull_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.5)

ggplot(data = dasschlossfull_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.5)

ggplot(data = amerikafull_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", span = 1)
ggplot(data = dasschlossfull_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", span = 0.1)
ggplot(data = verwandlung_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", span = 0.1)
ggplot(data = derbau_df, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", span = 0.1)
ggplot(data = bergemeere, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", span = 0.1)
ggplot(data = casanovasheimkehr, aes(x=satz, y=satzlaenge)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", span = 0.1)

ggplot(data = differenzen(amerikafull_df), aes(x=satz, y=differenz)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth()
ggplot(data = differenzen(bergemeere), aes(x=satz, y=differenz)) +
  geom_line(linetype = 1, color = "steelblue", linewidth = 0.7) +
  geom_smooth()

dummy_df <- data.frame(satz = array(1:10), 
                       inhalt = c("a", "b", "c", "d", "e", "a", "b", "c", "d", "e"),
                       satzlaenge = c(5, 4, 6, 7, 2, 9, 4, 3, 8, 1))

view(differenzen(casanovasheimkehr))
