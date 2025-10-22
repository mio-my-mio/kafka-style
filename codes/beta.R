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


#-----Dataframe Funktionen--------------------------------------------------------------------------------------

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


#------Zählen Funktionen----------------------------------------------------------------------------------------

gesamt <- function(text) {
  str_count(text, "\\w+")
}

komma_counter <- function(text) {
  str_count(text, ",")
}

satz_counter <- function(text) {
  saetze <- tokenize_sentence(text) [[1]]
  count <- length(saetze)
}

#---Satzlängen Funktionen---------------------------------------------------------------------------------------


naechster_plot <- function(df){
  gerade <- vector("numeric", length = nrow(df))
  danach <- vector("numeric", length = nrow(df))
  for(i in 1:nrow(df)) {
    wert <- df$satzlaenge[i]
    naechster <- df$satzlaenge[i+1]
    gerade[i] = wert
    if(is.na(naechster)){
      danach[i] = 0
    } else{
      danach[i] = naechster
    }
  }
  dataf <- data.frame(satz = gerade, folgend = danach)
  ggplot(data = dataf, aes(x=satz, y=folgend)) +
    geom_point() +
    labs(title = deparse(substitute(df)))
}

gruppiert <- function(df){
  gruppen <- vector("character", length = nrow(df))
  for(i in 1:nrow(df)) {
    wert <- df$satzlaenge[i]
    if(wert<11){
      gruppen[i] = "h" }
    else if(wert<21){
      gruppen[i] = "g" }  
    else if(wert<41){
      gruppen[i] = "f" } 
    else if(wert<61){
      gruppen[i] = "e" }
    else if(wert<81){
      gruppen[i] = "d" }
    else if(wert<101){
      gruppen[i] = "c" } 
    else if(wert<151){
      gruppen[i] = "b" } 
    else {
      gruppen[i] = "a" } 
  }
  df <- data.frame(satz = df$satz, satzlaenge = df$satzlaenge, gruppe = gruppen)
}

muster <- function(df, laenge) {
  a <- gruppiert(df)
  gruppen <- as.vector(a$gruppe, mode = "character")
  muster <- vector(mode = "character", length = nrow(df)-laenge+1)
  for(i in 1:(nrow(df)-laenge+1)) {
    x <- vector(mode = "character", length = laenge)
    for(j in 1:laenge){
      x[j] = gruppen[i+j-1]
    }
    muster[i] = paste0(x, collapse = "")
  }
  return(muster)
}

mfp <- function(df, x) {
  haeufigkeiten <- table(muster(df, x))
  dataf <- as.data.frame(haeufigkeiten)
  colnames(dataf) <- c("muster", "haeufigkeit")
  dataf <- dataf[order(dataf$haeufigkeit, decreasing = TRUE), ]
  dataf <- data.frame(rang = array(1:length(haeufigkeiten)), muster = dataf$muster, hauefigkeit = dataf$haeufigkeit)
}

vorkommen <- function(df, x, y) {
  j = 1
  m = as.data.frame(muster(df, y*2+1))
  pre = vector(mode ="character", length = nrow(df))
  post = vector(mode = "character", length = nrow(df))
  for (i in 1:(nrow(df))) {
    pattern <- as.String(m$muster[i]) 
    if(x == substr(pattern, y+1, y+1)) {
      davor <- substr(pattern, 1, y)
      danach <- substr(pattern, y+2, y*2+1)
      pre[j] = davor
      post[j] = danach
      j = j+1
    }
  }
  pre <- pre[pre != 0]
  post <- post[post != 0]
  length(pre) <- j-1
  length(post) <- j-1
  dataf <- data.frame(davor = pre, danach = post)
}

view(vorkommen(verwandlung_df, "b", 3))

view(muster(allekafka, 4))

view(mfp(allekafka, 4))


#---------------------------------------------------------------------------------------------------------------

#methode_test <- "Hier. sind. drei.. Punkte. [.]"
#ergebnistest <- punkt_counter(methode_test)

#AUSGABE: 3 METHODE FUNKTIONIERT KORREKT

kommas_kafkagesamt <- sum(map_int(kafkagesamt$text, komma_counter))
kommas_kafkawerke <- sum(map_int(kafkawerke$text, komma_counter))
kommas_kafkabriefe <- sum(map_int(kafkabriefe$text, komma_counter))
kommas_anderegesamt <- sum(map_int(anderegesamt$text, komma_counter))
kommas_anderewerke <- sum(map_int(anderewerke$text, komma_counter))
kommas_anderebriefe <- sum(map_int(anderebriefe$text, komma_counter))


saetze_kafkagesamt <- sum(map_int(kafkagesamt$text, satz_counter))
saetze_kafkawerke <- sum(map_int(kafkawerke$text, satz_counter))
saetze_kafkabriefe <- sum(map_int(kafkabriefe$text, satz_counter))
saetze_anderegesamt <- sum(map_int(anderegesamt$text, satz_counter))
saetze_anderewerke <- sum(map_int(anderewerke$text, satz_counter))
saetze_anderebriefe <- sum(map_int(anderebriefe$text, satz_counter))


kommas_pro_satz_kg <- kommas_kafkagesamt/saetze_kafkagesamt
kommas_pro_satz_kb <- kommas_kafkabriefe/saetze_kafkabriefe
kommas_pro_satz_kw <- kommas_kafkawerke/saetze_kafkawerke
kommas_pro_satz_ag <- kommas_anderegesamt/saetze_anderegesamt
kommas_pro_satz_ab <- kommas_anderebriefe/saetze_anderebriefe
kommas_pro_satz_aw <- kommas_anderewerke/saetze_anderewerke


#Ergebnis insgesamt für Kafka: 2.363 Andere: 1.333
#Ergebnisse für Kafkas Werke: 2.381 Kafkas Briefe: 2.221
#Ergebnisse für Andere Werke: 1.307 Andere Briefe: 1.378

#Kafka nutzt ungefähr doppelt so viele Kommas wie andere Autoren
#Kafka hat durchschnittlich zwei Kommas pro Satz
#Andere haben durchschnittlich ein Komma pro Satz


verwandlung_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dieverwandlung_full.txt")
derbau_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\derbau_full.txt")
heizer_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\heizer_full.txt")
landarzt_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\landarzt_full.txt")
betrachtung_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\betrachtung_full.txt")
prozessfull_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\prozessfull.txt")
dasschlossfull_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\dasschloss_full.txt")
amerikafull_df <- zumdataframe("C:\\Users\\Diana\\Documents\\kafka\\weitere\\amerika_full.txt")

allekafka <- bind_rows(verwandlung_df, derbau_df, heizer_df, betrachtung_df, 
                       prozessfull_df, amerikafull_df, dasschlossfull_df) |> as_tibble()

vorkommen

hmann_werke <- zumdf_express("^e_hmann.+\\.txt$")
doeblin_werke <- zumdf_express("^e_doeblin.+\\.txt$")
musil_werke <- zumdf_express("^e_musil.+\\.txt$")
sternheim_werke <- zumdf_express("^e_sternheim.+\\.txt$")
walser_werke <- zumdf_express("^e_walser.+\\.txt$")
werfel_werke <- zumdf_express("^e_werfel.+\\.txt$")
hauptmann_werke <- zumdf_moderne("^m_hauptmann.+\\.txt$")
schnitzler_werke <- zumdf_moderne("^m_schnitzler.+\\.txt$")
rilke_werke <- zumdf_moderne("^m_rilke.+\\.txt$")
bahr_werke <- zumdf_moderne("^m_bahr.+\\.txt$")
zweig_werke <- zumdf_moderne("^m_zweig.+\\.txt$")
huch_werke <- zumdf_moderne("^m_huch.+\\.txt$")

alle_anderen <- bind_rows(hmann_werke, doeblin_werke, musil_werke, sternheim_werke,
                          walser_werke, werfel_werke, hauptmann_werke, schnitzler_werke,
                          rilke_werke, bahr_werke, zweig_werke, huch_werke) |> as_tibble()

kommas_hmann <- sum(map_int(hmann_werke$inhalt, komma_counter))
kommas_doeblin <- sum(map_int(doeblin_werke$inhalt, komma_counter))
kommas_musil <- sum(map_int(musil_werke$inhalt, komma_counter))
kommas_sternheim <- sum(map_int(sternheim_werke$inhalt, komma_counter))
kommas_walser <- sum(map_int(walser_werke$inhalt, komma_counter))
kommas_werfel <- sum(map_int(werfel_werke$inhalt, komma_counter))
kommas_hauptmann <- sum(map_int(hauptmann_werke$inhalt, komma_counter))
kommas_schnitzler <- sum(map_int(schnitzler_werke$inhalt, komma_counter))
kommas_rilke <- sum(map_int(rilke_werke$inhalt, komma_counter))
kommas_bahr <- sum(map_int(bahr_werke$inhalt, komma_counter))
kommas_zweig <- sum(map_int(zweig_werke$inhalt, komma_counter))
kommas_huch <- sum(map_int(huch_werke$inhalt, komma_counter))
kommas_kafka <- sum(map_int(allekafka$inhalt, komma_counter))

kommas_kafka_v <- map_int(allekafka$inhalt, komma_counter)
kommas_andere_v <- map_int(alle_anderen$inhalt, komma_counter)

kommas_pro_satz_kafka <- kommas_kafka/nrow(allekafka)
kommas_pro_satz_hmann <- kommas_hmann / nrow(hmann_werke)
kommas_pro_satz_doeblin <- kommas_doeblin / nrow(doeblin_werke)
kommas_pro_satz_musil <- kommas_musil / nrow(musil_werke)
kommas_pro_satz_sternheim <- kommas_sternheim / nrow(sternheim_werke)
kommas_pro_satz_walser <- kommas_walser / nrow(walser_werke)
kommas_pro_satz_werfel <- kommas_werfel / nrow(werfel_werke)
kommas_pro_satz_hauptmann <- kommas_hauptmann / nrow(hauptmann_werke)
kommas_pro_satz_schnitzler <- kommas_schnitzler / nrow(schnitzler_werke)
kommas_pro_satz_rilke <- kommas_rilke / nrow(rilke_werke)
kommas_pro_satz_bahr <- kommas_bahr / nrow(bahr_werke)
kommas_pro_satz_zweig <- kommas_zweig / nrow(zweig_werke)
kommas_pro_satz_huch <- kommas_huch / nrow(huch_werke)
kommas_pro_satz_andere <- kommas_andere / nrow(alle_anderen)

kommas_pro_satz_df <- data.frame(
  autor = c(
  "kafka", "hmann", "doeblin", "musil", "sternheim", "walser", "werfel",
  "hauptmann", "schnitzler", "rilke", "bahr", "zweig", "huch"),
  wert = c(
  kommas_pro_satz_kafka,
  kommas_pro_satz_hmann,
  kommas_pro_satz_doeblin,
  kommas_pro_satz_musil,
  kommas_pro_satz_sternheim,
  kommas_pro_satz_walser,
  kommas_pro_satz_werfel,
  kommas_pro_satz_hauptmann,
  kommas_pro_satz_schnitzler,
  kommas_pro_satz_rilke,
  kommas_pro_satz_bahr,
  kommas_pro_satz_zweig,
  kommas_pro_satz_huch
)
)

ggplot(kommas_pro_satz_df, aes(x = reorder(autor, wert), y = wert, fill = autor)) +
  geom_bar(stat = "identity") +
  labs(title = "Kommas pro Satz", y = "Autor", x = "Kommas") +
  theme_classic()

t.test(kommas_kafka_v, kommas_andere_v)

durchschnitt_sl <- data.frame(name = c("kafka", "hmann", "doeblin", "musil", "sternheim", "walser", "werfel",
                                       "hauptmann", "schnitzler", "rilke", "bahr", "zweig", "huch"),
                              wert = c(mean(allekafka$satzlaenge), mean(hmann_werke$satzlaenge), mean(doeblin_werke$satzlaenge),
                                            mean(musil_werke$satzlaenge), mean(sternheim_werke$satzlaenge), mean(walser_werke$satzlaenge),
                                            mean(werfel_werke$satzlaenge), mean(hauptmann_werke$satzlaenge), mean(schnitzler_werke$satzlaenge),
                                            mean(rilke_werke$satzlaenge), mean(bahr_werke$satzlaenge), mean(zweig_werke$satzlaenge), 
                                            mean(huch_werke$satzlaenge)))

ggplot(durchschnitt_sl, aes(x = reorder(name, wert), y = wert, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Durchschnittliche Satzlänge", y = "Autor", x = "Wortzahl") +
  theme_classic()

#---------------------------------------------------------------------------------------------------------------

#funktion die jede satzlänge mit einem buchstaben ersetzt, der für die gruppe steht
#funktion, die für bestimmte gruppe bestimmt, welche gruppe am häuigsten davor und danach kommt

#alle nötigen dataframes schon da, auf jede Werkesammlung vorkommen anwenden
#häufigsten präfix und postfix bestimmen? table, lieber alle darstellen?
#erst jeder autor einzeln, daran schon vergleichbar, dann ausgehend davon alle gemeinsam


naechster_plot(allekafka)
naechster_plot(alle_anderen)

naechster_plot(hmann_werke)
naechster_plot(doeblin_werke)
naechster_plot(musil_werke)
naechster_plot(sternheim_werke)
naechster_plot(walser_werke)
naechster_plot(werfel_werke)
naechster_plot(hauptmann_werke)
naechster_plot(schnitzler_werke)
naechster_plot(rilke_werke)
naechster_plot(bahr_werke)
naechster_plot(zweig_werke)
naechster_plot(huch_werke)

naechster_plot(amerikafull_df)
naechster_plot(prozessfull_df)
naechster_plot(dasschlossfull_df)
naechster_plot(verwandlung_df)


view(vorkommen(alle_anderen, "a", 1))

ggplot(as.data.frame(table(vorkommen(allekafka, "b", 1))), aes(x = davor, y = Freq, fill = danach)) +
  geom_bar(stat = "identity") +
  labs(title = "Satzlaengen vor b bei Kafka", y = "frequency", x = "davor") +
  theme_classic()

#Bei einzelnen Autoren zu wenig lange Sätze um daraus einen Schluss zu ziehen

ggplot(as.data.frame(table(vorkommen(alle_anderen, "b", 1))), aes(x = davor, y = Freq, fill = danach)) +
  geom_bar(stat = "identity") +
  labs(title = "Satzlaengen vor b bei Anderen", y = "frequency", x = "davor") +
  theme_classic()

# zu wenig Werte, da andere seltener als Kafka so lange Sätze nutzen