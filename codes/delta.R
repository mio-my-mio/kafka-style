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
library("tokenizers")

#---------------------------------------------------------------------------------------------------------------

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

gruppiert_upd <- function(df){
  gruppen <- vector("character", length = nrow(df))
  for(i in 1:nrow(df)) {
    wert <- df$satzlaenge[i]
    if(wert<6){
      gruppen[i] = "a" }
    else if(wert<11){
      gruppen[i] = "b" } 
    else if(wert<21){
      gruppen[i] = "c" }  
    else if(wert<31){
      gruppen[i] = "d" } 
    else if(wert<41){
      gruppen[i] = "e" }
    else if(wert<51){
      gruppen[i] = "f" }
    else if(wert<76){
      gruppen[i] = "g" } 
    else if(wert<101){
      gruppen[i] = "h" } 
    else {
      gruppen[i] = "i" } 
  }
  df <- data.frame(satz = df$satz, satzlaenge = df$satzlaenge, gruppe = gruppen)
}

muster_upd <- function(df, laenge) {
  a <- gruppiert_upd(df)
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

vorkommen_upd <- function(df, x, y) {
  j = 1
  m = as.data.frame(muster_upd(df, y*2+1))
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

#---------------------------------------------------------------------------------------------------------------

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


#---------------------------------------------------------------------------------------------------------------

write(muster_upd(verwandlung_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_verwandlung_ngram.txt", sep = " ")
write(muster_upd(derbau_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_derbau_ngram.txt", sep = " ")
write(muster_upd(heizer_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_heizer_ngram.txt", sep = " ")
write(muster_upd(landarzt_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_landarzt_ngram.txt", sep = " ")
write(muster_upd(betrachtung_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_betrachtung_ngram.txt", sep = " ")
write(muster_upd(prozessfull_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_prozess_ngram.txt", sep = " ")
write(muster_upd(dasschlossfull_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_schloss_ngram.txt", sep = " ")
write(muster_upd(amerikafull_df, 5), "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_amerika_ngram.txt", sep = " ")

write(muster_upd(mann_novellen, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_mannnov_ngram.txt", sep = " ")
write(muster_upd(diekleinestadt, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_kleinestadt_ngram.txt", sep = " ")
write(muster_upd(pippospano, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_pippospano_ngram.txt", sep = " ")
write(muster_upd(bergemeere, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_bergemeere_ngram.txt", sep = " ")
write(muster_upd(freundinnenundgiftmord, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_giftmord_ngram.txt", sep = " ")
write(muster_upd(zoeglingtoerleß, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_toerless_ngram.txt", sep = " ")
write(muster_upd(sternheim_geschichten, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_sternheim_ngram.txt", sep = " ")
write(muster_upd(walser_geschichten, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_walser_ngram.txt", sep = " ")
write(muster_upd(spaziergang, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_spaziergang_ngram.txt", sep = " ")
write(muster_upd(dergehuelfe, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_gehuelfe_ngram.txt", sep = " ")
write(muster_upd(dermoerder, 5), "C:\\Users\\Diana\\Desktop\\results\\exp\\e_moerder_ngram.txt", sep = " ")

write(muster_upd(bahnwaerterthiel, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_thiel_ngram.txt", sep = " ")
write(muster_upd(ketzervonsoana, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_soana_ngram.txt", sep = " ")
write(muster_upd(griechischerfruehling, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_fruehling_ngram.txt")
write(muster_upd(schnitzler_kurzgeschichten, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_schnitzler_ngram.txt")
write(muster_upd(casanovasheimkehr, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_casanova_ngram.txt", sep = " ")
write(muster_upd(gottgeschichten, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_gott_ngram.txt", sep = " ")
write(muster_upd(pragergeschichten, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_prager_ngram.txt", sep = " ")
write(muster_upd(dalmatinischereise, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_reise_ngram.txt", sep = " ")
write(muster_upd(brennendesgeheimnis, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_geheimnis_ngram.txt", sep = " ")
write(muster_upd(dersaenger, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_saenger_ngram.txt", sep = " ")
write(muster_upd(hahnquakenbrueck, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_hahn_ngram.txt", sep = " ")
write(muster_upd(derfallderuga, 5), "C:\\Users\\Diana\\Desktop\\results\\mod\\m_deruga_ngram.txt", sep = " ")

#---------------------------------------------------------------------------------------------------------------

stylo()
view(muster_upd(verwandlung_df, 3))

#---------------------------------------------------------------------------------------------------------------

write(vorkommen_upd(verwandlung_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_verwandlung_ngram.txt", sep = " ")
write(vorkommen_upd(derbau_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_derbau_ngram.txt", sep = " ")
write(vorkommen_upd(landarzt_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_landarzt_ngram.txt", sep = " ")
write(vorkommen_upd(betrachtung_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_betrachtung_ngram.txt", sep = " ")
write(vorkommen_upd(prozessfull_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_prozess_ngram.txt", sep = " ")
write(vorkommen_upd(dasschlossfull_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_schloss_ngram.txt", sep = " ")
write(vorkommen_upd(amerikafull_df, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\kafka\\k_amerika_ngram.txt", sep = " ")

write(vorkommen_upd(mann_novellen, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_mannnov_ngram.txt", sep = " ")
write(vorkommen_upd(diekleinestadt, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_kleinestadt_ngram.txt", sep = " ")
write(vorkommen_upd(pippospano, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_pippospano_ngram.txt", sep = " ")
write(vorkommen_upd(bergemeere, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_bergemeere_ngram.txt", sep = " ")
write(vorkommen_upd(freundinnenundgiftmord, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_giftmord_ngram.txt", sep = " ")
write(vorkommen_upd(zoeglingtoerleß, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_toerless_ngram.txt", sep = " ")
write(vorkommen_upd(sternheim_geschichten, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_sternheim_ngram.txt", sep = " ")
write(vorkommen_upd(walser_geschichten, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_walser_ngram.txt", sep = " ")
write(vorkommen_upd(spaziergang, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_spaziergang_ngram.txt", sep = " ")
write(vorkommen_upd(dergehuelfe, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_gehuelfe_ngram.txt", sep = " ")
write(vorkommen_upd(dermoerder, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\exp\\e_moerder_ngram.txt", sep = " ")

write(vorkommen_upd(bahnwaerterthiel, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_thiel_ngram.txt", sep = " ")
write(vorkommen_upd(ketzervonsoana, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_soana_ngram.txt", sep = " ")
write(vorkommen_upd(griechischerfruehling, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_fruehling_ngram.txt")
write(vorkommen_upd(schnitzler_kurzgeschichten, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_schnitzler_ngram.txt")
write(vorkommen_upd(casanovasheimkehr, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_casanova_ngram.txt", sep = " ")
write(vorkommen_upd(gottgeschichten, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_gott_ngram.txt", sep = " ")
write(vorkommen_upd(pragergeschichten, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_prager_ngram.txt", sep = " ")
write(vorkommen_upd(dalmatinischereise, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_reise_ngram.txt", sep = " ")
write(vorkommen_upd(brennendesgeheimnis, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_geheimnis_ngram.txt", sep = " ")
write(vorkommen_upd(dersaenger, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_saenger_ngram.txt", sep = " ")
write(vorkommen_upd(hahnquakenbrueck, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_hahn_ngram.txt", sep = " ")
write(vorkommen_upd(derfallderuga, "g", 3)$davor, "C:\\Users\\Diana\\Desktop\\results\\mod\\m_deruga_ngram.txt", sep = " ")

stylo()

#---------------------------------------------------------------------------------------------------------------

abfolgen <- function(df, x, y) {
  sorted <- df[order(df$satzlaenge, decreasing = TRUE), ]
  top_idx <- sorted$satz[1:x]
  alle <- vector("list", length = x)
  for(i in seq_len(x)) {
    a <- top_idx[i]
    start <- max(1, a-y)
    end <- a 
    sequ <- df$satzlaenge[start:end]
    if(length(sequ) < (y+1)) {
      sequ <- c(rep(NA, (y+1-length(sequ))), sequ)
    }
    alle[[i]] <- sequ 
  }
  return(alle)
}

#abändern, dass es die diferenz ausrechnet

top_diff <- function(df, x) {
  sorted <- df[order(df$satzlaenge, decreasing = TRUE), ]
  top_idx <- sorted$satz[1:x]
  vec <- vector("numeric", length = x)
  for(i in nrow(top_idx)) {
    wert <- df$satzlaenge[top_idx[i]]
    pre <- df$satzlaenge[top_idx[i]-1]
    differenz <- wert-pre
    vec[[i]] <- differenz
  }
  return(vec)
}

top_diff(verwandlung_df, 5)

folge_plotten <- function(x) {
  df <- do.call(rbind, lapply(seq_along(x), function(i) {
    data.frame(
      id = paste0("Seq", i),
      pos = seq_along(x[[i]]),
      wert = x[[i]]
    )
  }))
  ggplot(df, aes(x = pos, y = wert, group = id, colour = id)) +
    geom_line() +
    labs(x = "Position in Fenster", y = "Satzlänge") +
    theme_classic() +
    theme(legend.position = "none") 
}


folge_plotten(abfolgen(verwandlung_df, 50, 3))
folge_plotten(abfolgen(amerikafull_df, 50, 3))
folge_plotten(abfolgen(prozessfull_df, 50, 3)) #fällt auch in stilo raus glaube #lüge
folge_plotten(abfolgen(dasschlossfull_df, 50, 3)) #das crazy
folge_plotten(abfolgen(derbau_df, 50,3))
folge_plotten(abfolgen(betrachtung_df, 50, 3))
folge_plotten(abfolgen(landarzt_df, 50, 3))

folge_plotten(abfolgen(mann_novellen, 20, 3))
folge_plotten(abfolgen(diekleinestadt, 50, 3)) 
folge_plotten(abfolgen(pippospano, 20, 3))
folge_plotten(abfolgen(bergemeere, 20, 3))
folge_plotten(abfolgen(freundinnenundgiftmord, 20, 3))
folge_plotten(abfolgen(zoeglingtoerleß, 50, 3)) #100
folge_plotten(abfolgen(sternheim_geschichten, 20, 3))
folge_plotten(abfolgen(walser_geschichten, 20, 3)) #drei der längsten sätze folgen unmittelbar aufeinander (cool)
folge_plotten(abfolgen(spaziergang, 50, 3)) #200
folge_plotten(abfolgen(dergehuelfe, 50, 3)) #100
folge_plotten(abfolgen(dermoerder, 20, 3))

folge_plotten(abfolgen(bahnwaerterthiel, 20, 3))
folge_plotten(abfolgen(ketzervonsoana, 50, 3)) #90
folge_plotten(abfolgen(griechischerfruehling, 50, 3)) #100
folge_plotten(abfolgen(schnitzler_kurzgeschichten, 50, 3)) #100
folge_plotten(abfolgen(casanovasheimkehr, 50, 3)) #200
folge_plotten(abfolgen(gottgeschichten, 20, 3))
folge_plotten(abfolgen(pragergeschichten, 50, 3)) #100
folge_plotten(abfolgen(dalmatinischereise, 50, 3)) #150
folge_plotten(abfolgen(brennendesgeheimnis, 20, 3))
folge_plotten(abfolgen(dersaenger, 20, 3))
folge_plotten(abfolgen(hahnquakenbrueck, 50, 3)) #100
folge_plotten(abfolgen(derfallderuga, 20, 3))

folge_plotten(abfolgen(verwandlung_df, 50, 1))
folge_plotten(abfolgen(dalmatinischereise, 50, 1))
