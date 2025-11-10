#Ausschnitt des gesamten Codes + die genutzten Libraries

library(tidyverse)
library(lsa)
library(reticulate)
reticulate::py_install("sentence-transformers")

model2 <- st$SentenceTransformer('sentence-transformers/paraphrase-multilingual-mpnet-base-v2')

#Modellierung Funktion mit Die Verwandlung
em_verwandlung_all <- model2$encode(paste(verwandlung_df$inhalt, collapse = " "))
em_verwandlung_all <- as.vector(em_verwandlung_all)

em_verwandlung_s <- model2$encode(verwandlung_df$inhalt)
em_verwandlung_s <- as.matrix(em_verwandlung_s)

dist_verwandlung <- sapply(1:nrow(verwandlung_df), function(i){
  1 - lsa::cosine(em_verwandlung_all, em_verwandlung_s[i, ])
})

em_verwandlung_df <- data.frame(satz = 1:nrow(verwandlung_df), cos_dist = dist_verwandlung)

hist(em_verwandlung_df$cos_dist, breaks = 100)

ggplot(em_verwandlung_df, aes(x = satz, y = cos_dist)) +
  geom_line() +
  labs(title = "Embedding: Die Verwandlung", subtitle = "Cosine Difference: Gesamtes Werk - Einzelne Sätze") +
  theme_classic(base_size = 14)


#Definition der Funktion nach dieser Art
book_sentence_dist <- function(df, model) {
  em_all <- model$encode(paste(df$inhalt, collapse = " "))
  em_all <- as.vector(em_all)
  em_s <- model2$encode(df$inhalt)
  em_s <- as.matrix(em_s)
  dist <- sapply(1:nrow(df), function(i){
    1 - lsa::cosine(em_all, em_s[i, ])
  })
  df <- data.frame(satz = 1:nrow(df), cos_dist = dist)
}

#Einzelbeispiele DFs
em_dist_verwandlung <- book_sentence_dist(verwandlung_df, model2)
em_dist_prozess <- book_sentence_dist(prozessfull_df, model2)
em_dist_schloss <- book_sentence_dist(dasschlossfull_df, model2)
em_dist_amerika <- book_sentence_dist(amerikafull_df, model2)
em_dist_bahnwerterthiel <- book_sentence_dist(bahnwaerterthiel, model2)
em_dist_ketzervonsoana <- book_sentence_dist(ketzervonsoana, model2)
em_dist_gehuelfe <- book_sentence_dist(dergehuelfe, model2)
em_dist_brennendesgeheimnis <- book_sentence_dist(brennendesgeheimnis, model2)
em_dist_dersaenger <- book_sentence_dist(dersaenger, model2)
em_dist_derfallderuga <- book_sentence_dist(derfallderuga, model2)
em_dist_spaziergang <- book_sentence_dist(spaziergang, model2)

#Einzelbeispiele Results - Histogramme
hist(em_dist_verwandlung, breaks = 100)
hist(em_dist_prozess, breaks = 100)
hist(em_dist_schloss, breaks = 100)
hist(em_dist_amerika, breaks = 100)
hist(em_dist_bahnwerterthiel, breaks = 100)
hist(em_dist_ketzervonsoana, breaks = 100)
hist(em_dist_gehuelfe, breaks = 100)
hist(em_dist_brennendesgeheimnis, breaks = 100)
hist(em_dist_dersaenger, breaks = 100)
hist(em_dist_derfallderuga, breaks = 100)
hist(em_dist_spaziergang, breaks = 100) 
