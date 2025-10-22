library(tidyverse)
library(tidytext)
library(text)
library(reticulate)
library(lsa)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(tokenizers)
reticulate::py_install("sentence-transformers")

st <- import("sentence_transformers")
np <- import("numpy")
model <- st$SentenceTransformer('all-MiniLM-L6-v2')

model2 <- st$SentenceTransformer('sentence-transformers/paraphrase-multilingual-mpnet-base-v2')

em <- model$encode(dasschlossfull_df$inhalt)
em_r <- np$asarray(em)

em_r

cos_dist <- function(x, y) {
  1-sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
}


#nrow(em_r) oder wie viele Sätze ab Beginn

distances <- sapply(2:200, function(i) {
  cos_dist(em_r[i-1, ], em_r[i, ])
})

distances

plot(distances, type = "l", main = "Semantische Änderung in Das Schloss (Satz 1-200)",
     xlab = "Satz", ylab = "Cosine Distance", lwd=2)


sem_dis <- function(df, model, x) {
  em <- model$encode(df$inhalt)
  em_r <- np$asarray(em)
  cos_dist <- function(x, y) {
    1-sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
  }
  distances <- sapply(2:x, function(i) {
    cos_dist(em_r[i-1, ], em_r[i, ])
  })
  return(distances)
}

verwandlung_dis <- sem_dis(verwandlung_df, model, 200)
schloss_dis <- sem_dis(dasschlossfull_df, model, 200)
sternheim_dis <- sem_dis(sternheim_geschichten, model, 200)
schnitzler_dis <- sem_dis(schnitzler_kurzgeschichten, model, 200)
rilke_dis <- sem_dis(gottgeschichten, model, 200)


plot(verwandlung_dis, type = "l", col = "blue", main = "Semantische Änderung im Vergleich")
lines(schloss_dis, col = "red")
lines(sternheim_dis, col = "yellow")
lines(schnitzler_dis, col = "green")
lines(rilke_dis, col = "lightgreen")
legend("bottomleft", legend = c("Verwandlung", "Schloss", "Sternheim", "Schnitzler", "Rilke"), 
       col = c("blue", "red", "yellow", "green", "lightgreen"), lwd = 2)

#Auschläge deutlich machen

em_matrix <- as.matrix(em)

diff_cos <- sapply(2:200, function(i){
  1- lsa::cosine(em_matrix[i-1, ], em_matrix[i, ])
})

mean_diff <- mean(diff_cos, na.rm = TRUE)
sd_diff <- sd(diff_cos, na.rm = TRUE)
th <- mean_diff + sd_diff

diff_df <- data.frame(satz = 2:(length(diff_cos) + 1), cos_dist = diff_cos, hoch = diff_cos > th)

ggplot(diff_df, aes(x = satz, y = cos_dist)) +
  geom_line() +
  geom_hline(yintercept = mean_diff, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = th, color = "red", linetype = "dotted") +
  geom_point(data = subset(diff_df, hoch), aes(x = satz, y = cos_dist), color = "red", size = 2) +
  labs(title = "Semantische Veränderung", subtitle = sprintf("Durchschnitt: %.3f  Schwelle: %.3f", mean_diff, th),
       x = "Satzposition", y = "Cosine-Distance") +
  theme_minimal(base_size = 14)

em_sternheim <- model$encode(sternheim_geschichten$inhalt)
em_sternheim <- as.matrix(em_sternheim)
diff_stern <- sapply(2:200, function(i){
  1- lsa::cosine(em_sternheim[i-1, ], em_sternheim[i, ])
})
mean_stern <-  mean(diff_stern, na.rm = TRUE)
sd_stern <- sd(diff_stern, na.rm = TRUE)
th_stern <- mean_stern + sd_stern
sterndiff_df <- data.frame(satz = 2:(length(diff_stern) + 1), cos_dist = diff_stern, hoch = diff_stern > th_stern)

ggplot(sterndiff_df, aes(x = satz, y = cos_dist)) +
  geom_line() +
  geom_hline(yintercept = mean_stern, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = th_stern, color = "red", linetype = "dotted") +
  geom_point(data = subset(sterndiff_df, hoch), aes(x = satz, y = cos_dist), color = "red", size = 2) +
  labs(title = "Semantische Veränderung", subtitle = sprintf("Durchschnitt: %.3f  Schwelle: %.3f", mean_stern, th_stern),
       x = "Satzposition", y = "Cosine-Distance") +
  theme_minimal(base_size = 14)

cosdist_df <- function(df, model, x) {
  em <- model$encode(df$inhalt)
  em <- as.matrix(em)
  diff <- sapply(2:x, function(i){
    1 - lsa::cosine(em[i-1, ], em[i, ])
  })
  mean <-  mean(diff, na.rm = TRUE)
  sd <- sd(diff, na.rm = TRUE)
  th <- mean + sd
  df <- data.frame(satz = 2:(length(diff) + 1), inhalt = df$inhalt[2:nrow(df)], cos_dist = diff, hoch = diff > th)
}

view(cosdist_df(verwandlung_df, model, nrow(verwandlung_df)))
view(cosdist_df(verwandlung_df, model2, nrow(verwandlung_df)))

plot_cosdist <- function(df, model, x) {
  em <- model$encode(df$inhalt)
  em <- as.matrix(em)
  diff <- sapply(2:x, function(i){
    1 - lsa::cosine(em[i-1, ], em[i, ])
  })
  mean <-  mean(diff, na.rm = TRUE)
  sd <- sd(diff, na.rm = TRUE)
  th <- mean + (2*sd)
  df <- data.frame(satz = 2:(length(diff) + 1), cos_dist = diff, hoch = diff > th)
  ggplot(df, aes(x = satz, y = cos_dist)) +
    geom_line() +
    geom_hline(yintercept = mean, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = th, color = "red", linetype = "dotted") +
    geom_point(data = subset(df, hoch), aes(x = satz, y = cos_dist), color = "red", size = 2) +
    labs(title = "Semantische Veränderung", subtitle = sprintf("Durchschnitt: %.3f  Schwelle: %.3f", mean, th),
         x = "Satzposition", y = "Cosine-Distance") +
    theme_minimal(base_size = 14)
}

#alle als png in ordner first tryout

plot_cosdist(dasschlossfull_df, model2, nrow(dasschlossfull_df))
plot_cosdist(derfallderuga, model2, nrow(derfallderuga))

plot_cosdist(dasschlossfull_df, model2, nrow(dasschlossfull_df))
plot_cosdist(amerikafull_df, model2, nrow(amerikafull_df))
plot_cosdist(prozessfull_df, model2, nrow(prozessfull_df))
plot_cosdist(verwandlung_df, model2, nrow(verwandlung_df))
plot_cosdist(derbau_df, model2, nrow(derbau_df))

plot_cosdist(mann_novellen, model2, nrow(mann_novellen))
plot_cosdist(diekleinestadt, model2, nrow(diekleinestadt))
plot_cosdist(pippospano, model2, nrow(pippospano))
plot_cosdist(bergemeere, model2, nrow(bergemeere))
plot_cosdist(freundinnenundgiftmord, model2, nrow(freundinnenundgiftmord))
plot_cosdist(zoeglingtoerleß, model2, nrow(zoeglingtoerleß))
plot_cosdist(sternheim_geschichten, model2, nrow(sternheim_geschichten))
plot_cosdist(walser_geschichten, model2, nrow(walser_geschichten))
plot_cosdist(spaziergang, model2, nrow(spaziergang))
plot_cosdist(dergehuelfe, model2, nrow(dergehuelfe))
plot_cosdist(dermoerder, model2, nrow(dermoerder))
plot_cosdist(bahnwaerterthiel, model2, nrow(bahnwaerterthiel))
plot_cosdist(ketzervonsoana, model2, nrow(ketzervonsoana))
plot_cosdist(griechischerfruehling, model2, nrow(griechischerfruehling))
plot_cosdist(schnitzler_kurzgeschichten, model2, nrow(schnitzler_kurzgeschichten))
plot_cosdist(casanovasheimkehr, model2, nrow(casanovasheimkehr))
plot_cosdist(gottgeschichten, model2, nrow(gottgeschichten))
plot_cosdist(pragergeschichten, model2, nrow(pragergeschichten))
plot_cosdist(dalmatinischereise, model2, nrow(dalmatinischereise))
plot_cosdist(brennendesgeheimnis, model2, nrow(brennendesgeheimnis))
plot_cosdist(dersaenger, model2, nrow(dersaenger))
plot_cosdist(hahnquakenbrueck, model2, nrow(hahnquakenbrueck))
plot_cosdist(derfallderuga, model2, nrow(derfallderuga))


#berechnen wie oft die cosine distance wert höher als 1 ist

cosdist_df(amerikafull_df)$cosdist > 1

# eher weniger

#cosine distance der word embeddings jedes wortes eines satzes, durchschnittliches embedding innerhab jedes satzes + max embedding

word_em <- function(df, model) {
  words <- tokenize_words(df$inhalt, lowercase = TRUE, strip_punct = TRUE)
  index <- nrow(df)
  embeddings = map(words, function(words) {
    if (length(words[[1]]) < 2) return(NULL)
    model$encode(words[[1]])})
  distances = map(embeddings, function(emb) {
    if (is.null(emb)) return(NA)
    sapply(1:(nrow(emb) - 1), function(i) {
      cos_dist(emb[i, ], emb[i + 1, ])
    })
  })
  means <- map(embeddings, mean())
  maxs <- map(embeddings, max())
  result <- data.frame(satz = index, inhalt = df$inhalt, mean = means, max = maxs)
  return(result)
} 

word_em(verwandlung_df, model)




