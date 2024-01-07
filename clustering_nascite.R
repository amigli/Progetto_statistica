# CLUSTERING SUL DATASET DELLE NASCITE
# Author: Daniela Amendola

library(readxl)
library(openxlsx)

data <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

View(data)

# Si estraggono dal datafram i nomi dei Paesi
nome_paesi <- data$Country
nome_paesi <- sub(" \\(People's Republic of\\)", "", data$Country) # Modifico il nome della Cina

# Si imposta l'opzione per evitare la notazione scientifica nei grafici
options(scipen = 999)

################################################################################
#------------------------- Matrice delle distanze -----------------------------#
################################################################################

# Si calcola la matrice delle distanze tramite la metrica euclidea
distanzaEuclidea <- dist(data[, -1], method = "euclidean")
distanzaEuclidea

# Si converte la matrice delle distanze in un dataframe
distanzaEuclidea_df <- as.data.frame(as.matrix(distanzaEuclidea))

# Si aggiungono i nomi dei Paesi alle righe e alle colonne
rownames(distanzaEuclidea_df) <- nome_paesi
colnames(distanzaEuclidea_df) <- nome_paesi

View(distanzaEuclidea_df)

# Si salva la matrice delle distanze euclidee in un file Excel
write.xlsx(distanzaEuclidea_df, "matrice_distanze_nascite.xlsx", rowNames = TRUE)

################################################################################
#----------------------- Misura di non omogeneità totale ----------------------#
################################################################################

n <- nrow(data[, -1]) # numero di righe nel df
WI <- cov(data[, -1]) # matrice di covarianza
HI <- (n - 1)* WI # matrice di non omogeneità statistica

trHI <- sum(diag(HI)) # misura di non omogeneita’ statistica 
trHI

################################################################################
# ---------------------------- Metodi gerarchici ------------------------------#
################################################################################

#------------ Metodo del legame singolo ------------#
hls_singolo <- hclust(distanzaEuclidea, method = "single")

# Si realizza lo screeplot per vedere il numero di cluster consigliato
plot(c(0, hls_singolo$height), 
     seq(48,1),
     type = "b", 
     main = "Screeplot del metodo del legame singolo", 
     sub = "",
     xlab = "Distanza di aggregazione",
     ylab = "Numero di cluster",
     col = "red")

c(0, hls_singolo$height) # Consigliato 3 cluster

#----- Misure di non omogeneità statistiche
# Si taglia il dendrogramma in 3 cluster
taglio_singolo <- cutree(hls_singolo, k = 3, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_singolo <- table(taglio_singolo)

# Lista di indici per i gruppi
taglioList_singolo <- list(taglio_singolo)

agvar_singolo <- aggregate(data[, -1], taglioList_singolo, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_singolo[[1]] - 1) * sum(agvar_singolo[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_singolo[[2]] - 1) * sum(agvar_singolo[2, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_singolo[[3]] - 1) * sum(agvar_singolo[3, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 
trWithin

# Si calcola la misura di non omogeneita’ tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9659548

#-------- Si Prova con 5 cluster
taglio_singolo <- cutree(hls_singolo, k = 5, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_singolo <- table(taglio_singolo)

# Lista di indici per i gruppi
taglioList_singolo <- list(taglio_singolo)

agvar_singolo <- aggregate(data[, -1], taglioList_singolo, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_singolo[[1]] - 1) * sum(agvar_singolo[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_singolo[[2]] - 1) * sum(agvar_singolo[2, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_singolo[[3]] - 1) * sum(agvar_singolo[3, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_singolo[[4]] - 1) * sum(agvar_singolo[4, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_singolo[[5]] - 1) * sum(agvar_singolo[5, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 
trWithin

# Si calcola la misura di non omogeneita’ tra i cluster (between)
trBetween <- trHI - trWithin 
trBetween/trHI # 0.9882972

# ------- Si Prova con 10 cluster
taglio_singolo <- cutree(hls_singolo, k = 10, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_singolo <- table(taglio_singolo)

# Lista di indici per i gruppi
taglioList_singolo <- list(taglio_singolo)

agvar_singolo <- aggregate(data[, -1], taglioList_singolo, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_singolo[[1]] - 1) * sum(agvar_singolo[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_singolo[[2]] - 1) * sum(agvar_singolo[2, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_singolo[[3]] - 1) * sum(agvar_singolo[3, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_singolo[[4]] - 1) * sum(agvar_singolo[4, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_singolo[[5]] - 1) * sum(agvar_singolo[5, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Gruppo 6
trH6 <- (num_gruppi_singolo[[6]] - 1) * sum(agvar_singolo[6, ])
if(is.na(trH6))
  trH6 <- 0
trH6

# Gruppo 7
trH7 <- (num_gruppi_singolo[[7]] - 1) * sum(agvar_singolo[7, ])
if(is.na(trH7))
  trH7 <- 0
trH7

# Gruppo 8
trH8 <- (num_gruppi_singolo[[8]] - 1) * sum(agvar_singolo[8, ])
if(is.na(trH8))
  trH8 <- 0
trH8

# Gruppo 9
trH9 <- (num_gruppi_singolo[[9]] - 1) * sum(agvar_singolo[9, ])
if(is.na(trH9))
  trH9 <- 0
trH9

# Gruppo 10
trH10 <- (num_gruppi_singolo[[10]] - 1) * sum(agvar_singolo[10, ])
if(is.na(trH10))
  trH10 <- 0
trH10

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8 + trH9 + trH10 
trWithin

# Si calcola la misura di non omogeneita’ tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.997217

#---------Dendrogramma
# Si visualizza il dendrogramma con 10 cluster
plot(hls_singolo, 
     hang = -1,
     main = "Dendrogramma del metodo del legame singolo", 
     sub = "",
     xlab = "",
     labels = nome_paesi)
axis(side = 4, at = round(hls_singolo$height, 2))

# Si inseriscono i rettangoli per i cluster
rect.hclust(hls_singolo, k = 10, border = "red")


#------------ Metodo del legame completo ------------#
hlc_completo <- hclust(distanzaEuclidea, method = "complete")

# Si realizza lo screeplot per vedere il numero di cluster consigliato
plot(c(0, hlc_completo$height), 
     seq(48,1),
     type = "b", 
     main = "Screeplot del metodo del legame completo", 
     sub = "",
     xlab = "Distanza di aggregazione",
     ylab = "Numero di cluster",
     col = "red")

c(0, hlc_completo$height) # Consigliato 2 cluster

#----- Misure di non omogeneità statistiche
# Si taglia il dendrogramma in 2 cluster
taglio_completo <- cutree(hlc_completo, k = 2, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_completo <- table(taglio_completo)

# Lista di indici per i gruppi
taglioList_completo <- list(taglio_completo)

agvar_completo <- aggregate(data[, -1], taglioList_completo, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_completo[[1]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_completo[[2]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2
trWithin

# Si calcola la misura di non omogeneita’ tra i cluster (between)
trBetween <- trHI - trWithin 
trBetween/trHI # 0.9651982

#-------- Si Prova con 5 cluster
# Si taglia il dendrogramma in 5 cluster
taglio_completo <- cutree(hlc_completo, k = 5, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_completo <- table(taglio_completo)

# Lista di indici per i gruppi
taglioList_completo <- list(taglio_completo)

agvar_completo <- aggregate(data[, -1], taglioList_completo, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_completo[[1]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_completo[[2]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_completo[[3]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_completo[[4]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_completo[[5]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9968508

#-------- Si Prova con 10 cluster
# Si taglia il dendrogramma in 10 cluster
taglio_completo <- cutree(hlc_completo, k = 10, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_completo <- table(taglio_completo)

# Lista di indici per i gruppi
taglioList_completo <- list(taglio_completo)

agvar_completo <- aggregate(data[, -1], taglioList_completo, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_completo[[1]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_completo[[2]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_completo[[3]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_completo[[4]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_completo[[5]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Gruppo 6
trH6 <- (num_gruppi_completo[[6]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH6))
  trH6 <- 0
trH6

# Gruppo 7
trH7 <- (num_gruppi_completo[[7]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH7))
  trH7 <- 0
trH7

# Gruppo 8
trH8 <- (num_gruppi_completo[[8]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH8))
  trH8 <- 0
trH8

# Gruppo 9
trH9 <- (num_gruppi_completo[[9]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH9))
  trH9 <- 0
trH9

# Gruppo 10
trH10 <- (num_gruppi_completo[[10]] - 1) * sum(agvar_completo[1, ])
if(is.na(trH10))
  trH10 <- 0
trH10

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8 + trH9 + trH10
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9996666

#-------- Dendrogramma
# Si visualizza il dendrogramma con 10 cluster
plot(hlc_completo, 
     hang = -1,
     main = "Dendrogramma del metodo del legame completo", 
     sub = "",
     xlab = "",
     labels = nome_paesi)
axis(side = 4, at = round(hlc_completo$height, 2))

# Si inseriscono i rettangoli per i cluster
rect.hclust(hlc_completo, k = 10, border = "red")

#------------ Metodo del legame medio ------------#
hlm_medio <- hclust(distanzaEuclidea, method = "average")

# Si realizza lo screeplot per vedere il numero di cluster consigliato
plot(c(0, hlm_medio$height), 
     seq(48,1),
     type = "b", 
     main = "Screeplot del metodo del legame medio", 
     sub = "",
     xlab = "Distanza di aggregazione",
     ylab = "Numero di cluster",
     col = "red")

c(0, hlm_medio$height) # Consigliato 2 cluster

#----- Misure di non omogeneità statistiche
# Si taglia il dendrogramma in 2 cluster
taglio_medio <- cutree(hlm_medio, k = 2, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_medio <- table(taglio_medio)

# Lista di indici per i gruppi
taglioList_medio <- list(taglio_medio)

agvar_medio <- aggregate(data[, -1], taglioList_medio, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_medio[[1]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_medio[[2]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9651982

#-------- Si Prova con 5 cluster
# Si taglia il dendrogramma in 5 cluster
taglio_medio <- cutree(hlm_medio, k = 5, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_medio <- table(taglio_medio)

# Lista di indici per i gruppi
taglioList_medio <- list(taglio_medio)

agvar_medio <- aggregate(data[, -1], taglioList_medio, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_medio[[1]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_medio[[2]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_medio[[3]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_medio[[4]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_medio[[5]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9944106

#-------- Si Prova con 10 cluster
# Si taglia il dendrogramma in 10 cluster
taglio_medio <- cutree(hlm_medio, k = 10, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_medio <- table(taglio_medio)

# Lista di indici per i gruppi
taglioList_medio <- list(taglio_medio)

agvar_medio <- aggregate(data[, -1], taglioList_medio, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_medio[[1]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_medio[[2]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_medio[[3]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_medio[[4]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_medio[[5]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Gruppo 6
trH6 <- (num_gruppi_medio[[6]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH6))
  trH6 <- 0
trH6

# Gruppo 7
trH7 <- (num_gruppi_medio[[7]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH7))
  trH7 <- 0
trH7

# Gruppo 8
trH8 <- (num_gruppi_medio[[8]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH8))
  trH8 <- 0
trH8

# Gruppo 9
trH9 <- (num_gruppi_medio[[9]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH9))
  trH9 <- 0
trH9

# Gruppo 10
trH10 <- (num_gruppi_medio[[10]] - 1) * sum(agvar_medio[1, ])
if(is.na(trH10))
  trH10 <- 0
trH10

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8 + trH9 + trH10 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9996666

#---------Dendrogramma
# Si visualizza il dendrogramma con 10 cluster
plot(hlm_medio,
     hang = -1,
     main = "Dendrogramma del metodo del legame medio", 
     sub = "",
     xlab = "",
     labels = nome_paesi)
axis(side = 4, at = round(hlm_medio$height, 2))

# Si inseriscono i rettangoli per i cluster
rect.hclust(hlm_medio, k = 10, border = "red")

#------------ Metodo del centroide ------------#
# Si calcola la matrice contenente i quadrati delle distanze euclidee
distanzaEuclidea2 <- distanzaEuclidea ^ 2

hc_centroide <- hclust(distanzaEuclidea2, method = "centroid")

#----- Misure di non omogeneità statistiche
# Si taglia il dendrogramma in 2 cluster
taglio_centroide <- cutree(hc_centroide, k = 2, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_centroide <- table(taglio_centroide)

# Lista di indici per i gruppi
taglioList_centroide <- list(taglio_centroide)

agvar_centroide <- aggregate(data[, -1], taglioList_centroide, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_centroide[[1]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_centroide[[2]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9651982

#-------- Si Prova con 5 cluster
# Si taglia il dendrogramma in 5 cluster
taglio_centroide <- cutree(hc_centroide, k = 5, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_centroide <- table(taglio_centroide)

# Lista di indici per i gruppi
taglioList_centroide <- list(taglio_centroide)

agvar_centroide <- aggregate(data[, -1], taglioList_centroide, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_centroide[[1]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_centroide[[2]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_centroide[[3]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_centroide[[4]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_centroide[[5]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9944106

#--------- Si Prova con 10 cluster
# Si taglia il dendrogramma in 10 cluster
taglio_centroide <- cutree(hc_centroide, k = 10, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_centroide <- table(taglio_centroide)

# Lista di indici per i gruppi
taglioList_centroide <- list(taglio_centroide)

agvar_centroide <- aggregate(data[, -1], taglioList_centroide, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_centroide[[1]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_centroide[[2]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_centroide[[3]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_centroide[[4]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_centroide[[5]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Gruppo 6
trH6 <- (num_gruppi_centroide[[6]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH6))
  trH6 <- 0
trH6

# Gruppo 7
trH7 <- (num_gruppi_centroide[[7]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH7))
  trH7 <- 0
trH7

# Gruppo 8
trH8 <- (num_gruppi_centroide[[8]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH8))
  trH8 <- 0
trH8

# Gruppo 9
trH9 <- (num_gruppi_centroide[[9]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH9))
  trH9 <- 0
trH9

# Gruppo 10
trH10 <- (num_gruppi_centroide[[10]] - 1) * sum(agvar_centroide[1, ])
if(is.na(trH10))
  trH10 <- 0
trH10

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8 + trH9 + trH10 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9996666

#---------Dendrogramma
# Si visualizza il dendrogramma con 10 cluster
plot(hc_centroide,
     hang = -1,
     main = "Dendrogramma del metodo del centroide", 
     sub = "",
     xlab = "",
     labels = nome_paesi)
axis(side = 4, at = round(hc_centroide$height, 2))

# Si inseriscono i rettangoli per i cluster
rect.hclust(hc_centroide, k = 10, border = "red")

#------------ Metodo della mediana ------------#
hmed_mediana <- hclust(distanzaEuclidea2, method = "median")

#----- Misure di non omogeneità statistiche
# Si taglia il dendrogramma in 2 cluster
taglio_mediana <- cutree(hmed_mediana, k = 2, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_mediana <- table(taglio_mediana)

# Lista di indici per i gruppi
taglioList_mediana <- list(taglio_mediana)

agvar_mediana <- aggregate(data[, -1], taglioList_mediana, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_mediana[[1]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_mediana[[2]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9651982

#--------- Si Prova con 5 cluster
# Si taglia il dendrogramma in 5 cluster
taglio_mediana <- cutree(hmed_mediana, k = 5, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_mediana <- table(taglio_mediana)

# Lista di indici per i gruppi
taglioList_mediana <- list(taglio_mediana)

agvar_mediana <- aggregate(data[, -1], taglioList_mediana, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_mediana[[1]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_mediana[[2]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_mediana[[3]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_mediana[[4]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_mediana[[5]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 
trWithin

# Si calcola la misura di non omogeneità tra i cluster (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9944106

#--------- Si Prova con 10 cluster
# Si taglia il dendrogramma in 10 cluster
taglio_mediana <- cutree(hmed_mediana, k = 10, h = NULL)

# Numeri di elementi dei gruppi
num_gruppi_mediana <- table(taglio_mediana)

# Lista di indici per i gruppi
taglioList_mediana <- list(taglio_mediana)

agvar_mediana <- aggregate(data[, -1], taglioList_mediana, var)[, -1]

# Gruppo 1
trH1 <- (num_gruppi_mediana[[1]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH1))
  trH1 <- 0
trH1

# Gruppo 2
trH2 <- (num_gruppi_mediana[[2]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH2))
  trH2 <- 0
trH2

# Gruppo 3
trH3 <- (num_gruppi_mediana[[3]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH3))
  trH3 <- 0
trH3

# Gruppo 4
trH4 <- (num_gruppi_mediana[[4]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH4))
  trH4 <- 0
trH4

# Gruppo 5
trH5 <- (num_gruppi_mediana[[5]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH5))
  trH5 <- 0
trH5

# Gruppo 6
trH6 <- (num_gruppi_mediana[[6]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH6))
  trH6 <- 0
trH6

# Gruppo 7
trH7 <- (num_gruppi_mediana[[7]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH7))
  trH7 <- 0
trH7

# Gruppo 8
trH8 <- (num_gruppi_mediana[[8]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH8))
  trH8 <- 0
trH8

# Gruppo 9
trH9 <- (num_gruppi_mediana[[9]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH9))
  trH9 <- 0
trH9

# Gruppo 10
trH10 <- (num_gruppi_mediana[[10]] - 1) * sum(agvar_mediana[1, ])
if(is.na(trH10))
  trH10 <- 0
trH10

# Si calcola la misura di non omogeneità interna (within)
trWithin <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8 + trH9 + trH10 
trWithin

# Si calcola la misura di non omogeneità esterna (between)
trBetween <- trHI - trWithin
trBetween/trHI # 0.9992951

#-------- Dendrogramma
# Si visualizza il dendrogramma con 10 cluster
plot(hmed_mediana, 
     hang = -1,
     main = "Dendrogramma del metodo della mediana", 
     sub = "",
     xlab = "",
     labels = nome_paesi)
axis(side = 4, at = round(hmed_mediana$height, 2))

# Si inseriscono i rettangoli per i cluster
rect.hclust(hmed_mediana, k = 10, border = "red")


################################################################################
# -------------------------- Metodi non gerarchici ----------------------------#
################################################################################

# ------------ k-means ------------#
# Visti i risultati precedenti si sceglie di calcolare il k-means con 10 cluster
km <- kmeans(data[, -1], centers = 10, iter.max = 15, nstart = 10)
km

# Si crea un dataframe associando il numero del cluster al nome del paese
associazioni <- data.frame(Numero = km$cluster, Paese = nome_paesi)

# Si visualizza il dataframe
View(associazioni)

# Si calcola la misura di non omogeneità totale
km$totss # 9594970616

# Si calcola la misura di non omoegeneità interna (within)
km$tot.withinss # 10951176

# Si calcola la misura di non omogeneità esterna (between)
km$betweenss/km$totss # 0.9988587



