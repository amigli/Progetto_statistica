#CLUSTERING

#________________________________________________________________________
#                       OPERAZIONI PRELIMINARI
#________________________________________________________________________

library(readxl)
library(writexl)

#Leggo il set di dati
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Visualizzo i dati
View(data)

#Salvo i nomi dei Paesi in modo da poterli utilizzare nel clustering
nomi_paesi <- c("Australia", "Austria", "Belgium", "Canada", "Chile",
                     "Colombia", "Costa Rica", "Czech Rep.", 
                     "Denmark", "Estonia", "Finland", "France", "Germany",
                     "Greece", "Hungary", "Iceland", "Ireland", "Israel",
                     "Italy", "Japan", "Korea", "Latvia", "Lithuania",
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand",
                     "Norway", "Poland", "Portugal", "Slovak Rep.",
                     "Slovenia", "Spain", "Sweden", "Switzerland", 
                     "Türkiye", "UK", "US", "Argentina", "Brazil",
                     "Bulgaria", "Cina", "Croatia", "India", "Indonesia",
                     "Peru", "Romania", "Russia", "South Africa")

#________________________________________________________________________
#                         MATRICE DELLE DISTANZE
#________________________________________________________________________

#Osservo la matrice delle distanze con distanza euclidea
distanceMatrix <- dist(data, method = "euclidean", diag = FALSE, upper = FALSE)
distanceMatrix

#Converto la matrice in un dataframe
distance_df <- as.data.frame(as.matrix(distanceMatrix))

# Salva la matrice delle distanze in un file Excel
write_xlsx(distance_df, "matrice_distanze_fertilita.xlsx")

#Calcolo quante sono le istanze totali
length(distanceMatrix)

#________________________________________________________________________
#                   MATRICE DI NON OMOGENEITA' TOTALE
#________________________________________________________________________

#Matrice delle varianze e covarianze campionarie tra le caratteristiche
WI <- cov(data[,-1])

#Vedo quanto i dati sono omogenei attraverso la traccia
n <- nrow(data[, -1])
HI <- (n-1)*WI

#HI è la matrice di non omogeneità statistica
HI

#Visualizzo la misura di non omogeneità statistica
trHI <- sum(diag(HI))
trHI

#________________________________________________________________________
#                       METODO DEL LEGAME SINGOLO
#________________________________________________________________________

#Faccio il clustering sulla base della matrice delle distanze
hls <- hclust(distanceMatrix, method = "single")
hls$height
str(hls)

#Costruisco il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del legame singolo", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Costruisco lo screeplot
plot(c(0, hls$height), seq(49,1), type = "b", main = "Screeplot", 
     xlab = "Distanza di aggregazione", ylab = "Numero di cluster",
     col = "red")

#Vediamo le altezze a cui sono avvenute le varia agglomerazioni
c(0, hls$height)

#Verifichiamo anche con l'elbow point
# Calcola la varianza cumulativa
cumulative_variance <- cumsum(hls$height) / sum(hls$height)

# Trova il punto di gomito
elbow_point <- which(diff(cumulative_variance) < 0.05)[1]

# Plot dello screeplot
plot(1:length(hls$height), cumulative_variance, type = "b",
     main = "Elbow Point",
     xlab = "Numero di Cluster", ylab = "Varianza Cumulativa")

# Aggiungi una linea per evidenziare il punto di gomito
abline(v = elbow_point, col = "red", lty = 2)

# Stampa il numero consigliato di cluster
cat("Numero consigliato di cluster:", elbow_point, "\n")

#________________________________________________________________________

#Evidenziamo le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =2, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Somma delle tracce
sum <- trH1 + trH2 
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 5 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =5, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 8 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =8, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
trB <- trHI - sum
trB/trHI

#Evidenzio gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                       METODO DEL LEGAME COMPLETO
#________________________________________________________________________


#Faccio il clustering sulla base della matrice delle distanze
hls <- hclust(distanceMatrix, method = "complete")
hls$height
str(hls)

#Costruisco il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del legame completo", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Costruisco lo screeplot
plot(c(0, hls$height), seq(49,1), type = "b", main = "Screeplot", 
     xlab = "Distanza di aggregazione", ylab = "Numero di cluster",
     col = "red")

#Vediamo le altezze a cui sono avvenute le varia agglomerazioni
c(0, hls$height)

#Verifichiamo anche con l'elbow point
# Calcola la varianza cumulativa
cumulative_variance <- cumsum(hls$height) / sum(hls$height)

# Trova il punto di gomito
elbow_point <- which(diff(cumulative_variance) < 0.05)[1]

# Plot dello screeplot
plot(1:length(hls$height), cumulative_variance, type = "b",
     main = "Elbow Point",
     xlab = "Numero di Cluster", ylab = "Varianza Cumulativa")

# Aggiungi una linea per evidenziare il punto di gomito
abline(v = elbow_point, col = "red", lty = 2)

# Stampa il numero consigliato di cluster
cat("Numero consigliato di cluster:", elbow_point, "\n")

#________________________________________________________________________

#Evidenziamo le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =2, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Somma delle tracce
sum <- trH1 + trH2 
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 5 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =5, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 8 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =8, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
trB <- trHI - sum
trB/trHI

#Evidenzio gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                       METODO DEL LEGAME MEDIO
#________________________________________________________________________

#Faccio il clustering sulla base della matrice delle distanze
hls <- hclust(distanceMatrix, method = "average")
hls$height
str(hls)

#Costruisco il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del legame medio", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Costruisco lo screeplot
plot(c(0, hls$height), seq(49,1), type = "b", main = "Screeplot", 
     xlab = "Distanza di aggregazione", ylab = "Numero di cluster",
     col = "red")

#Vediamo le altezze a cui sono avvenute le varia agglomerazioni
c(0, hls$height)

#Verifichiamo anche con l'elbow point
# Calcola la varianza cumulativa
cumulative_variance <- cumsum(hls$height) / sum(hls$height)

# Trova il punto di gomito
elbow_point <- which(diff(cumulative_variance) < 0.05)[1]

# Plot dello screeplot
plot(1:length(hls$height), cumulative_variance, type = "b",
     main = "Elbow Point",
     xlab = "Numero di Cluster", ylab = "Varianza Cumulativa")

# Aggiungi una linea per evidenziare il punto di gomito
abline(v = elbow_point, col = "red", lty = 2)

# Stampa il numero consigliato di cluster
cat("Numero consigliato di cluster:", elbow_point, "\n")

#________________________________________________________________________

#Evidenziamo le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =2, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Somma delle tracce
sum <- trH1 + trH2 
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 5 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =5, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 8 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =8, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
trB <- trHI - sum
trB/trHI

#Evidenzio gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")

#________________________________________________________________________
#                           METODO DEL CENTROIDE
#________________________________________________________________________


#Faccio il clustering sulla base della matrice delle distanze
hls <- hclust(distanceMatrix, method = "centroid")
hls$height
str(hls)

#Costruisco il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del centroide", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Evidenziamo le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =2, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Somma delle tracce
sum <- trH1 + trH2 
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 5 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =5, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 8 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =8, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
trB <- trHI - sum
trB/trHI

#Evidenzio gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                           METODO DELLA MEDIANA
#________________________________________________________________________


#Faccio il clustering sulla base della matrice delle distanze
hls <- hclust(distanceMatrix, method = "median")
hls$height
str(hls)

#Costruisco il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="della mediana", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Evidenziamo le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =2, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Somma delle tracce
sum <- trH1 + trH2 
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 5 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =5, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5
trB <- trHI - sum
trB/trHI

#________________________________________________________________________

#Provo con 8 cluster se la misura migliora
#Calcoliamo la misura di non omogeneità statistica tra i cluster
taglio <- cutree (hls , k =8, h = NULL)
num <- table (taglio)

#Lista di indici per i gruppi
tagliolist <- list(taglio)

#Calcolo le medie campionarie, le varianze e deviazioni
#standard dei vari gruppi
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Somma delle tracce
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
trB <- trHI - sum
trB/trHI

#Evidenzio gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")
