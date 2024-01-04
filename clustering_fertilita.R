#CLUSTERING SUL DATASET FERTILITA'
#Author: Annalaura Miglino

#________________________________________________________________________
#                       OPERAZIONI PRELIMINARI
#________________________________________________________________________

library(readxl)
library(writexl)

#Si legge il set di dati
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Si visualizzano i dati
View(data)

#Vengono salvati i nomi dei Paesi in modo da poterli utilizzare nel dendrogramma
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

#Si costruisce la matrice delle distanze con distanza euclidea
distanceMatrix <- dist(data, method = "euclidean", diag = FALSE, upper = FALSE)
distanceMatrix

#Si converte la matrice in un dataframe per poterla salvare in Excel
distance_df <- as.data.frame(as.matrix(distanceMatrix))

#Si salva la matrice delle distanze in un file Excel
write_xlsx(distance_df, "matrice_distanze_fertilita.xlsx")

#Si calcola quante sono le istanze totali
length(distanceMatrix)


#________________________________________________________________________
#                   MATRICE DI NON OMOGENEITA' TOTALE
#________________________________________________________________________

#Matrice delle varianze e covarianze campionarie tra le caratteristiche
WI <- cov(data[,-1])

#Si memorizza il numero di righe
n <- nrow(data[, -1])

#Si calcola la matrice di non omogeneità statistica e viene stampata
HI <- (n-1)*WI
HI

#Si calcola la misura di non omogeneità statistica (la traccia)
#In questo modo, si verifica quanto i dati sono omogenei
trHI <- sum(diag(HI))
trHI


#________________________________________________________________________
#                             METODI GERARCHICI
#________________________________________________________________________

#________________________________________________________________________
#                       METODO DEL LEGAME SINGOLO
#________________________________________________________________________

#Si effettua il clustering sulla base della matrice delle distanze
#Il metodo scelto è quello del legame singolo
hls <- hclust(distanceMatrix, method = "single")

#Si estraggono le altezze di unione degli individui
hls$height

#Si visualizza la struttura dell'oggetto hls
str(hls)

#Si costruisce il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del legame singolo", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#VERIFICA DEL NUMERO DI CLUSTER IDEALE
#Si costruisce lo screeplot
plot(c(0, hls$height), seq(49,1), type = "b", main = "Screeplot", 
     xlab = "Distanza di aggregazione", ylab = "Numero di cluster",
     col = "red")

#Si osservano le altezze a cui sono avvenute le varie agglomerazioni
c(0, hls$height)

#Si verifica anche con l'elbow point il numero ideale di cluster
#Viene calcolata la varianza cumulativa
cumulative_variance <- cumsum(hls$height) / sum(hls$height)

#Si trova il punto di gomito
elbow_point <- which(diff(cumulative_variance) < 0.05)[1]

#Si osserva lo screeplot
plot(1:length(hls$height), cumulative_variance, type = "b",
     main = "Elbow Point",
     xlab = "Numero di Cluster", ylab = "Varianza Cumulativa")

#Si aggiunge una linea per evidenziare il punto di gomito
abline(v = elbow_point, col = "red", lty = 2)

#Viene stampato il numero consigliato di cluster
cat("Numero consigliato di cluster:", elbow_point, "\n")

#________________________________________________________________________

#Si calcolano le misure di non omogeneità statistica tra i cluster
#Si effettua il taglio del dendrogramma in modo che siano 2 gruppi
taglio <- cutree (hls , k =2, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Within
sum <- trH1 + trH2
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#________________________________________________________________________

#Si verifica con 5 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 5 gruppi
taglio <- cutree (hls , k =5, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 5 cluster nel dendrogramma
rect.hclust(hls, k = 5, border = "red")

#________________________________________________________________________

#Si verifica con 8 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 8 gruppi
taglio <- cutree (hls , k =8, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Si calcola la traccia del sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Si calcola la traccia del settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Si calcola la traccia dell'ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                       METODO DEL LEGAME COMPLETO
#________________________________________________________________________

#Si effettua il clustering sulla base della matrice delle distanze
#Il metodo scelto è quello del legame completo
hls <- hclust(distanceMatrix, method = "complete")

#Si estraggono le altezze di unione degli individui
hls$height

#Si visualizza la struttura dell'oggetto hls
str(hls)

#Si costruisce il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del legame completo", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#VERIFICA DEL NUMERO IDEALE DI CLUSTER
#Si costruisce lo screeplot
plot(c(0, hls$height), seq(49,1), type = "b", main = "Screeplot", 
     xlab = "Distanza di aggregazione", ylab = "Numero di cluster",
     col = "red")

#Si osservano le altezze a cui sono avvenute le varie agglomerazioni
c(0, hls$height)

#Si verifica anche con l'elbow point
#Si calcola la varianza cumulativa
cumulative_variance <- cumsum(hls$height) / sum(hls$height)

#Si trova il punto di gomito
elbow_point <- which(diff(cumulative_variance) < 0.05)[1]

#Si osserva lo screeplot
plot(1:length(hls$height), cumulative_variance, type = "b",
     main = "Elbow Point",
     xlab = "Numero di Cluster", ylab = "Varianza Cumulativa")

#Si aggiunge una linea per evidenziare il punto di gomito
abline(v = elbow_point, col = "red", lty = 2)

#Viene stampato il numero consigliato di cluster
cat("Numero consigliato di cluster:", elbow_point, "\n")

#________________________________________________________________________

#Si calcolano le misure di non omogeneità statistica tra i cluster
#Si effettua il taglio del dendrogramma in modo che siano 2 gruppi
taglio <- cutree (hls , k =2, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Within
sum <- trH1 + trH2
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#________________________________________________________________________

#Si verifica con 5 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 5 gruppi
taglio <- cutree (hls , k =5, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 5 cluster nel dendrogramma
rect.hclust(hls, k = 5, border = "red")

#________________________________________________________________________

#Si verifica con 8 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 8 gruppi
taglio <- cutree (hls , k =8, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Si calcola la traccia del sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Si calcola la traccia del settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Si calcola la traccia dell'ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                       METODO DEL LEGAME MEDIO
#________________________________________________________________________

#Si effettua il clustering sulla base della matrice delle distanze
#Il metodo scelto è quello del legame medio
hls <- hclust(distanceMatrix, method = "average")

#Si estraggono le altezze di unione degli individui
hls$height

#Si visualizza la struttura dell'oggetto hls
str(hls)

#Si costruisce il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del legame medio", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#VERIFICA DEL NUMERO DI CLUSTER IDEALE
#Si costruisce lo screeplot
plot(c(0, hls$height), seq(49,1), type = "b", main = "Screeplot", 
     xlab = "Distanza di aggregazione", ylab = "Numero di cluster",
     col = "red")

#Si osservano le altezze a cui sono avvenute le varie agglomerazioni
c(0, hls$height)

#Si verifica anche con l'elbow point
#Viene calcolata la varianza cumulativa
cumulative_variance <- cumsum(hls$height) / sum(hls$height)

#Si trova il punto di gomito
elbow_point <- which(diff(cumulative_variance) < 0.05)[1]

#Si osserva lo screeplot
plot(1:length(hls$height), cumulative_variance, type = "b",
     main = "Elbow Point",
     xlab = "Numero di Cluster", ylab = "Varianza Cumulativa")

#Si aggiunge una linea per evidenziare il punto di gomito
abline(v = elbow_point, col = "red", lty = 2)

#Viene stampato il numero consigliato di cluster
cat("Numero consigliato di cluster:", elbow_point, "\n")

#________________________________________________________________________

#Si calcola la misura di non omogeneità statistica tra i cluster
#Si effettua il taglio del dendrogramma in modo che siano 2 gruppi
taglio <- cutree (hls , k =2, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Within
sum <- trH1 + trH2
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#________________________________________________________________________

#Si verifica con 5 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 5 gruppi
taglio <- cutree (hls , k =5, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 5 cluster nel dendrogramma
rect.hclust(hls, k = 5, border = "red")

#________________________________________________________________________

#Si verifica con 8 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 8 gruppi
taglio <- cutree (hls , k =8, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Si calcola la traccia del sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Si calcola la traccia del settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Si calcola la traccia dell'ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                           METODO DEL CENTROIDE
#________________________________________________________________________

#Effettuo il clustering sulla base dei quadrati della matrice delle distanze
d2 <- distanceMatrix^2

#Il metodo scelto è quello del centroide
hls <- hclust(d2, method = "centroid")

#Si estraggono le altezze di unione degli individui
hls$height

#Si visualizza la struttura dell'oggetto hls
str(hls)

#Si costruisce il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="del centroide", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Si calcola la misura di non omogeneità statistica tra i cluster
#Si effettua il taglio del dendrogramma in modo che siano 2 gruppi
taglio <- cutree (hls , k =2, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Within
sum <- trH1 + trH2
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#________________________________________________________________________

#Si verifica con 5 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 5 gruppi
taglio <- cutree (hls , k =5, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 5 cluster nel dendrogramma
rect.hclust(hls, k = 5, border = "red")

#________________________________________________________________________

#Si verifica con 8 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 8 gruppi
taglio <- cutree (hls , k =8, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Si calcola la traccia del sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Si calcola la traccia del settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Si calcola la traccia dell'ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                           METODO DELLA MEDIANA
#________________________________________________________________________

#Effettuo il clustering sulla base dei quadrati della matrice delle distanze
#Il metodo scelto è quello della mediana
hls <- hclust(d2, method = "median")

#Si estraggono le altezze di unione degli individui
hls$height

#Si visualizza la struttura dell'oggetto hls
str(hls)

#Si costruisce il dendrogramma 
plot(hls, hang = -1, xlab = "Metodo gerarchico agglomerativo",
     sub="della mediana", labels = nomi_paesi, main = "Dendrogramma")
axis(side = 4, at = round(c(0, hls$height), 2))

#________________________________________________________________________

#Si calcola la misura di non omogeneità statistica tra i cluster
#Si effettua il taglio del dendrogramma in modo che siano 2 gruppi
taglio <- cutree (hls , k =2, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Within
sum <- trH1 + trH2
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 2 cluster nel dendrogramma
rect.hclust(hls, k = 2, border = "red")

#________________________________________________________________________

#Si verifica con 5 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 5 gruppi
taglio <- cutree (hls , k =5, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) *sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano le partizioni con 5 cluster nel dendrogramma
rect.hclust(hls, k = 5, border = "red")

#________________________________________________________________________

#Si verifica con 8 cluster se le misure migliorano
#Si effettua il taglio del dendrogramma in modo che siano 8 gruppi
taglio <- cutree (hls , k =8, h = NULL)

#Si osserva quanti individui ci sono in ciascun cluster
num <- table (taglio)

#Si crea una lista contenente le etichette dei cluster
tagliolist <- list(taglio)

#Si calcola la varianza campionaria delle colonne dei dati
#in base alle etichette dei cluster
agvar <- aggregate(data[, -1], tagliolist, var)[, -1]

#Si calcola la traccia del primo gruppo
trH1 <-(num [[1]] -1) * sum(agvar [1, ])
if(is.na(trH1))
  trH1 <- 0

#Si calcola la traccia del secondo gruppo
trH2 <-(num [[2]] -1) *sum(agvar [2, ])
if(is.na(trH2))
  trH2 <- 0

#Si calcola la traccia del terzo gruppo
trH3 <-(num [[3]] -1) *sum(agvar [3, ])
if(is.na(trH3))
  trH3 <- 0

#Si calcola la traccia del quarto gruppo
trH4 <-(num [[4]] -1) * sum(agvar [4, ])
if(is.na(trH4))
  trH4 <- 0

#Si calcola la traccia del quinto gruppo
trH5 <-(num [[5]] -1) *sum(agvar [5, ])
if(is.na(trH5))
  trH5 <- 0

#Si calcola la traccia del sesto gruppo
trH6 <-(num [[6]] -1) *sum(agvar [6, ])
if(is.na(trH6))
  trH6 <- 0

#Si calcola la traccia del settimo gruppo
trH7 <-(num [[7]] -1) *sum(agvar [7, ])
if(is.na(trH7))
  trH7 <- 0

#Si calcola la traccia dell'ottavo gruppo
trH8 <-(num [[8]] -1) *sum(agvar [8, ])
if(is.na(trH8))
  trH8 <- 0

#Within
sum <- trH1 + trH2 + trH3 + trH4 + trH5 + trH6 + trH7 + trH8
sum

#Between
trB <- trHI - sum
trB/trHI

#Si evidenziano gli 8 cluster nel dendrogramma
rect.hclust(hls, k = 8, border = "red")


#________________________________________________________________________
#                           METODI NON GERARCHICI
#________________________________________________________________________

#________________________________________________________________________
#                                   K-MEANS
#________________________________________________________________________

#Si applica l'algoritmo k-means
km <- kmeans (data[, -1], centers = 8 , iter.max = 20, nstart = 10)

#Si osservano informazioni sul raggruppamento effettuato
km

#Si calcola la misura di non omogeneità statistica
km$betweenss/km$totss

#Si calcola la misura di non omogeneità totale
km$totss

#Si calcola la somma delle within
km$tot.withinss

#Si calcola la between
km$betweenss