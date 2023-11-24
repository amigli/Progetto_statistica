#analisi descrittiva del dataset sulla fertilità
library(readxl)
library(writexl)
data <- read_excel("C:/Users/migli/Desktop/Università/Magistrale/Statistica e Analisi dei Dati/progetto_statistica/dataset_puliti/fertilita_pulito.xlsx")
View(data)

# Verifico se le colonne sono in formato numerico
colonne_numeriche <- sapply(data[, -1], is.numeric)
print(colonne_numeriche)

#Faccio delle operazioni preliminari per poter operare più facilmente sul dataset
# Estraggo i nomi delle colonne per le operazioni successive
nomi_colonne_numeriche <- names(colonne_numeriche)
nomi_colonne_numeriche


# Utilizzo lapply per convertire le colonne in numerico
data[, nomi_colonne_numeriche] <- lapply(data[, nomi_colonne_numeriche], as.numeric)

#Verifico se le colonne sono in formato numerico
colonne_numeriche <- sapply(data[, -1], is.numeric)
colonne_numeriche

#Arrotondo i valori alla seconda cifra decimale
data[, nomi_colonne_numeriche] <- round(data[nomi_colonne_numeriche], 2)
View(data)

#Salvo il dataset con i valori arrotondati
write_xlsx(data, "dataset_puliti/fertilita_arrotondato.xlsx")

#________________________________________________________________________

#Fare un grafico con le medie tra tutti i Paesi anno per anno
#Scrivere le cose nella documentazione, trarne le conclusioni

#Calcolo la media per ogni anno
media_per_anno <- colMeans(data[, -1])
media_per_anno

#Rappresento tutto sotto forma di serie temporale
media_anni <- ts(media_per_anno, start = 2010, frequency = 1)

#Creo il grafico relativo a questa serie temporale
plot(media_anni, main="Media dei bambini nati per donna anno per anno", xlab="Anno", ylab="Valori", col="blue", type="o")

#________________________________________________________________________

#Funzione di distribuzione ogni 3 anni
#commentare nella documentazione

print(min(data[, -1]))
print(max(data[, -1]))

#Definisco gli intervalli, quindi
#tra 0 e 1.49 (1 bambino in media per donna), essendo che il valore minimo è 0.81
#tra 1.50 e 2.49 (2 bambini in media per donna)
#dopo 2.50 (3 bambini in media per donna) essendo che il valore massimo è 3.11
intervalli <- c(0.5, 1.49, 2.49, 3.49) 

#Divido i dati in 3 intervalli, ciascuno di 3 anni
dati_2010_2013 <- data[, c("2010", "2011", "2012", "2013")]
dati_2014_2017 <- data[, c("2014", "2015", "2016", "2017")]
dati_2018_2021 <- data[, c("2018", "2019", "2020", "2021")]

#Converto tutti i dati in un vettore numerico in modo da poter operarci più facilmente
dati_2010_2013 <- as.numeric(as.character(unlist(dati_2010_2013)))
dati_2014_2017 <- as.numeric(as.character(unlist(dati_2014_2017)))
dati_2018_2021 <- as.numeric(as.character(unlist(dati_2018_2021)))

#Calcolo le frequenze assolute per ognuno
frequenzeA_10_13 <- table(cut(dati_2010_2013, breaks = intervalli))
frequenzeA_14_17 <- table(cut(dati_2014_2017, breaks = intervalli))
frequenzeA_18_21 <- table(cut(dati_2018_2021, breaks = intervalli))

#Calcolo anche le frequenze assolute cumulate per ognuno
cumulateA_10_13 <- cumsum(frequenzeA_10_13)
cumulateA_14_17 <- cumsum(frequenzeA_14_17)
cumulateA_18_21 <- cumsum(frequenzeA_18_21)

#Vediamo quante istanze ci sono di ciascun range con un istogramma
#Ne costruiamo uno per ogni anno
hist(dati_2010_2013, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA_10_13) + 20),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2010 e il 2013",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

hist(dati_2014_2017, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA_14_17) + 20),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2014 e il 2017",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

hist(dati_2018_2021, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA_18_21) + 20),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2018 e il 2021",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

#Questi grafici in basso sono come gli istogrammi fatti prima
#La differenza sta nel fatto che negli istogrammi si ragiona a range
#Qui, invece, i valori vengono direttamente arrotondati
#Rappresentano le frequenze assolute anno per anno
barplot(frequenzeA_10_13, xlab = "Numero di figli in media per donna tra il 2010 e il 2013", 
        col=1:3, names.arg = c("1", "2", "3"), 
        ylim = c(0, max(frequenzeA_10_13) + 20))
frequenzeA_10_13
barplot(frequenzeA_14_17, xlab = "Numero di figli in media per donna tra il 2014 e il 2017", 
        col=1:3, names.arg = c("1", "2", "3"),
        ylim = c(0, max(frequenzeA_14_17) + 20))
frequenzeA_14_17
barplot(frequenzeA_18_21, xlab = "Numero di figli in media per donna tra il 2018 e il 2021", 
        col=1:3, names.arg = c("1", "2", "3"),
        ylim = c(0, max(frequenzeA_18_21) + 20))

#Faccio una tabella di contingenza
contingenza_assolute <- cbind(frequenzeA_10_13, frequenzeA_14_17, frequenzeA_18_21)
contingenza_assolute

#Frequenza assoluta congiunta
#La traspongo in modo da avere su ogni barra i dati giusti
#Nella prima barra, ci sono 56, 46 e 56 per tutte le triennali
#Nella seconda barra, ci sono 131, 146 e 136 per tutte le triennali
#Nella terza barra, ci sono 9, 4 e 4 per tutte le triennali
#Infatti, la somma della prima deve fare 56+46+56=158, che si trova ecc...
barplot(
  t(contingenza_assolute),
  col = c("blue", "green", "red"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  names.arg = c("1 figlio", "2 figli", "3 figli")
)

#Frequenza relativa condizionata (range anni | numero figli)
#Sono espressi diversamente i dati in questo caso:
#Per 1 figlio, ci sono tutti e 3 gli anni (56, 46 e 56) messi a confronto
#Così via anche per gli altri valori
barplot(
  t(contingenza_assolute),
  beside = TRUE,
  col = c("blue", "green", "red"),
  names.arg = c("1 figlio", "2 figli", "3 figli"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  xlab = "Numero di figli in media per donna",
  ylab = "Frequenza assoluta",
  main = "Frequenze assolute per intervallo di anni",
  ylim = c(0, max(matrice_assolute) + 1)
)

#Distribuzione di frequenza marginale del numero di figli
#Qui vediamo la totalità di istanze per tutti gli anni messi insieme
freqMarginaleFigli <- margin.table(contingenza_assolute, 1)

#Grafico per la frequenza assoluta marginale
#Questo grafico rappresenta il totale delle istanze per ciascun numero
#di figli medio per donna.
barplot(freqMarginaleFigli, 
        main = "Frequenza assoluta marginale del numero di figli",
        col = c("red", "green", "blue"),
        names.arg = c("1 figlio", "2 figli", "3 figli"))


#______________________________________________________________________

#fare lo stesso anche per le frequenze relative

#Calcolo le frequenze relative per ognuno
frequenzeR_10_13 <- table(cut(dati_2010_2013, breaks = intervalli))/length(dati_2010_2013)
frequenzeR_14_17 <- table(cut(dati_2014_2017, breaks = intervalli))/length(dati_2014_2017)
frequenzeR_18_21 <- table(cut(dati_2018_2021, breaks = intervalli))/length(dati_2018_2021)

#Calcolo anche le frequenze relative cumulate per ognuno
cumulateR_10_13 <- cumsum(frequenzeR_10_13)
cumulateR_14_17 <- cumsum(frequenzeR_14_17)
cumulateR_18_21 <- cumsum(frequenzeR_18_21)

#Vediamo quante istanze ci sono di ciascun range con un istogramma
#Ne costruiamo uno per ogni anno
#Alla fine è uguale a quello delle frequenze assolute
hist(dati_2010_2013, breaks = intervalli, freq = FALSE,
     ylim = c(0, max(frequenzeR_10_13) + 0.2),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2010 e il 2013",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

hist(dati_2014_2017, breaks = intervalli, freq = FALSE,
     ylim = c(0, max(frequenzeR_14_17) + 0.2),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2014 e il 2017",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

hist(dati_2018_2021, breaks = intervalli, freq = FALSE,
     ylim = c(0, max(frequenzeR_18_21) + 0.2),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2018 e il 2021",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

#Costruisco i grafici per ognuno
#Sono uguali a quelli delle frequenze assolute, cambiano solo i valori su Y
#Rappresentano i valori delle frequenze relative anno per anno
barplot(frequenzeR_10_13, xlab = "Numero di figli in media per donna tra il 2010 e il 2013", 
        col=1:3, names.arg = c("1", "2", "3"),
        ylim = c(0, max(frequenzeR_10_13) + 0.2))
frequenzeR_10_13
barplot(frequenzeR_14_17, xlab = "Numero di figli in media per donna tra il 2014 e il 2017", 
        col=1:3, names.arg = c("1", "2", "3"),
        ylim = c(0, max(frequenzeR_14_17) + 0.2))
frequenzeR_14_17
barplot(frequenzeR_18_21, xlab = "Numero di figli in media per donna tra il 2018 e il 2021", 
        col=1:3, names.arg = c("1", "2", "3"),
        ylim = c(0, max(frequenzeR_18_21) + 0.2))
frequenzeR_18_21

#faccio la tabella di contingenza
contingenza_relative <- cbind(frequenzeR_10_13, frequenzeR_14_17, frequenzeR_18_21)
contingenza_relative

#Frequenza relativa congiunta
#La traspongo in modo da avere su ogni barra i dati giusti
#Il ragionamento è lo stesso delle frequenze assolute
#Proviamo il ragionamento per la prima barra:
#0.28571429+0.23469388+0.28571429=0.80612246 che è l'altezza della prima barra
barplot(
  t(contingenza_relative),
  col = c("blue", "green", "red"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  names.arg = c("1 figlio", "2 figli", "3 figli")
)

#Costruisco il grafico a barre
#è uguale a quello delle frequenze assolute, ma con quelle relative
barplot(
  t(contingenza_relative),
  beside = TRUE,
  col = c("blue", "green", "red"),
  names.arg = c("1", "2", "3"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  xlab = "Numero di figli in media per donna",
  ylab = "Frequenza relativa",
  main = "Frequenze relative per intervallo di anni",
  ylim = c(0, max(contingenza_relative)+0.2)
)

#Frequenze relative congiunte
#Sarebbe la proporzione di ciascun elemento nella matrice rispetto alla 
#somma totale degli elementi della matrice: ogni elemento viene diviso per
#il numero totale di elementi.
freqFigli <- prop.table(contingenza_relative)
freqFigli

#Somma le istanze in base al numero di figli della tabella precedente
margin.table(freqFigli, 1)
#Questo viene rappresentato nel grafico sottostante, dove ogni colonna
#del margin.table viene rappresentata da una barra

#Frequenze relative congiunte
barplot(
  t(freqFigli),
  col = c("blue", "green", "red"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  names.arg = c("1 figlio", "2 figli", "3 figli")
)

#Vediamo anche la somma per le frequenze relative per numero di figli
margin.table(contingenza_relative, 1)

#Grafico per la frequenza relativa marginale
#è come quello per la frequenza assoluta, ovviamente però ci sono i valori
#relativi sull'asse Y
barplot(margin.table(contingenza_relative, 1), 
        main = "Frequenza relativa marginale del numero di figli",
        col = c("red", "green", "blue"),
        names.arg = c("1 figlio", "2 figli", "3 figli"))

#_________________________________________________________________________

#Fare il boxplot per vedere eventuali valori anomali anno per anno

#ANNO 2010
dati_2010 <- data[["2010"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2010)
summary(dati_2010)
#Costruisco il boxplot
boxplot(dati_2010,
        main = "Boxplot anno 2010",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2010)
limite_superiore <- stats$stats[5]
limite_superiore
#A questo punto, è chiaro che vi è un solo outlier: Israel

#ANNO 2011
dati_2011 <- data[["2011"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2011)
summary(dati_2011)
#Costruisco il boxplot
boxplot(dati_2011,
        main = "Boxplot anno 2011",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2011)
limite_superiore <- stats$stats[5]
limite_superiore
#Anche in questo caso vi è un solo outlier: Israel

#ANNO 2012
dati_2012 <- data[["2012"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2012)
summary(dati_2012)
#Costruisco il boxplot
boxplot(dati_2012,
        main = "Boxplot anno 2012",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2012)
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2013
dati_2013 <- data[["2013"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2013)
summary(dati_2013)
#Costruisco il boxplot
boxplot(dati_2013,
        main = "Boxplot anno 2013",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2013)
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2014
dati_2014 <- data[["2014"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2014)
summary(dati_2014)
#Costruisco il boxplot
boxplot(dati_2014,
        main = "Boxplot anno 2014",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2014)
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2015
dati_2015 <- data[["2015"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2015)
summary(dati_2015)
#Costruisco il boxplot
boxplot(dati_2015,
        main = "Boxplot anno 2015",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2015)
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2016
dati_2016 <- data[["2016"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2016)
summary(dati_2016)
#Costruisco il boxplot
boxplot(dati_2016,
        main = "Boxplot anno 2016",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2016)
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel, India, Indonesia, Peru e Sud Africa

#ANNO 2017
dati_2017 <- data[["2017"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2017)
summary(dati_2017)
#Costruisco il boxplot
boxplot(dati_2017,
        main = "Boxplot anno 2017",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2017)
limite_superiore <- stats$stats[5]
limite_inferiore <- stats$stats[1]
limite_superiore
limite_inferiore
#Outlier: Korea (Inferiore), Israele, India, Indonesia, Peru, Sud Africa

#ANNO 2018
dati_2018 <- data[["2018"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2018)
summary(dati_2018)
#Costruisco il boxplot
boxplot(dati_2018,
        main = "Boxplot anno 2018",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2018)
limite_superiore <- stats$stats[5]
limite_inferiore <- stats$stats[1]
limite_superiore
limite_inferiore
#Outlier: Korea (inferiore), Israele, India, Indonesia, Peru, Sud Africa

#ANNO 2019
dati_2019 <- data[["2019"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2019)
summary(dati_2019)
#Costruisco il boxplot
boxplot(dati_2019,
        main = "Boxplot anno 2019",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2019)
limite_superiore <- stats$stats[5]
limite_inferiore <- stats$stats[1]
limite_superiore
limite_inferiore
#Outlier: Korea (inferiore), Israele, India, Indonesia, Peru, Sud Africa

#ANNO 2020
dati_2020 <- data[["2020"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2020)
summary(dati_2020)
#Costruisco il boxplot
boxplot(dati_2020,
        main = "Boxplot anno 2020",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2020)
limite_superiore <- stats$stats[5]
limite_inferiore <- stats$stats[1]
limite_superiore
limite_inferiore
#Outlier: Korea (inferiore), Israele, Indonesia, Perù, Sud Africa

#ANNO 2021
dati_2021 <- data[["2021"]]
#Calcolo i quantili e massimo, minimo, mediana, media
quantile(dati_2021)
summary(dati_2021)
#Costruisco il boxplot
boxplot(dati_2021,
        main = "Boxplot anno 2021",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Vedo qual è il limite superiore
stats <- boxplot.stats(dati_2021)
limite_superiore <- stats$stats[5]
limite_inferiore <- stats$stats[1]
limite_superiore
limite_inferiore
#Outlier: Korea (inferiore), Israele, Indonesia, Perù e Sud Africa

#Confronto gli anni a 4 a 4 come i range definiti prima
#Questi servono nella documentazione per mostrare l'andamento
#DAL 2010 AL 2013
boxplot(dati_2010, dati_2011, dati_2012, dati_2013, 
        names = c("Anno 2010", "Anno 2011", "Anno 2012", "Anno 2013"),
        col = c("red", "blue", "green", "yellow"))
#DAL 2014 AL 2017
boxplot(dati_2014, dati_2015, dati_2016, dati_2017, 
        names = c("Anno 2014", "Anno 2015", "Anno 2016", "Anno 2017"),
        col = c("red", "blue", "green", "yellow"))
#DAL 2018 AL 2021
boxplot(dati_2018, dati_2019, dati_2020, dati_2021, 
        names = c("Anno 2018", "Anno 2019", "Anno 2020", "Anno 2021"),
        col = c("red", "blue", "green", "yellow"))

#Confronto tra primo e ultimo anno tramite il boxplot ad intaglio
boxplot(dati_2010, dati_2021, notch = TRUE, 
        names = c("Anno 2010", "Anno 2021"), col = c("green", "orange"))
#Vediamo se le mediane si sovrappongono
#ANNO 2010
IQR_2010 <- quantile(dati_2010, 0.75) - quantile(dati_2010, 0.25)
M1_2010 <- quantile(dati_2010, 0.75) - 1.57*IQR_2010/sqrt(length(dati_2010))
M2_2010 <- quantile(dati_2010, 0.75) + 1.57*IQR_2010/sqrt(length(dati_2010))

#ANNO 2021
IQR_2021 <- quantile(dati_2021, 0.75) - quantile(dati_2021, 0.25)
M1_2021 <- quantile(dati_2021, 0.75) - 1.57*IQR_2021/sqrt(length(dati_2021))
M2_2021 <- quantile(dati_2021, 0.75) + 1.57*IQR_2021/sqrt(length(dati_2021))

c(M1_2010, M2_2010)
c(M1_2021, M2_2021)

#Poiché i due intervalli non si sovrappongono, con un livello di 
#significatività del 5% si può affermare che le mediane dei voti delle
#due classi sono differenti.

#________________________________________________________________________

#Come fatto per la media, vediamo anche la moda nel corso del tempo
#Calcolare la moda, così capiamo quale valore si presenta più spesso anno per anno
#Scrivere le cose nella documentazione, trarne le conclusioni
moda <- function(v) {
  tmp <- unique(v)
  tmp[which.max(tabulate(match(v, tmp)))]
}

moda_per_anno <- sapply(data[, -1], moda)
moda_per_anno

#Rappresento tutto sotto forma di serie temporale
moda_anni <- ts(moda_per_anno, start = 2010, frequency = 1)

#Creo il grafico relativo a questa serie temporale
plot(moda_anni, main="Moda dei bambini nati per donna anno per anno", 
     xlab="Anno", ylab="Valori", col="blue", type="o")

#_________________________________________________________________________
#Varianza anno per anno per vedere la dispersione dei dati
#Anche qui, commentare nella documentazione
#Tanto maggiore è la varianza tanto più i valori sono dispersi rispetto 
#alla media
#Calcoliamo la varianza anno per anno

#ANNO 2010
mean(dati_2010)
var(dati_2010)
sd(dati_2010)
#La dispersione dei dati sembra essere moderata, in quanto sia varianza
#che deviazione standard non sono molto alti.

#ANNO 2011
mean(dati_2011)
var(dati_2011)
sd(dati_2011)
#Vale lo stesso del 2010, perché i valori sono simili.

#ANNO 2012
mean(dati_2012)
var(dati_2012)
sd(dati_2012)
#Vale come il 2010

#ANNO 2013
mean(dati_2013)
var(dati_2013)
sd(dati_2013)
#Qui i valori sono un po' più bassi, segno che sono un po' meno dispersi.

#ANNO 2014
mean(dati_2014)
var(dati_2014)
sd(dati_2014)
#Ancora più bassi, quindi ancora meno dispersione.

#ANNO 2015
mean(dati_2015)
var(dati_2015)
sd(dati_2015)
#Ancora più bassi.

#ANNO 2016
mean(dati_2016)
var(dati_2016)
sd(dati_2016)
#Ancora più bassi.

#ANNO 2017
mean(dati_2017)
var(dati_2017)
sd(dati_2017)
#Ancora più bassi.

#ANNO 2018
mean(dati_2018)
var(dati_2018)
sd(dati_2018)
#Leggermente più alti rispetto al 2017.

#ANNO 2019
mean(dati_2019)
var(dati_2019)
sd(dati_2019)
#Un po' più bassi rispetto al 2018.

#ANNO 2020
mean(dati_2020)
var(dati_2020)
sd(dati_2020)
#Più bassi rispetto al 2019.

#ANNO 2021
mean(dati_2021)
var(dati_2021)
sd(dati_2021)
#Un po' più alti rispetto al 2020.

#________________________________________________________________________
#Misuro la simmetria o meno di una distribuzione di frequenze con skewness
#Questo si fa per l'intero set di dati
library(e1071)

#Trasformo il dataset in un vettore numerico per poter calcolare skewness
dati_numerici <- as.numeric(as.character(unlist(data[, -1])))
dati_numerici

#Calcolo la skewness
skew <- skewness(dati_numerici)
skew
#Essendo che è maggiore di 0, significa che c'è asimmetria positiva.
#Un valore positivo della skewness indica che la coda della distribuzione 
#è più lunga a destra rispetto alla coda sinistra, cioè ci sono più dati 
#superiori alla media che spingono la coda in quella direzione.
#Questo perché ci sono dei Paesi con un numero di nascite superiori alla 
#media. La skewness si concentra sull'asimmetria della distribuzione.

#_________________________________________________________________________

#Per valutare meglio la skewness, è importante anche la curtosi perché
#così si valuta quanto i dati si concentrano nella coda della skewness.
curtosi <- kurtosis(dati_numerici)
curtosi

#Se la curtosi è positiva come in questo caso, significa che ci sono code 
#più pesanti rispetto a una distribuzione normale. Ciò potrebbe indicare 
#che ci sono Paesi con tassi di natalità significativamente superiori o 
#inferiori rispetto alla media.

#Rappresento graficamente le distribuzioni normale e quella dei dati

# Creo un vettore di valori per la curva di densità
x <- seq(min(dati_numerici) - 1, max(dati_numerici) + 1, length = 100)
# Calcolo la densità per la distribuzione normale
dens_normale <- dnorm(x, mean = mean(dati_numerici), sd = sd(dati_numerici))
# Calcolo la densità per i nostri dati
dens_dati <- density(dati_numerici)$y
# Creo il grafico di densità
plot(x, dens_normale, type = "l", col = "blue", lwd = 2, ylim = c(0, max(dens_normale, dens_dati)),
     main = "Confronto densità tra la distribuzione Normale e quella dei dati", xlab = "Valori", ylab = "Densità")
# Aggiungo la curva di densità per i dati
lines(density(dati_numerici), col = "red", lwd = 2)
# Aggiungi una legenda
legend("topright", legend = c("Distribuzione Normale", "Distribuzione dei dati"), col = c("blue", "red"), lty = 1, cex = 0.8)
# Aggiungo una linea per indicare la media
abline(v = mean(dati_numerici), col = "green", lty = 2)