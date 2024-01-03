#ANALISI DESCRITTIVA DEL DATASET SULLA FERTILITA'


#________________________________________________________________________
#                     OPERAZIONI PRELIMINARI
#________________________________________________________________________

#Si importano le librerie necessarie
library(readxl)
library(e1071)

#Si legge il file Excel
data <- read_excel("C:/Users/migli/Desktop/Università/Magistrale/Statistica e Analisi dei Dati/progetto_statistica/dataset_puliti/fertilita_pulito.xlsx")
View(data)


#________________________________________________________________________
#       FREQUENZE RELATIVE E ASSOLUTE SULL'INTERO SET DI DATI
#________________________________________________________________________

#Si stampa il minimo del set di dati
print(min(data[, -1]))
#Si stampa il massimo del set di dati
print(max(data[, -1]))

#Si selezionano solo le colonne numeriche
data_numeric <- data[, sapply(data, is.numeric)]

#Si converte il dataframe in una matrice
data_matrix <- as.matrix(data_numeric)

#Si calcola la media
mean(data_matrix)

#Si calcola la deviazione standard
sqrt(var(as.vector(data_matrix)))

#Si definiscono gli intervalli, quindi
#tra 0 e 1.49 (1 bambino in media per donna)
#tra 1.50 e 2.49 (2 bambini in media per donna)
#dopo 2.50 (3 bambini in media per donna) 
intervalli <- c(0.5, 1.5, 2.5, 3.5) 

#Si calcolano quante istanze ci sono in ciascun range con un istogramma
hist(dati_numerici, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA)+92),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2010 e il 2021",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

#Si costruisce anche il grafico a torta per avere una più chiara
#interpretazione con le percentuali
pie(table(cut(dati_numerici, breaks = intervalli, 
              labels = c("0.5 <= x < 1.5",
                         "1.5 <= x < 2.5",
                         "2.5 <= x < 3.5"))), 
    col = c("red", "blue", "green"), 
    main = "Rappresentazione percentuali intervalli")

#Si crea lo stesso grafico, ma con le frequenze relative
hist(dati_numerici, breaks = intervalli, freq = FALSE,
     ylim = c(0, 1),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2010 e il 2021",
     xlab = "Range di valori medi", ylab = "Frequenza relativa")

#Si calcolano le frequenze assolute per intervalli
frequenzeA <- table(cut(dati_numerici, breaks = intervalli))

#Si calcolano le frequenze assolute cumulate per l'intero set di dati tenendo
#conto sempre degli intervalli
cumsum(frequenzeA)

#Si calcolano le frequenze relative per l'intero set di dati per intervalli 
frequenzeR <- table(cut(dati_numerici, breaks = intervalli))/length(dati_numerici)

#Si calcolano le frequenze relative cumulate per l'intero set di dati tenendo
#conto sempre degli intervalli
cumsum(frequenzeR)


#________________________________________________________________________
#                       DIVISIONE IN 3 RANGE DI ANNI
#________________________________________________________________________

#Si dividono i dati in 3 intervalli, ciascuno di 4 anni
dati_2010_2013 <- data[, c("2010", "2011", "2012", "2013")]
dati_2014_2017 <- data[, c("2014", "2015", "2016", "2017")]
dati_2018_2021 <- data[, c("2018", "2019", "2020", "2021")]

#Si convertono tutti i dati in un vettore numerico 
#In questo modo, ci si può operare più facilmente
dati_2010_2013 <- as.numeric(as.character(unlist(dati_2010_2013)))
dati_2014_2017 <- as.numeric(as.character(unlist(dati_2014_2017)))
dati_2018_2021 <- as.numeric(as.character(unlist(dati_2018_2021)))

#Si calcolano le frequenze assolute per ognuno
frequenzeA_10_13 <- table(cut(dati_2010_2013, breaks = intervalli))
frequenzeA_14_17 <- table(cut(dati_2014_2017, breaks = intervalli))
frequenzeA_18_21 <- table(cut(dati_2018_2021, breaks = intervalli))

#Si calcolano anche le frequenze assolute cumulate per ognuno
cumulateA_10_13 <- cumsum(frequenzeA_10_13)
cumulateA_14_17 <- cumsum(frequenzeA_14_17)
cumulateA_18_21 <- cumsum(frequenzeA_18_21)

#Si calcolano le frequenze relative per ognuno
frequenzeR_10_13 <- table(cut(dati_2010_2013, breaks = intervalli))/length(dati_2010_2013)
frequenzeR_14_17 <- table(cut(dati_2014_2017, breaks = intervalli))/length(dati_2014_2017)
frequenzeR_18_21 <- table(cut(dati_2018_2021, breaks = intervalli))/length(dati_2018_2021)

#Si calcolano anche le frequenze relative cumulate per ognuno
cumulateR_10_13 <- cumsum(frequenzeR_10_13)
cumulateR_14_17 <- cumsum(frequenzeR_14_17)
cumulateR_18_21 <- cumsum(frequenzeR_18_21)

#Si contano le istanze presenti in ciascun range con un istogramma
#tenendo conto della frequenza assoluta.
#Se ne costruisce uno per ogni quadriennio
hist(dati_2010_2013, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA_10_13) + 20),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2010 e il 2013",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

hist(dati_2014_2017, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA_14_17) + 45),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2014 e il 2017",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

hist(dati_2018_2021, breaks = intervalli, freq = TRUE,
     ylim = c(0, max(frequenzeA_18_21) + 20),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2018 e il 2021",
     xlab = "Range di valori medi", ylab = "Frequenza assoluta")

#Allo stesso modo, si calcola la frequenza relativa sempre con lo stesso grafico
#È analogo a quanto fatto per le frequenze assolute ma con le relative
hist(dati_2010_2013, breaks = intervalli, freq = FALSE,
     ylim = c(0, 1),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2010 e il 2013",
     xlab = "Range di valori medi", ylab = "Frequenza relativa")

hist(dati_2014_2017, breaks = intervalli, freq = FALSE,
     ylim = c(0, 1),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2014 e il 2017",
     xlab = "Range di valori medi", ylab = "Frequenza relativa")

hist(dati_2018_2021, breaks = intervalli, freq = FALSE,
     ylim = c(0, 1),
     col = c("red", "blue", "green"), border = "black",
     main = "Media bambini per donna tra il 2018 e il 2021",
     xlab = "Range di valori medi", ylab = "Frequenza relativa")

#Si crea una tabella di contingenza per le frequenze assolute
contingenza_assolute <- cbind(frequenzeA_10_13, frequenzeA_14_17, frequenzeA_18_21)
contingenza_assolute

#Distribuzione di frequenza assoluta marginale del numero di figli
#Qui si osserva la totalità di istanze per tutti gli anni insieme
freqMarginaleFigliA <- margin.table(contingenza_assolute, 1)

#Si crea la tabella di contingenza anche per le frequenze relative
contingenza_relative <- cbind(frequenzeR_10_13, frequenzeR_14_17, frequenzeR_18_21)
contingenza_relative

#Distribuzione di frequenza relativa marginale del numero di figli
#È analogo a quanto fatto con le frequenze assolute, ma è con 
#le frequenze relative
freqMarginaleFigliR <- margin.table(freqFigli, 1)

#Frequenza assoluta congiunta
#Viene trasposta in modo da ottenere su ogni barra i dati giusti
#Nella prima barra, si hanno 58, 48 e 58 per tutti i quadrienni
#Nella seconda barra, i valori sono 130, 144 e 134 per tutti i quadrienni
#Nella terza barra, si hanno 8, 4 e 4 per tutti i quadrienni
#Infatti, la somma della prima risulta 58+48+58=164, e così via anche
#per le altre barre
barplot(
  t(contingenza_assolute),
  col = c("blue", "green", "red"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  names.arg = c("[0.5,1.5)", "[1.5,2.5)", "[2.5,3.5)"),
  main = "Frequenza assoluta congiunta",
  xlab = "Numero di figli in media per donna"
)

#Frequenza relativa congiunta
#Sarebbe la proporzione di ciascun elemento nella matrice rispetto alla 
#somma totale degli elementi della matrice: ogni elemento viene diviso per
#il numero totale di elementi.
freqFigli <- prop.table(contingenza_assolute)
freqFigli

#Frequenza relativa congiunta
#La traspongo in modo da avere su ogni barra i dati giusti
#Il ragionamento è lo stesso delle frequenze assolute, ma con le relative
barplot(
  t(freqFigli),
  ylim = c(0,1),
  col = c("blue", "green", "red"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  names.arg = c("[0.5,1.5)", "[1.5,2.5)", "[2.5,3.5)"),
  main = "Frequenza relativa congiunta",
  xlab = "Numero di figli in media per donna"
)

#Frequenza relativa condizionata
#Quadriennio | figli
relativaCondizionata <- prop.table(contingenza_assolute,1)

#Si costruisce il grafico a barre
#È uguale a quello delle frequenze assolute, ma con quelle relative
barplot(
  t(relativaCondizionata),
  beside = TRUE,
  col = c("blue", "green", "red"),
  names.arg = c("[0.5,1.5)", "[1.5,2.5)", "[2.5,3.5)"),
  legend = c("2010-2013", "2014-2017", "2018-2021"),
  xlab = "Numero di figli in media per donna",
  ylab = "Frequenza relativa",
  main = "Frequenza relativa condizionata (Quadriennio | N. figli)",
  ylim = c(0, 1),
  legend.position = "top"
)


#________________________________________________________________________
#                       INDICE DI SINTESI: MEDIA
#________________________________________________________________________

#Si costruisce un grafico con le medie tra tutti i Paesi anno per anno
#Si calcola la media per ogni anno
media_per_anno <- colMeans(data[, -1])
media_per_anno

#Si rappresenta tutto sotto forma di serie temporale
media_anni <- ts(media_per_anno, start = 2010, frequency = 1)

#Si crea il grafico relativo a questa serie temporale
plot(media_anni, main="Numero medio di bambini nati per donna (2010-2021)", 
     xlab="Anno", ylab="Valore medio", col="blue", type="o")


#________________________________________________________________________
#                                   BOXPLOT
#________________________________________________________________________

#Si costruiscono i boxplot per verificare eventuali valori anomali anno per anno
#ANNO 2010
#Si estraggono i dati relativi al 2010
dati_2010 <- data[["2010"]]
#Si calcolano i quantili dei dati del 2010
quantile(dati_2010)
#Riepilogo statistico dei dati del 2010
summary(dati_2010)
#Si costruisce il boxplot
boxplot(dati_2010,
        main = "Boxplot anno 2010",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2010
stats <- boxplot.stats(dati_2010)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#A questo punto, è chiaro che vi è un solo outlier: Israel

#ANNO 2011
#Si estraggono i dati relativi al 2011
dati_2011 <- data[["2011"]]
#Si calcolano i quantili dei dati del 2011
quantile(dati_2011)
#Riepilogo statistico dei dati del 2011
summary(dati_2011)
#Si costruisce il boxplot
boxplot(dati_2011,
        main = "Boxplot anno 2011",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2011
stats <- boxplot.stats(dati_2011)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Anche in questo caso vi è un solo outlier: Israel

#ANNO 2012
#Si estraggono i dati relativi al 2012
dati_2012 <- data[["2012"]]
#Si calcolano i quantili dei dati del 2012
quantile(dati_2012)
#Riepilogo statistico dei dati del 2012
summary(dati_2012)
#Si costruisce il boxplot
boxplot(dati_2012,
        main = "Boxplot anno 2012",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2012
stats <- boxplot.stats(dati_2012)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2013
#Si estraggono i dati relativi al 2013
dati_2013 <- data[["2013"]]
#Si calcolano i quantili dei dati del 2013
quantile(dati_2013)
#Riepilogo statistico dei dati del 2013
summary(dati_2013)
#Si costruisce il boxplot
boxplot(dati_2013,
        main = "Boxplot anno 2013",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2013
stats <- boxplot.stats(dati_2013)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2014
#Si estraggono i dati relativi al 2014
dati_2014 <- data[["2014"]]
#Si calcolano i quantili dei dati del 2014
quantile(dati_2014)
#Riepilogo statistico dei dati del 2014
summary(dati_2014)
#Si costruisce il boxplot
boxplot(dati_2014,
        main = "Boxplot anno 2014",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2014
stats <- boxplot.stats(dati_2014)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2015
#Si estraggono i dati relativi al 2015
dati_2015 <- data[["2015"]]
#Si calcolano i quantili dei dati del 2015
quantile(dati_2015)
#Riepilogo statistico dei dati del 2015
summary(dati_2015)
#Si costruisce il boxplot
boxplot(dati_2015,
        main = "Boxplot anno 2015",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2015
stats <- boxplot.stats(dati_2015)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Outlier: Israel

#ANNO 2016
#Si estraggono i dati relativi al 2016
dati_2016 <- data[["2016"]]
#Si calcolano i quantili dei dati del 2016
quantile(dati_2016)
#Riepilogo statistico dei dati del 2016
summary(dati_2016)
#Si costruisce il boxplot
boxplot(dati_2016,
        main = "Boxplot anno 2016",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2016
stats <- boxplot.stats(dati_2016)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Outliers: Israel, India, Indonesia, Peru e Sud Africa

#ANNO 2017
#Si estraggono i dati relativi al 2017
dati_2017 <- data[["2017"]]
#Si calcolano i quantili dei dati del 2017
quantile(dati_2017)
#Riepilogo statistico dei dati del 2017
summary(dati_2017)
#Si costruisce il boxplot
boxplot(dati_2017,
        main = "Boxplot anno 2017",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2017
stats <- boxplot.stats(dati_2017)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Si osserva quanto misura il limite inferiore
limite_inferiore <- stats$stats[1]
limite_inferiore
#Outliers: Korea (Inferiore), Israele, India, Indonesia, Peru, Sud Africa

#ANNO 2018
#Si estraggono i dati relativi al 2018
dati_2018 <- data[["2018"]]
#Si calcolano i quantili dei dati del 2018
quantile(dati_2018)
#Riepilogo statistico dei dati del 2018
summary(dati_2018)
#Si costruisce il boxplot
boxplot(dati_2018,
        main = "Boxplot anno 2018",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2018
stats <- boxplot.stats(dati_2018)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Si osserva quanto misura il limite inferiore
limite_inferiore <- stats$stats[1]
limite_inferiore
#Outliers: Korea (inferiore), Israele, India, Indonesia, Peru, Sud Africa

#ANNO 2019
#Si estraggono i dati relativi al 2019
dati_2019 <- data[["2019"]]
#Si calcolano i quantili dei dati del 2019
quantile(dati_2019)
#Riepilogo statistico dei dati del 2019
summary(dati_2019)
#Si costruisce il boxplot
boxplot(dati_2019,
        main = "Boxplot anno 2019",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2019
stats <- boxplot.stats(dati_2019)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Si osserva quanto misura il limite inferiore
limite_inferiore <- stats$stats[1]
limite_inferiore
#Outliers: Korea (inferiore), Israele, India, Indonesia, Peru, Sud Africa

#ANNO 2020
#Si estraggono i dati relativi al 2020
dati_2020 <- data[["2020"]]
#Si calcolano i quantili dei dati del 2020
quantile(dati_2020)
#Riepilogo statistico dei dati del 2020
summary(dati_2020)
#Si costruisce il boxplot
boxplot(dati_2020,
        main = "Boxplot anno 2020",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2020
stats <- boxplot.stats(dati_2020)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Si osserva quanto misura il limite inferiore
limite_inferiore <- stats$stats[1]
limite_inferiore
#Outliers: Korea (inferiore), Israele, Indonesia, Perù, Sud Africa

#ANNO 2021
#Si estraggono i dati relativi al 2021
dati_2021 <- data[["2021"]]
#Si calcolano i quantili dei dati del 2021
quantile(dati_2021)
#Riepilogo statistico dei dati del 2021
summary(dati_2021)
#Si costruisce il boxplot
boxplot(dati_2021,
        main = "Boxplot anno 2021",
        ylab = "Valore",
        col = "lightblue",
        border = "black")
#Si calcolano le statistiche del boxplot per i dati del 2021
stats <- boxplot.stats(dati_2021)
#Si osserva quanto misura il limite superiore
limite_superiore <- stats$stats[5]
limite_superiore
#Si osserva quanto misura il limite inferiore
limite_inferiore <- stats$stats[1]
limite_inferiore
#Outliers: Korea (inferiore), Israele, Indonesia, Perù e Sud Africa

#Si confrontano gli anni a 4 a 4 come i range definiti prima
#Servono a capire l'andamento degli outlier e della mediana nel tempo
#DAL 2010 AL 2013
boxplot(dati_2010, dati_2011, dati_2012, dati_2013, 
        names = c("Anno 2010", "Anno 2011", "Anno 2012", "Anno 2013"),
        col = c("red", "lightblue", "green", "yellow"))
#DAL 2014 AL 2017
boxplot(dati_2014, dati_2015, dati_2016, dati_2017, 
        names = c("Anno 2014", "Anno 2015", "Anno 2016", "Anno 2017"),
        col = c("red", "lightblue", "green", "yellow"))
#DAL 2018 AL 2021
boxplot(dati_2018, dati_2019, dati_2020, dati_2021, 
        names = c("Anno 2018", "Anno 2019", "Anno 2020", "Anno 2021"),
        col = c("red", "lightblue", "green", "yellow"))

#Si effettua il confronto tra primo e ultimo anno tramite il boxplot ad intaglio
boxplot(dati_2010, dati_2021, notch = TRUE, 
        names = c("Anno 2010", "Anno 2021"), col = c("green", "orange"))

#Si verifica se le mediane si sovrappongono
#ANNO 2010
#Si calcola la distanza interquartile
IQR_2010 <- quantile(dati_2010, 0.75) - quantile(dati_2010, 0.25)
#Si calcola il limite inferiore per la mediana dei dati del 2010
M1_2010 <- quantile(dati_2010, 0.5) - 1.57*IQR_2010/sqrt(length(dati_2010))
#Si calcola il limite superiore per la mediana dei dati del 2010
M2_2010 <- quantile(dati_2010, 0.5) + 1.57*IQR_2010/sqrt(length(dati_2010))

#ANNO 2021
#Si calcola la distanza interquartile
IQR_2021 <- quantile(dati_2021, 0.75) - quantile(dati_2021, 0.25)
#Si calcola il limite inferiore per la mediana dei dati del 2021
M1_2021 <- quantile(dati_2021, 0.5) - 1.57*IQR_2021/sqrt(length(dati_2021))
#Si calcola il limite superiore per la mediana dei dati del 2021
M2_2021 <- quantile(dati_2021, 0.5) + 1.57*IQR_2021/sqrt(length(dati_2021))

#Si osserva se gli intervalli si sovrappongono
c(M1_2010, M2_2010)
c(M1_2021, M2_2021)
#Poiché i due intervalli non si sovrappongono, con un livello di 
#significatività del 5% si può affermare che le mediane dei voti delle
#due classi sono differenti.


#________________________________________________________________________
#                         INDICE DI SINTESI: MODA
#________________________________________________________________________

#Come fatto per la media, si valuta anche la moda nel corso del tempo
#Si calcola la moda
#Tramite essa, si capisce quale valore si presenta più spesso anno per anno
#Si costruisce una funzione, essendo che in R non è presente
moda <- function(v) {
  tmp <- unique(v)
  tmp[which.max(tabulate(match(v, tmp)))]
}

#Si calcola la moda per ogni anno
moda_per_anno <- sapply(data[, -1], moda)
moda_per_anno

#Si rappresenta tutto sotto forma di serie temporale
moda_anni <- ts(moda_per_anno, start = 2010, frequency = 1)

#Si crea il grafico relativo a questa serie temporale
plot(moda_anni, main="Moda di bambini nati per donna (2010-2021)", 
     xlab="Anno", ylab="Valore medio", col="blue", type="o")


#________________________________________________________________________
#                         INDICI DI DISPERSIONE:
#                     VARIANZA E DEVIAZIONE STANDARD
#________________________________________________________________________

#Varianza anno per anno per osservare la dispersione dei dati
#Tanto maggiore è la varianza tanto più i valori sono dispersi rispetto 
#alla media
#Si calcola la varianza anno per anno

#ANNO 2010
#Si calcola la media
mean(dati_2010)
#Si calcola la varianza
var(dati_2010)
#Si calcola la deviazione standard
sd(dati_2010)
#La dispersione dei dati sembra essere moderata, in quanto sia varianza
#che deviazione standard non sono molto alti.

#ANNO 2011
#Si calcola la media
mean(dati_2011)
#Si calcola la varianza
var(dati_2011)
#Si calcola la deviazione standard
sd(dati_2011)
#Anche qui, i dati non sono molto dispersi rispetto alla media.

#ANNO 2012
#Si calcola la media
mean(dati_2012)
#Si calcola la varianza
var(dati_2012)
#Si calcola la deviazione standard
sd(dati_2012)
#La dispersione dei dati è moderata.

#ANNO 2013
#Si calcola la media
mean(dati_2013)
#Si calcola la varianza
var(dati_2013)
#Si calcola la deviazione standard
sd(dati_2013)
#Qui i valori sono un po' più bassi, segno che sono un po' meno dispersi
#rispetto agli anni precedenti.

#ANNO 2014
#Si calcola la media
mean(dati_2014)
#Si calcola la varianza
var(dati_2014)
#Si calcola la deviazione standard
sd(dati_2014)
#Ancora più bassi rispetto al 2013, quindi ancora meno dispersione.

#ANNO 2015
#Si calcola la media
mean(dati_2015)
#Si calcola la varianza
var(dati_2015)
#Si calcola la deviazione standard
sd(dati_2015)
#I valori continuano a diminuire, segno di una maggiore concentrazione
#attorno alla media.

#ANNO 2016
#Si calcola la media
mean(dati_2016)
#Si calcola la varianza
var(dati_2016)
#Si calcola la deviazione standard
sd(dati_2016)
#I valori diminuiscono sempre più, quindi anche la dispersione è minore.

#ANNO 2017
#Si calcola la media
mean(dati_2017)
#Si calcola la varianza
var(dati_2017)
#Si calcola la deviazione standard
sd(dati_2017)
#I valori sono ancora più bassi, segno di una maggiore stabilità.

#ANNO 2018
#Si calcola la media
mean(dati_2018)
#Si calcola la varianza
var(dati_2018)
#Si calcola la deviazione standard
sd(dati_2018)
#I valori sono leggermente più alti rispetto al 2017, quindi alcuni valori
#si iniziano ad allontanare dalla media.

#ANNO 2019
#Si calcola la media
mean(dati_2019)
#Si calcola la varianza
var(dati_2019)
#Si calcola la deviazione standard
sd(dati_2019)
#I valori sono un po' più bassi rispetto al 2018, quindi è probabile che nel 
#2018 sia aumentata leggermente la fertilità e si sia riabbassata nel 2019.

#ANNO 2020
#Si calcola la media
mean(dati_2020)
#Si calcola la varianza
var(dati_2020)
#Si calcola la deviazione standard
sd(dati_2020)
#I valori sono ancora più bassi rispetto al 2019, e significa che i valori sono
#più concentrati intorno alla media.

#ANNO 2021
#Si calcola la media
mean(dati_2021)
#Si calcola la varianza
var(dati_2021)
#Si calcola la deviazione standard
sd(dati_2021)
#I valori sono un po' più alti rispetto al 2020, e significa che la fertilità
#è aumentata rispetto al 2020, perché ci sono valori più alti rispetto alla
#media, e di conseguenza sono più dispersi.

#Si calcola la varianza anno per anno
varianza_per_anno <- sapply(data[, -1], var)
varianza_per_anno

#Si calcola la deviazione standard anno per anno
devstandard_per_anno <- sapply(data[, -1], sd)
devstandard_per_anno

#Si rappresenta la varianza sotto forma di serie temporale
varianza_anni <- ts(varianza_per_anno, start = 2010, frequency = 1)

#Si rappresenta la deviazione standard sotto forma di serie temporale
devstandard_anni <- ts(devstandard_per_anno, start = 2010, frequency = 1)

#Si crea il grafico per la varianza 
plot(varianza_anni, col = "blue", type = "o", ylab = "Varianza", 
     xlab = "Anno", main = "Varianza e Deviazione Standard per Anno",
     ylim = c(0,0.5))

#Si aggiunge la deviazione standard al grafico esistente
lines(devstandard_anni, col = "red", type = "o")

#Si aggiunge una legenda
legend("topright", legend = c("Varianza", "Deviazione Standard"), 
       col = c("blue", "red"), lty = 1)


#________________________________________________________________________
#                                 SKEWNESS
#________________________________________________________________________

#Si misura la simmetria di una distribuzione di frequenze con la skewness
#Si trasforma il dataset in un vettore numerico per poter calcolare la skewness
dati_numerici <- as.numeric(as.character(unlist(data[, -1])))
dati_numerici

#Si calcola la skewness
skew <- skewness(dati_numerici)
skew
#Essendo che è maggiore di 0, significa che c'è asimmetria positiva.
#Un valore positivo della skewness indica che la coda della distribuzione 
#è più lunga a destra rispetto alla coda sinistra, cioè ci sono più dati 
#superiori alla media che spingono la coda in quella direzione.
#Questo perché ci sono dei Paesi con un numero di nascite superiori alla 
#media. La skewness si concentra sull'asimmetria della distribuzione.


#________________________________________________________________________
#                                   CURTOSI
#________________________________________________________________________

#Per valutare meglio la skewness, è importante anche la curtosi perché
#così si osserva quanto i dati si concentrano nella coda della skewness.
curtosi <- kurtosis(dati_numerici)
curtosi

#Si utilizzano i momenti per calcolare Beta2, che viene impiegata
#per calcolare la curtosi
momento_ordine_2 <- var(dati_numerici)
momento_ordine_4 <- moment(dati_numerici, order = 4)

#Si calcola Beta2
b2 <- (momento_ordine_4)/(momento_ordine_2^2)
b2

#Si calcola la curtosi
y2 <- b2-3
y2
#Essendo che b2 è maggiore di 3 e y2 è maggiore di 0, la distribuzione
#di frequenze si dice leptocurtica, cioè la distribuzione dei dati è più
#piccata di una normale.

#Si rappresenta graficamente la distribuzione dei dati confrontata
#con una normale.
#Si crea un vettore di valori per la curva di densità
x <- seq(min(dati_numerici) - 1, max(dati_numerici) + 1, length = 100)
#Si calcola la densità per la distribuzione normale
dens_normale <- dnorm(x, mean = mean(dati_numerici), sd = sd(dati_numerici))
#Si calcola la densità per i dati considerati
dens_dati <- density(dati_numerici)$y
#Si crea il grafico di densità
plot(x, dens_normale, type = "l", col = "blue", lwd = 2, ylim = c(0, max(dens_normale, dens_dati)),
     xlab = "Valori", ylab = "Densità")
#Si aggiunge la curva di densità per i dati
lines(density(dati_numerici), col = "red", lwd = 2)
#Si aggiunge una legenda
legend("topright", legend = c("Distribuzione Normale", "Distribuzione dei dati"), col = c("blue", "red"), lty = 1, cex = 0.8)
#Si aggiunge una linea per indicare la media
abline(v = mean(dati_numerici), col = "green", lty = 2)