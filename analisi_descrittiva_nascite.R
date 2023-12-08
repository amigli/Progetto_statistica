library(readxl)
library(e1071)

data <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

# Visualizzo il dataframe
View(data)


############################################################################################################################
# Funzione di distribuzione ogni 3 anni

# Verifico quali sono il valore minimo e massimo all'interno del dataset
min_dati <- min(data[, -1])
max_dati <- max(data[, -1])
print(min_dati)
print(max_dati)

# Essendo il valore massimo 26600, arrotondo per eccesso a 27000
max_dati <- 27000

# Essendo il numero di dati elevato, divido il dataset in intervalli
# Numero di intervalli
num_intervalli <- 8

# Calcolo la lunghezza di ogni intervallo
lunghezza_intervallo <- (max_dati - min_dati) / num_intervalli

# Genero gli intervalli
intervalli <- seq(min_dati, max_dati, by = lunghezza_intervallo)

# Divido il dataset in 3 gruppi, ognuno di 4 anni
# Il primo gruppo va dal 2010 al 2014
# Il secondo gruppo va dal 2015 al 2018
# Il terzo gruppo va dal 2019 al 2022
dati_2010_2013 <- data[, c("2010", "2011", "2012", "2013")]
dati_2014_2017 <- data[, c("2014", "2015", "2016", "2017")]
dati_2018_2021 <- data[, c("2018", "2019", "2020", "2021")]

# Converto tutti i dati in un vettore numerico in modo da poter operarci più facilmente
dati_2010_2013 <- as.numeric(as.character(unlist(dati_2010_2013)))
dati_2014_2017 <- as.numeric(as.character(unlist(dati_2014_2017)))
dati_2018_2021 <- as.numeric(as.character(unlist(dati_2018_2021)))

# Rimuovo la prima colonna (dove sono presenti i Paesi)
dati_senza_intestazioni <- data[, -1]

# Converto i dati in un formato numerico
dati_tot <- as.numeric(as.character(unlist(dati_senza_intestazioni)))

# Calcolo la frequenza assoluta per l'intero set di dati, escludendo la prima riga di 
# intestazione e la prima colonna, che contiene i nomi dei Paesi
freqA_tot <- table(cut(dati_tot, intervalli, right = FALSE))
freqA_tot

# Calcolo le frequenze assolute cumulate per l'intero set di dati
freqAC_tot <- cumsum(freqA_tot)
freqAC_tot

# Calcolo la frequenza assoluta per ogni gruppo
freqA_2010_2013 <- table(cut(dati_2010_2013, intervalli, right = FALSE))
freqA_2014_2017 <- table(cut(dati_2014_2017, intervalli, right = FALSE))
freqA_2018_2021 <- table(cut(dati_2018_2021, intervalli, right = FALSE))

# Calcolo la frequenza assoluta cumulata per ogni gruppo
freqAC_2010_2013 <- cumsum(freqA_2010_2013)
freqAC_2014_2017 <- cumsum(freqA_2014_2017)
freqAC_2018_2021 <- cumsum(freqA_2018_2021)

# Calcolo la frequenza relativa per l'intero set di dati, escludendo la prima riga di 
# intestazione e la prima colonna, che contiene i nomi dei Paesi
freqR_tot <- table(cut(dati_tot, intervalli, right = FALSE))/length(dati_tot)
freqR_tot

# Calcolo le frequenze relative cumulate per l'intero set di dati
freqRC_tot <- cumsum(freqR_tot)
freqRC_tot

# Calcolo la frequenza relativa per ogni gruppo
freqR_2010_2013 <- table(cut(dati_2010_2013,intervalli, right = FALSE))/length(dati_2010_2013)
freqR_2014_2017 <- table(cut(dati_2014_2017,intervalli, right = FALSE))/length(dati_2014_2017)
freqR_2018_2021 <- table(cut(dati_2018_2021,intervalli, right = FALSE))/length(dati_2018_2021)

# Calcolo la frequenza relativa cumulata per ogni gruppo
freqRC_2010_2013 <- cumsum(freqR_2010_2013)
freqRC_2014_2017 <- cumsum(freqR_2014_2017)
freqRC_2018_2021 <- cumsum(freqR_2018_2021)

################# Istogrammi #################
# Istogramma per l'intero set di dati

# Frequenza assoluta
hist_freqA_tot <- hist(dati_tot, breaks = intervalli, freq = TRUE,
                 ylim = c(0, max(freqA_tot) + 100),
                 col = rainbow(8), border = "black",
                 main = "Numero di bambini nati vivi tra il 2010 e il 2021",
                 xlab = "Range di valori", ylab = "Frequenza assoluta")

# Per una maggiore chiarezza inserisco delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(hist_freqA_tot$counts)) {
  text(hist_freqA_tot$mids[i], hist_freqA_tot$counts[i], 
       labels = hist_freqA_tot$counts[i], pos = 3, col = "black")
}

# Frequenza relativa
hist(dati_tot, breaks = intervalli, freq = FALSE,
     ylim = c(0, 0.0003),     
     col = rainbow(8), border = "black", 
     main = "Numero di bambini nati vivi tra il 2010 e il 2021",  
     xlab = "Range di valori", 
     ylab = "Frequenza relativa")

# Calcolo delle percentuali
percentuali_tot <- round((freqA_tot / sum(freqA_tot)) * 100, 2)
percentuali_2010_2013 <- round((freqA_2010_2013 / sum(freqA_2010_2013)) * 100, 2)
percentuali_2014_2017 <- round((freqA_2014_2017 / sum(freqA_2014_2017)) * 100, 2)
percentuali_2018_2021 <- round((freqA_2018_2021 / sum(freqA_2018_2021)) * 100, 2)

# Creo un vettore di etichette
intervalli_etichette <- c("[4,3380)", "[3380,6750)", "[6750,10100)", "[10100,13500)",
                         "[13500,16900)", "[16900,20300)", "[20300,23600)", "[23600,27000)")

# Istogrammi per raggruppamenti di 4 anni
# Frequenza assoluta
#-------- 2010-2013 --------#
hist_freqA_2010_2013 <- hist(dati_2010_2013, breaks = intervalli, freq = TRUE,
                             ylim = c(0, max(freqA_2010_2013) + 30),
                             col = rainbow(8), border = "black",
                             main = "Numero di bambini nati vivi tra il 2010 e il 2013",
                             xlab = "Range di valori", ylab = "Frequenza assoluta")

# Per una maggiore chiarezza inserisco delle etichette sopra le colonne,
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(hist_freqA_2010_2013$counts)) {
  text(hist_freqA_2010_2013$mids[i], hist_freqA_2010_2013$counts[i],
       labels = hist_freqA_2010_2013$counts[i], pos = 3, col = "black")
}

#-------- 2014-2017 --------#
hist_freqA_2014_2017 <- hist(dati_2014_2017, breaks = intervalli, freq = TRUE,
                             ylim = c(0, max(freqA_2014_2017) + 30),
                             col = rainbow(8), border = "black",
                             main = "Numero di bambini nati vivi tra il 2014 e il 2017",
                             xlab = "Range di valori", ylab = "Frequenza assoluta")

# Per una maggiore chiarezza inserisco delle etichette sopra le colonne,
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(hist_freqA_2014_2017$counts)) {
  text(hist_freqA_2014_2017$mids[i], hist_freqA_2014_2017$counts[i],
       labels = hist_freqA_2014_2017$counts[i], pos = 3, col = "black")
}

#-------- 2018-2021 --------#
hist_freqA_2018_2021 <- hist(dati_2018_2021, breaks = intervalli, freq = TRUE,
                             ylim = c(0, max(freqA_2018_2021) + 30),
                             col = rainbow(8), border = "black",
                             main = "Numero di bambini nati vivi tra il 2018 e il 2021",
                             xlab = "Range di valori", ylab = "Frequenza assoluta")

# Per una maggiore chiarezza inserisco delle etichette sopra le colonne,
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(hist_freqA_2018_2021$counts)) {
  text(hist_freqA_2018_2021$mids[i], hist_freqA_2018_2021$counts[i],
       labels = hist_freqA_2018_2021$counts[i], pos = 3, col = "black")
}

#--------------- Frequenza relativa ---------------#
hist(dati_2010_2013, breaks = intervalli, freq = FALSE,
     ylim = c(0, 0.0003),
     col = rainbow(8), border = "black",
     main = "Numero di bambini nati vivi tra il 2010 e il 2013",
     xlab = "Range di valori", ylab = "Frequenza relativa")

hist(dati_2014_2017, breaks = intervalli, freq = FALSE,
     ylim = c(0, 0.0003),
     col = rainbow(8), border = "black",
     main = "Numero di bambini nati vivi tra il 2014 e il 2017",
     xlab = "Range di valori", ylab = "Frequenza relativa")

hist(dati_2018_2021, breaks = intervalli, freq = FALSE,
     ylim = c(0, 0.0003),
     col = rainbow(8), border = "black",
     main = "Numero di bambini nati vivi tra il 2018 e il 2021",
     xlab = "Range di valori", ylab = "Frequenza relativa")

#--------------- Costruisco la tabella di contingenza ---------------#
contingenza_assolute <- cbind(freqA_2010_2013, freqA_2014_2017, freqA_2018_2021)
contingenza_assolute

#--------------- Frequenza assoluta congiunta ---------------#
barplot(t(contingenza_assolute), 
        col = c("blue", "green", "red"),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        ylim = c(0, 600),
        main = "Frequenza assoluta congiunta",
        names.arg = c("[4, 3380)", 
                      "[3380, 6750)", 
                      "[6750, 10100)", 
                      "[10100, 13500)", 
                      "[13500, 16900)", 
                      "[16900, 20300)", 
                      "[20300, 23600)", 
                      "[23600, 27000)"),
        las = 2,
        cex.names = 0.7)

# Per una maggiore chiarezza, vista la presenza di valori molto bassi per alcuni intervalli
# rifaccio il grafico togliendo il primo intervallo; ovvero, (4, 3333]. Così facendo 
# si riesce a vedere meglio la contingenza assoluta per gli altri intervalli
barplot(t(contingenza_assolute[-1, ]), 
        col = c("blue", "green", "red"),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza assoluta congiunta (senza il primo intervallo)",
        names.arg = c("[3380, 6750)", 
                      "[6750, 10100)", 
                      "[10100, 13500)", 
                      "[13500, 16900)", 
                      "[16900, 20300)", 
                      "[20300, 23600)", 
                      "[23600, 27000)"),
        las = 2,
        cex.names = 0.7)

# Costruisco la tabella di contingenza relativa
contingenza_relativa <- cbind(freqR_2010_2013, freqR_2014_2017, freqR_2018_2021)
contingenza_relativa

# Frequenza relativa congiunta
barplot(t(contingenza_relativa), 
        col = c("blue", "green", "red"),
        ylim = c(0, 3),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza relativa congiunta",
        names.arg = c("[4, 3380)", 
                      "[3380, 6750)", 
                      "[6750, 10100)", 
                      "[10100, 13500)", 
                      "[13500, 16900)", 
                      "[16900, 20300)", 
                      "[20300, 23600)", 
                      "[23600, 27000)"),
        las = 2,
        cex.names = 0.7)

# Per una maggiore chiarezza, vista la presenza di valori molto bassi per alcuni intervalli
# rifaccio il grafico togliendo il primo intervallo; ovvero, (4, 3333]. Così facendo
# si riesce a vedere meglio la contingenza relativa per gli altri intervalli
barplot(t(contingenza_relativa[-1, ]), 
        col = c("blue", "green", "red"),
        ylim = c(0, 0.07),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza relativa congiunta (senza il primo intervallo)",
        names.arg = c("[3380, 6750)", 
                      "[6750, 10100)", 
                      "[10100, 13500)", 
                      "[13500, 16900)", 
                      "[16900, 20300)", 
                      "[20300, 23600)", 
                      "[23600, 27000)"),
        las = 2,
        cex.names = 0.7)


# Distribuzione di frequenza assoluta marginale del numero di nascite
# Totalità di istanze per tutti gli anni messi insieme
freq_marg_ass <- margin.table(contingenza_assolute, 1)
freq_marg_ass

# Faccio la stessa cosa per la frequenza relativa
freq_marg_rel <- margin.table(contingenza_relativa, 1)
freq_marg_rel

# Calcolo la frequenza relativa condizionata
freq_cond_rel <- prop.table(contingenza_assolute, 1)
freq_cond_rel[is.nan(freq_cond_rel)] <- 0
freq_cond_rel

# Rappresentazione della frequenza relativa condizionata 
barplot(t(freq_cond_rel),
        beside = TRUE,
        col = c("blue", "green", "red"),
        ylim = c(0, 1),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza relativa condizionata (Quadriennio | N. Nascite)",
        names.arg = c("[4, 3380)", 
                      "[3380, 6750)", 
                      "[6750, 10100)", 
                      "[10100, 13500)", 
                      "[13500, 16900)", 
                      "[16900, 20300)", 
                      "[20300, 23600)", 
                      "[23600, 27000)"),
        las = 2,
        cex.names = 0.7)

############################################################################################################################

# Calcolo la media campionaria per ogni anno
means_by_year <- colMeans(data[, -1])

# Visualizzo la media campionaria per ogni anno
print(means_by_year)

# Rappresento la media sottoforma di serie temporale
ts_means <- ts(means_by_year, start = 2010, frequency = 1)

# Visualizzo la serie temporale
plot(ts_means, 
     main = "Media campionaria per anno", 
     ylim = c(1130, 1400),
     xlab = "Anno", 
     ylab = "Numero di nati vivi (in migliaia)", 
     col = "blue", 
     type = "o")

###############################################################################
# Boxplot quadriennali 

# 2010 - 2013
dati_2010 <- data[["2010"]]
dati_2011 <- data[["2011"]]
dati_2012 <- data[["2012"]]
dati_2013 <- data[["2013"]]

# Calcolo i quantili per ogni anno
quantili_2010 <- quantile(dati_2010)
quantili_2010

quantili_2011 <- quantile(dati_2011)
quantili_2011

quantili_2012 <- quantile(dati_2012)
quantili_2012

quantili_2013 <- quantile(dati_2013)
quantili_2013

# Visualizzo le principali informazioni (media, mediana, minimo e massimo) per ogni anno  
summary_2010 <- summary(dati_2010)
summary_2010

summary_2011 <- summary(dati_2011)
summary_2011

summary_2012 <- summary(dati_2012)
summary_2012

summary_2013 <- summary(dati_2013)
summary_2013

# Rappresento i boxplot
boxplot(dati_2010, dati_2011, dati_2012, dati_2013, 
        main = "Boxplot 2010 - 2013",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2010, dati_2011, dati_2012, dati_2013), 
                 max(dati_2010, dati_2011, dati_2012, dati_2013)),
        names = c("2010", "2011", "2012", "2013"),
        col = c("red", "lightblue", "green", "yellow"),
        border = "black")

# Essendo presenti degli outliers molto grandi, 
# di seguito si riportano i boxplot senza gli outliers
boxplot(dati_2010, dati_2011, dati_2012, dati_2013, 
        main = "Boxplot 2010 - 2013 senza outliers",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2010, dati_2011, dati_2012, dati_2013), 
                 max(dati_2010, dati_2011, dati_2012, dati_2013) - 25340),
        names = c("2010", "2011", "2012", "2013"),
        col = c("red", "lightblue", "green", "yellow"),
        border = "black")

# Verifico qual è il limite superiore
stats_2010 <- boxplot.stats(dati_2010)
stats_2010$stats[5]

stats_2011 <- boxplot.stats(dati_2011)
stats_2011$stats[5]

stats_2012 <- boxplot.stats(dati_2012)
stats_2012$stats[5]

stats_2013 <- boxplot.stats(dati_2013)
stats_2013$stats[5]

# 2014 - 2017
dati_2014 <- data[["2014"]]
dati_2015 <- data[["2015"]]
dati_2016 <- data[["2016"]]
dati_2017 <- data[["2017"]]

# Calcolo i quantili per ogni anno
quantili_2014 <- quantile(dati_2014)
quantili_2014

quantili_2015 <- quantile(dati_2015)
quantili_2015

quantili_2016 <- quantile(dati_2016)
quantili_2016

quantili_2017 <- quantile(dati_2017)
quantili_2017

# Visualizzo le principali informazioni (media, mediana, minimo e massimo) per ogni anno
summary_2014 <- summary(dati_2014)
summary_2014

summary_2015 <- summary(dati_2015)
summary_2015

summary_2016 <- summary(dati_2016)
summary_2016

summary_2017 <- summary(dati_2017)
summary_2017

# Rappresento i boxplot
boxplot(dati_2014, dati_2015, dati_2016, dati_2017, 
        main = "Boxplot 2014 - 2017",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2014, dati_2015, dati_2016, dati_2017), 
                 max(dati_2014, dati_2015, dati_2016, dati_2017)),
        names = c("2014", "2015", "2016", "2017"),
        col = c("red", "lightblue", "green", "yellow"),
        border = "black")

# Essendo presenti degli outliers molto grandi,
# di seguito si riportano i boxplot senza gli outliers
boxplot(dati_2014, dati_2015, dati_2016, dati_2017, 
        main = "Boxplot 2014 - 2017 senza outliers",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2014, dati_2015, dati_2016, dati_2017), 
                 max(dati_2014, dati_2015, dati_2016, dati_2017) - 23200),
        names = c("2014", "2015", "2016", "2017"),
        col = c("red", "lightblue", "green", "yellow"),
        border = "black")

# Verifico qual è il limite superiore
stats_2014 <- boxplot.stats(dati_2014)
stats_2014$stats[5]

stats_2015 <- boxplot.stats(dati_2015)
stats_2015$stats[5]

stats_2016 <- boxplot.stats(dati_2016)
stats_2016$stats[5]

stats_2017 <- boxplot.stats(dati_2017)
stats_2017$stats[5]

# 2018 - 2021
dati_2018 <- data[["2018"]]
dati_2019 <- data[["2019"]]
dati_2020 <- data[["2020"]]
dati_2021 <- data[["2021"]]

# Calcolo i quantili per ogni anno
quantili_2018 <- quantile(dati_2018)
quantili_2018

quantili_2019 <- quantile(dati_2019)
quantili_2019

quantili_2020 <- quantile(dati_2020)
quantili_2020

quantili_2021 <- quantile(dati_2021)
quantili_2021

# Visualizzo le principali informazioni (media, mediana, minimo e massimo) per ogni anno
summary_2018 <- summary(dati_2018)
summary_2018

summary_2019 <- summary(dati_2019)
summary_2019

summary_2020 <- summary(dati_2020)
summary_2020

summary_2021 <- summary(dati_2021)
summary_2021

# Rappresento i boxplot
boxplot(dati_2018, dati_2019, dati_2020, dati_2021, 
        main = "Boxplot 2018 - 2021",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2018, dati_2019, dati_2020, dati_2021), 
                 max(dati_2018, dati_2019, dati_2020, dati_2021)),
        names = c("2018", "2019", "2020", "2021"),
        col = c("red", "lightblue", "green", "yellow"),
        border = "black")

# Essendo presenti degli outliers molto grandi,
# di seguito si riportano i boxplot senza gli outliers
boxplot(dati_2018, dati_2019, dati_2020, dati_2021, 
        main = "Boxplot 2018 - 2021 senza outliers",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2018, dati_2019, dati_2020, dati_2021), 
                 max(dati_2018, dati_2019, dati_2020, dati_2021) - 23200),
        names = c("2018", "2019", "2020", "2021"),
        col = c("red", "lightblue", "green", "yellow"),
        border = "black")

# Verifico qual è il limite superiore
stats_2018 <- boxplot.stats(dati_2018)
stats_2018$stats[5]

stats_2019 <- boxplot.stats(dati_2019)
stats_2019$stats[5]

stats_2020 <- boxplot.stats(dati_2020)
stats_2020$stats[5]

stats_2021 <- boxplot.stats(dati_2021)
stats_2021$stats[5]

# Confronto tra primo e ultimo anno attraverso un boxplot ad intaglio
boxplot(dati_2010, dati_2021, 
        notch = TRUE,
        main = "Confronto boxplot 2010 e 2021",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2010, dati_2021), 
                 max(dati_2010, dati_2021)),
        names = c("2010", "2021"),
        col = c("green", "orange"),
        border = "black")

# Per una migliore visualizzazione, si riporta il boxplot senza outliers
boxplot(dati_2010, dati_2021, 
        notch = TRUE,
        main = "Confronto boxplot 2010 e 2021 senza outliers",
        ylab = "Numero di bambini nati vivi (in migliaia)",
        ylim = c(min(dati_2010, dati_2021), 
                 1400),
        names = c("2010", "2021"),
        col = c("green", "orange"),
        border = "black")

# Visualizzo i dettagli dei due boxplot
summary_2010
summary_2021

# Confronto la mediana del primo e dell'ultimo anno e verifico se si sovrappongono
IQR_2010 <- quantile(dati_2010, 0.75) - quantile(dati_2010, 0.25)
M1_2010 <- quantile(dati_2010, 0.5) - 1.57*IQR_2010/sqrt(length(dati_2010))
M2_2010 <- quantile(dati_2010, 0.5) + 1.57*IQR_2010/sqrt(length(dati_2010))

IQR_2021 <- quantile(dati_2021, 0.75) - quantile(dati_2021, 0.25)
M1_2021 <- quantile(dati_2021, 0.5) - 1.57*IQR_2021/sqrt(length(dati_2021))
M2_2021 <- quantile(dati_2021, 0.5) + 1.57*IQR_2021/sqrt(length(dati_2021))

# Intervallo di confidenza del 95% per la mediana del primo anno
c(M1_2010, M2_2010)

# Intervallo di confidenza del 95% per la mediana dell'ultimo anno
c(M1_2021, M2_2021)


###############################################################################
#------------------------ Varianza e deviazione standard ---------------------#

# Calcolo la varianza e la deviazione standard per ogni anno
varianza_2010 <- var(dati_2010)
deviazione_standard_2010 <- sd(dati_2010)

varianza_2011 <- var(dati_2011)
deviazione_standard_2011 <- sd(dati_2011)

varianza_2012 <- var(dati_2012)
deviazione_standard_2012 <- sd(dati_2012)

varianza_2013 <- var(dati_2013)
deviazione_standard_2013 <- sd(dati_2013)

varianza_2014 <- var(dati_2014)
deviazione_standard_2014 <- sd(dati_2014)

varianza_2015 <- var(dati_2015)
deviazione_standard_2015 <- sd(dati_2015)

varianza_2016 <- var(dati_2016)
deviazione_standard_2016 <- sd(dati_2016)

varianza_2017 <- var(dati_2017)
deviazione_standard_2017 <- sd(dati_2017)

varianza_2018 <- var(dati_2018)
deviazione_standard_2018 <- sd(dati_2018)

varianza_2019 <- var(dati_2019)
deviazione_standard_2019 <- sd(dati_2019)

varianza_2020 <- var(dati_2020)
deviazione_standard_2020 <- sd(dati_2020)

varianza_2021 <- var(dati_2021)
deviazione_standard_2021 <- sd(dati_2021)

# Rappresento tali dati sottoforma di serie temporale
ts_varianza <- ts(c(varianza_2010, varianza_2011, varianza_2012, varianza_2013, varianza_2014, varianza_2015,
                    varianza_2016, varianza_2017, varianza_2018, varianza_2019, varianza_2020, varianza_2021), 
                  start = 2010, frequency = 1)

ts_deviazione_standard <- ts(c(deviazione_standard_2010, deviazione_standard_2011, deviazione_standard_2012, 
                               deviazione_standard_2013, deviazione_standard_2014, deviazione_standard_2015,
                               deviazione_standard_2016, deviazione_standard_2017, deviazione_standard_2018, 
                               deviazione_standard_2019, deviazione_standard_2020, deviazione_standard_2021), 
                             start = 2010, frequency = 1)

# Visualizzo le due serie temporali 
plot(ts_varianza,
     type = 'o', 
     col = 'blue', 
     xlab = 'Anno', 
     ylab = 'Varianza', 
     main = 'Varianza nel tempo',
     yaxt = 'n')  # Per nascondere l'asse y automatico

# Modifico il formato dell'asse y
axis(2, at = pretty(ts_varianza), labels = format(pretty(ts_varianza), scientific = FALSE), mgp = c(2, 1, 0))

plot(ts_deviazione_standard, 
     type = 'o', 
     col = 'red', 
     xlab = 'Anno', 
     ylab = 'Deviazione Standard', 
     main = 'Deviazione Standard nel tempo')

###############################################################################
# Calcolo la skewness per tutto il dataset
skew <- skewness(dati_tot)

# Calcolo la curtosi 
curtosi <- kurtosis(dati_tot)

# Creo un vettore di valori per la curva di densità
x <- seq(min(dati_tot) - 5000, max(dati_tot) + 1, length = 1000)

# Calcolo la densità della distribuzione normale
densita_normale <- dnorm(x, mean = mean(dati_tot), sd = sd(dati_tot))

# Calcolo la densità per i dati
densita_dati <- density(dati_tot)$y

# Creo il grafico di densità
plot(x, densita_normale, 
     type = "l",
     col = "blue",
     main = "Densità della distribuzione dei dati",
     xlab = "Numero di bambini nati vivi (in migliaia)",
     ylab = "Densità",
     ylim = c(0, max(densita_normale, densita_dati)),
     xlim = c(min(dati_tot) - 5000, max(dati_tot) + 1),
     lwd = 1)

# Aggiungo la curva di densità per i dati
lines(density(dati_tot), col = "red", lwd = 1)

# Aggiungo una linea per indicare la media
abline(v = mean(dati_tot), col = "green", lty = 2)

# Aggiungo una legenda
legend("topright", 
       legend = c("Distribuzione Normale ", "Distribuzione dei dati ", "Media campionaria "), 
       col = c("blue", "red", "green"), 
       lty = c(1, 1, 2), 
       cex = 0.8)

# Calcolo il momento centrato campionario di ordine 2
momento_2 <- var(dati_tot)

# Calcolo il momento centrato campionario di ordine 4
momento_4 <- moment(dati_tot, order = 4)

# Calcolo l'indice di Pearson
indice_pearson <- momento_4 / momento_2^2

# Calcolo la curtosi campionaria
curtosi_campionaria <- indice_pearson - 3







