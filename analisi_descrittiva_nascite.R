# ANALISI DESCRITTIVA DEL DATASET SULLE NASCITE
# Author: Daniela Amendola 

library(readxl)
library(e1071)

data <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

# Si visualizza il dataframe
View(data)

################################################################################
#------------------------------- Frequenze ------------------------------------#
################################################################################

# Si calcolano il valore minimo e massimo all'interno del dataset
min_dati <- min(data[, -1])
max_dati <- max(data[, -1])
print(min_dati)
print(max_dati)

# Essendo il valore massimo 26600, si arrotonda per eccesso a 27000
max_dati <- 27000

# Essendo il numero di dati elevato, si divide il dataset in intervalli
# Numero di intervalli
num_intervalli <- 8

# Si calcola la lunghezza di ogni intervallo
lunghezza_intervallo <- (max_dati - min_dati) / num_intervalli

# Si generoano gli intervalli
intervalli <- seq(min_dati, max_dati, by = lunghezza_intervallo)

# Si divide il dataset in 3 quadrienni:
# Il primo quadriennio va dal 2010 al 2014
# Il secondo quadriennio  va dal 2015 al 2018
# Il terzo quadriennio va dal 2019 al 2022
dati_2010_2013 <- data[, c("2010", "2011", "2012", "2013")]
dati_2014_2017 <- data[, c("2014", "2015", "2016", "2017")]
dati_2018_2021 <- data[, c("2018", "2019", "2020", "2021")]

# Si convertono tutti i dati in un vettore numerico (in quanto sono di tipo character)
dati_2010_2013 <- as.numeric(as.character(unlist(dati_2010_2013)))
dati_2014_2017 <- as.numeric(as.character(unlist(dati_2014_2017)))
dati_2018_2021 <- as.numeric(as.character(unlist(dati_2018_2021)))

# Si rimuove la prima colonna (dove sono presenti i Paesi)
dati_senza_intestazioni <- data[, -1]

# Si convertono tutti i dati in un vettore numerico (in quanto sono di tipo character)
dati_tot <- as.numeric(as.character(unlist(dati_senza_intestazioni)))

#---------- Frequenza assoluta ----------#
# Si calcola la frequenza assoluta per l'intero set di dati, escludendo la prima riga di 
# intestazione e la prima colonna, che contiene i nomi dei Paesi
freqA_tot <- table(cut(dati_tot, intervalli, right = FALSE))
freqA_tot

# Si calcola la frequenza assoluta per ogni gruppo
freqA_2010_2013 <- table(cut(dati_2010_2013, intervalli, right = FALSE))
freqA_2014_2017 <- table(cut(dati_2014_2017, intervalli, right = FALSE))
freqA_2018_2021 <- table(cut(dati_2018_2021, intervalli, right = FALSE))

# Si calcolano le percentuali
percentuali_tot <- round((freqA_tot / sum(freqA_tot)) * 100, 2)
percentuali_2010_2013 <- round((freqA_2010_2013 / sum(freqA_2010_2013)) * 100, 2)
percentuali_2014_2017 <- round((freqA_2014_2017 / sum(freqA_2014_2017)) * 100, 2)
percentuali_2018_2021 <- round((freqA_2018_2021 / sum(freqA_2018_2021)) * 100, 2)

#---------- Frequenza assoluta cumulata ----------#
# Si calcola la frequenza assoluta cumulata per l'intero set di dati
freqAC_tot <- cumsum(freqA_tot)
freqAC_tot

# Si calcola  la frequenza assoluta cumulata per ogni gruppo
freqAC_2010_2013 <- cumsum(freqA_2010_2013)
freqAC_2014_2017 <- cumsum(freqA_2014_2017)
freqAC_2018_2021 <- cumsum(freqA_2018_2021)

#---------- Frequenza relativa ----------#
# Si calcola  la frequenza relativa per l'intero set di dati, escludendo la prima riga di 
# intestazione e la prima colonna, che contiene i nomi dei Paesi
freqR_tot <- table(cut(dati_tot, intervalli, right = FALSE))/length(dati_tot)
freqR_tot

# Si calcola la frequenza relativa per ogni gruppo
freqR_2010_2013 <- table(cut(dati_2010_2013,intervalli, right = FALSE))/length(dati_2010_2013)
freqR_2014_2017 <- table(cut(dati_2014_2017,intervalli, right = FALSE))/length(dati_2014_2017)
freqR_2018_2021 <- table(cut(dati_2018_2021,intervalli, right = FALSE))/length(dati_2018_2021)

#---------- Frequenza relativa cumulata ----------#
# Si calcola  le frequenze relative cumulate per l'intero set di dati
freqRC_tot <- cumsum(freqR_tot)
freqRC_tot

# Si calcola  la frequenza relativa cumulata per ogni gruppo
freqRC_2010_2013 <- cumsum(freqR_2010_2013)
freqRC_2014_2017 <- cumsum(freqR_2014_2017)
freqRC_2018_2021 <- cumsum(freqR_2018_2021)

#---------- Tabella di contingenza assoluta ----------#
contingenza_assolute <- cbind(freqA_2010_2013, freqA_2014_2017, freqA_2018_2021)
contingenza_assolute

#---------- Tabella di contingenza relativa ----------#
contingenza_relativa <- cbind(freqR_2010_2013, freqR_2014_2017, freqR_2018_2021)
contingenza_relativa

#---------- Frequenza relativa congiunta ----------#
freqRCong <-prop.table(contingenza_assolute)
freqRCong

#---------- Frequenza marginale ----------#
# Si calcola la frequenza assoluta marginale
freq_marg_ass <- margin.table(contingenza_assolute, 1)
freq_marg_ass

# Si calcola la frequenza relativa marginale
freq_marg_rel <- margin.table(contingenza_relativa, 1)
freq_marg_rel

#--------- Frequenza relativa condizionata ----------#
freq_cond_rel <- prop.table(contingenza_assolute, 1)
freq_cond_rel[is.nan(freq_cond_rel)] <- 0
freq_cond_rel

################################################################################
#----------------------------- Grafici frequenze ------------------------------#
################################################################################

# Si crea un vettore di etichette
intervalli_etichette <- c("[4, 3380)", "[3380, 6750)", "[6750, 10100)", "[10100, 13500)",
                          "[13500, 16900)", "[16900, 20300)", "[20300, 23600)", "[23600, 27000)")

#-------------------------- Grafici per l'intero set di dati --------------------------#
#---------- Frequenza assoluta ----------#
barplot_freqA_tot <- barplot(freqA_tot, 
                          names.arg = intervalli_etichette, 
                          ylim = c(0, max(freqA_tot) + 100),
                          col = rainbow(8),
                          main = "Numero di bambini nati vivi tra il 2010 e il 2021",
                          ylab = "Frequenza assoluta",
                          angle = 45, 
                          cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqA_tot)) {
  text(barplot_freqA_tot [i], freqA_tot[i] + 5, 
       labels = freqA_tot[i], pos = 3, col = "black", cex = 0.8)
}

#---------- Frequenza relativa ----------#
barplot_freqR_tot <- barplot(freqR_tot, 
        names.arg = intervalli_etichette, 
        ylim = c(0, 1.1),
        col = rainbow(8),
        main = "Numero di bambini nati vivi tra il 2010 e il 2021",
        ylab = "Frequenza relativa",
        angle = 45, 
        cex.names = 0.7)

# Si aggiungono le etichette sopra le colonne
for (i in 1:length(barplot_freqR_tot)) {
  text(barplot_freqR_tot[i], freqR_tot[i] + 0.01, 
       labels = sprintf("%.3f", freqR_tot[i]), pos = 3, col = "black", cex = 0.8)
}

#-------------------------- Grafici per ogni quadriennio --------------------------#
#--------------- Frequenza assoluta ---------------#
#-------- 2010-2013 --------#
barplot_freqA_10_13 <- barplot(freqA_2010_2013, 
                             names.arg = intervalli_etichette, 
                             ylim = c(0, max(freqA_2010_2013) + 50),
                             col = rainbow(8),
                             main = "Numero di bambini nati vivi tra il 2010 e il 2013",
                             ylab = "Frequenza assoluta",
                             angle = 45, 
                             cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqA_10_13)) {
  text(barplot_freqA_10_13[i], freqA_2010_2013[i] + 0.05, 
       labels = freqA_2010_2013[i], pos = 3, col = "black", cex = 0.8)
}

#-------- 2014-2017 --------#
barplot_freqA_14_17 <- barplot(freqA_2014_2017, 
                               names.arg = intervalli_etichette, 
                               ylim = c(0, max(freqA_2014_2017) + 50),
                               col = rainbow(8),
                               main = "Numero di bambini nati vivi tra il 2014 e il 2017",
                               ylab = "Frequenza assoluta",
                               angle = 45, 
                               cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqA_14_17)) {
  text(barplot_freqA_14_17[i], freqA_2014_2017[i] + 0.05, 
       labels = freqA_2014_2017[i], pos = 3, col = "black", cex = 0.8)
}

#-------- 2018-2021 --------#
barplot_freqA_18_21 <- barplot(freqA_2018_2021, 
                               names.arg = intervalli_etichette, 
                               ylim = c(0, max(freqA_2018_2021) + 50),
                               col = rainbow(8),
                               main = "Numero di bambini nati vivi tra il 2018 e il 2021",
                               ylab = "Frequenza assoluta",
                               angle = 45, 
                               cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqA_18_21)) {
  text(barplot_freqA_18_21[i], freqA_2018_2021[i] + 0.05, 
       labels = freqA_2018_2021[i], pos = 3, col = "black", cex = 0.8)
}

#--------------- Frequenza relativa ---------------#
#-------- 2010-2013 --------#
barplot_freqR_10_13 <- barplot(freqR_2010_2013, 
                             names.arg = intervalli_etichette, 
                             ylim = c(0, 1.1),
                             col = rainbow(8),
                             main = "Numero di bambini nati vivi tra il 2010 e il 2013",
                             ylab = "Frequenza relativa",
                             angle = 45, 
                             cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqR_10_13)) {
  text(barplot_freqR_10_13[i], freqR_2010_2013[i] + 0.01, 
       labels = sprintf("%.3f", freqR_2010_2013[i]), pos = 3, col = "black", cex = 0.8)
}

#-------- 2014-2017 --------#
barplot_freqR_14_17 <- barplot(freqR_2014_2017, 
                               names.arg = intervalli_etichette, 
                               ylim = c(0, 1.1),
                               col = rainbow(8),
                               main = "Numero di bambini nati vivi tra il 2014 e il 2017",
                               ylab = "Frequenza relativa",
                               angle = 45, 
                               cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqR_14_17)) {
  text(barplot_freqR_14_17[i], freqR_2014_2017[i] + 0.01, 
       labels = sprintf("%.3f", freqR_2014_2017[i]), pos = 3, col = "black", cex = 0.8)
}

#-------- 2018-2021 --------#
barplot_freqR_18_21 <- barplot(freqR_2018_2021, 
                               names.arg = intervalli_etichette, 
                               ylim = c(0, 1.1),
                               col = rainbow(8),
                               main = "Numero di bambini nati vivi tra il 2018 e il 2021",
                               ylab = "Frequenza relativa",
                               angle = 45, 
                               cex.names = 0.7)

# Per una maggiore chiarezza si inseriscono delle etichette sopra le colonne, 
# che vanno ad indicare il valore effettivo di ogni colonna
for (i in 1:length(barplot_freqR_18_21)) {
  text(barplot_freqR_18_21[i], freqR_2018_2021[i] + 0.01, 
       labels = sprintf("%.3f", freqR_2018_2021[i]), pos = 3, col = "black", cex = 0.8)
}



#--------------- Frequenza assoluta congiunta ---------------#
barplot(t(contingenza_assolute), 
        col = c("blue", "green", "red"),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        ylim = c(0, 600),
        main = "Frequenza assoluta congiunta",
        names.arg = intervalli_etichette,
        las = 2,
        cex.names = 0.7)

# Per una maggiore chiarezza, vista la presenza di valori molto bassi per alcuni intervalli
# si rifà il grafico rimuovendo il primo intervallo; ovvero, (4, 3380]. Così facendo 
# si riesce a vedere meglio la contingenza assoluta per gli altri intervalli
barplot(t(contingenza_assolute[-1, ]), 
        col = c("blue", "green", "red"),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza assoluta congiunta (senza il primo intervallo)",
        names.arg = intervalli_etichette[-1],
        las = 2,
        cex.names = 0.7)

#--------------- Frequenza relativa congiunta ---------------#
barplot(t(freqRCong), 
        col = c("blue", "green", "red"),
        ylim = c(0, 1),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza relativa congiunta",
        names.arg = intervalli_etichette,
        las = 2,
        cex.names = 0.7)

# Per una maggiore chiarezza, vista la presenza di valori molto bassi per alcuni intervalli
# si rifà il grafico rimuovendo il primo intervallo; ovvero, (4, 3380]. Così facendo 
# si riesce a vedere meglio la contingenza assoluta per gli altri intervalli
barplot(t(freqRCong[-1, ]), 
        col = c("blue", "green", "red"),
        ylim = c(0, 0.025),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza relativa congiunta (senza il primo intervallo)",
        names.arg = intervalli_etichette[-1],
        las = 2,
        cex.names = 0.7)

#--------------- Frequenza relativa condizionata ---------------#
barplot(t(freq_cond_rel),
        beside = TRUE,
        col = c("blue", "green", "red"),
        ylim = c(0, 1),
        legend = c("2010-2013", "2014-2017", "2018-2021"), 
        main = "Frequenza relativa condizionata (Quadriennio | N. Nascite)",
        names.arg = intervalli_etichette,
        las = 2,
        cex.names = 0.7)

################################################################################
#----------------------------- Media campionaria ------------------------------#
################################################################################

# Si calcola la media campionaria per ogni anno
means_by_year <- colMeans(data[, -1])
means_by_year

# Si rappresenta la media sottoforma di serie temporale
ts_means <- ts(means_by_year, start = 2010, frequency = 1)

# Grafico della media campionaria per anno 
plot(ts_means, 
     main = "Numero medio di nascite (2010−2021)", 
     ylim = c(1130, 1400),
     xlab = "Anno", 
     ylab = "Numero di nati vivi (in migliaia)", 
     col = "blue", 
     type = "o")

################################################################################
#----------------------------------- Boxplot ----------------------------------#
################################################################################

#-------- 2010-2013 --------#
dati_2010 <- data[["2010"]]
dati_2011 <- data[["2011"]]
dati_2012 <- data[["2012"]]
dati_2013 <- data[["2013"]]

# Si calcolano i quantili per ogni anno
quantili_2010 <- quantile(dati_2010)
quantili_2010

quantili_2011 <- quantile(dati_2011)
quantili_2011

quantili_2012 <- quantile(dati_2012)
quantili_2012

quantili_2013 <- quantile(dati_2013)
quantili_2013

# Si visualizzano le principali informazioni (media, mediana, minimo e massimo) per ogni anno  
summary_2010 <- summary(dati_2010)
summary_2010

summary_2011 <- summary(dati_2011)
summary_2011

summary_2012 <- summary(dati_2012)
summary_2012

summary_2013 <- summary(dati_2013)
summary_2013

# Si visualizzano i boxplot
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

# Si verifica qual è il limite superiore
stats_2010 <- boxplot.stats(dati_2010)
stats_2010$stats[5]

stats_2011 <- boxplot.stats(dati_2011)
stats_2011$stats[5]

stats_2012 <- boxplot.stats(dati_2012)
stats_2012$stats[5]

stats_2013 <- boxplot.stats(dati_2013)
stats_2013$stats[5]

#-------- 2014-2017 --------#
dati_2014 <- data[["2014"]]
dati_2015 <- data[["2015"]]
dati_2016 <- data[["2016"]]
dati_2017 <- data[["2017"]]

# Si calcolano i quantili per ogni anno
quantili_2014 <- quantile(dati_2014)
quantili_2014

quantili_2015 <- quantile(dati_2015)
quantili_2015

quantili_2016 <- quantile(dati_2016)
quantili_2016

quantili_2017 <- quantile(dati_2017)
quantili_2017

# Si visualizzano le principali informazioni (media, mediana, minimo e massimo) per ogni anno  
summary_2014 <- summary(dati_2014)
summary_2014

summary_2015 <- summary(dati_2015)
summary_2015

summary_2016 <- summary(dati_2016)
summary_2016

summary_2017 <- summary(dati_2017)
summary_2017

# Si visualizzano i boxplot
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

# Si verifica qual è il limite superiore
stats_2014 <- boxplot.stats(dati_2014)
stats_2014$stats[5]

stats_2015 <- boxplot.stats(dati_2015)
stats_2015$stats[5]

stats_2016 <- boxplot.stats(dati_2016)
stats_2016$stats[5]

stats_2017 <- boxplot.stats(dati_2017)
stats_2017$stats[5]

#-------- 2018-2021 --------#
dati_2018 <- data[["2018"]]
dati_2019 <- data[["2019"]]
dati_2020 <- data[["2020"]]
dati_2021 <- data[["2021"]]

# Si calcolano i quantili per ogni anno
quantili_2018 <- quantile(dati_2018)
quantili_2018

quantili_2019 <- quantile(dati_2019)
quantili_2019

quantili_2020 <- quantile(dati_2020)
quantili_2020

quantili_2021 <- quantile(dati_2021)
quantili_2021

# Si visualizzano le principali informazioni (media, mediana, minimo e massimo) per ogni anno  
summary_2018 <- summary(dati_2018)
summary_2018

summary_2019 <- summary(dati_2019)
summary_2019

summary_2020 <- summary(dati_2020)
summary_2020

summary_2021 <- summary(dati_2021)
summary_2021

# Si visualizzano i boxplot
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

# Si verifica qual è il limite superiore
stats_2018 <- boxplot.stats(dati_2018)
stats_2018$stats[5]

stats_2019 <- boxplot.stats(dati_2019)
stats_2019$stats[5]

stats_2020 <- boxplot.stats(dati_2020)
stats_2020$stats[5]

stats_2021 <- boxplot.stats(dati_2021)
stats_2021$stats[5]

# Si confrontano primo e ultimo anno attraverso un boxplot ad intaglio
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

# Si visualizzano i dettagli dei due boxplot
summary_2010
summary_2021

# Si confrontano la mediana del primo e dell'ultimo anno e si verifica se si sovrappongono
IQR_2010 <- quantile(dati_2010, 0.75) - quantile(dati_2010, 0.25)
IQR_2010
M1_2010 <- quantile(dati_2010, 0.5) - 1.57*IQR_2010/sqrt(length(dati_2010))
M2_2010 <- quantile(dati_2010, 0.5) + 1.57*IQR_2010/sqrt(length(dati_2010))

IQR_2021 <- quantile(dati_2021, 0.75) - quantile(dati_2021, 0.25)
IQR_2021
M1_2021 <- quantile(dati_2021, 0.5) - 1.57*IQR_2021/sqrt(length(dati_2021))
M2_2021 <- quantile(dati_2021, 0.5) + 1.57*IQR_2021/sqrt(length(dati_2021))

# Intervallo di confidenza del 95% per la mediana del primo anno
c(M1_2010, M2_2010)

# Intervallo di confidenza del 95% per la mediana dell'ultimo anno
c(M1_2021, M2_2021)


###############################################################################
#------------------------ Varianza e deviazione standard ---------------------#
###############################################################################

# Si calcola la varianza e la deviazione standard per ogni anno
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

# Si rappresenta la varianza sottoforma di serie temporale
ts_varianza <- ts(c(varianza_2010, varianza_2011, varianza_2012, varianza_2013, varianza_2014, varianza_2015,
                    varianza_2016, varianza_2017, varianza_2018, varianza_2019, varianza_2020, varianza_2021), 
                  start = 2010, frequency = 1)

# Si rappresenta la deviazione standard sottoforma di serie temporale
ts_deviazione_standard <- ts(c(deviazione_standard_2010, deviazione_standard_2011, deviazione_standard_2012, 
                               deviazione_standard_2013, deviazione_standard_2014, deviazione_standard_2015,
                               deviazione_standard_2016, deviazione_standard_2017, deviazione_standard_2018, 
                               deviazione_standard_2019, deviazione_standard_2020, deviazione_standard_2021), 
                             start = 2010, frequency = 1)

# Si visualizza il grafico della varianza nel tempo 
plot(ts_varianza,
     type = 'o', 
     col = 'blue', 
     xlab = 'Anno', 
     ylab = 'Varianza', 
     main = 'Varianza nel tempo',
     yaxt = 'n')  # Si nasconde l'asse y automatico, perchè verrà modificato

# Si modifica il formato dell'asse y
axis(2, at = pretty(ts_varianza), labels = format(pretty(ts_varianza), scientific = FALSE), mgp = c(2, 1, 0))

# Si visualizza il grafico della deviazione standard nel tempo 
plot(ts_deviazione_standard, 
     type = 'o', 
     col = 'red', 
     xlab = 'Anno', 
     ylab = 'Deviazione Standard', 
     main = 'Deviazione Standard nel tempo')

################################################################################
#------------------------ Skewness e curtosi ----------------------------------#
################################################################################

# Si calcola la skewness per tutto il dataset
skew <- skewness(dati_tot)

# Si calcola la curtosi 
curtosi <- kurtosis(dati_tot)

# Si crea un vettore di valori per la curva di densità
x <- seq(min(dati_tot) - 5000, max(dati_tot) + 1, length = 1000)

# Si calcola la densità della distribuzione normale
densita_normale <- dnorm(x, mean = mean(dati_tot), sd = sd(dati_tot))

# Si calcola la densità per i dati
densita_dati <- density(dati_tot)$y

# Grafico di densità
plot(x, densita_normale, 
     type = "l",
     col = "blue",
     main = "Densità della distribuzione dei dati",
     xlab = "Numero di bambini nati vivi (in migliaia)",
     ylab = "Densità",
     ylim = c(0, max(densita_normale, densita_dati)),
     xlim = c(min(dati_tot) - 5000, max(dati_tot) + 1),
     lwd = 1)

# Si aggiunge la curva di densità per i dati
lines(density(dati_tot), col = "red", lwd = 1)

# Si aggiunge una linea per indicare la media
abline(v = mean(dati_tot), col = "green", lty = 2)

# Si aggiunge la legenda
legend("topright", 
       legend = c("Distribuzione Normale ", "Distribuzione dei dati ", "Media campionaria "), 
       col = c("blue", "red", "green"), 
       lty = c(1, 1, 2), 
       cex = 0.8)

# Si calcola il momento centrato campionario di ordine 2
momento_2 <- var(dati_tot)

# Si calcola il momento centrato campionario di ordine 4
momento_4 <- moment(dati_tot, order = 4)

# Si calcola l'indice di Pearson
indice_pearson <- momento_4 / momento_2^2

# Si calcola la curtosi campionaria
curtosi_campionaria <- indice_pearson - 3
curtosi_campionaria