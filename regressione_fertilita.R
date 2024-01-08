#REGRESSIONE SUL DATASET FERTILITA'
#Author: Annalaura Miglino

#________________________________________________________________________
#                       OPERAZIONI PRELIMINARI
#________________________________________________________________________

#Si importano le librerie necessarie
library(readxl)
library(writexl)
library(ggplot2)

#Si legge il set di dati
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Si visualizza il dataframe
View(data)


#________________________________________________________________________
#             REGRESSIONE LINEARE SINGOLA - 2021 e 2010
#________________________________________________________________________

#Si calcola la covarianza campionaria
cov(data$`2010`, data$`2021`)

#Si calcola il coefficiente di correlazione campionario
cor(data$`2010`, data$`2021`)

#Si crea lo scatterplot
plot(data$`2010`, data$`2021`, main = "Retta di regressione",
     xlab = "Fertilità nel 2010", ylab = "Fertilità nel 2021",
     col = "red")
abline(lm(data$`2021` ~ data$`2010`), col = "blue")


#________________________________________________________________________
#             REGRESSIONE LINEARE SINGOLA - 2021 e 2020
#________________________________________________________________________

#Si calcola la covarianza campionaria
cov(data$`2020`, data$`2021`)

#Si calcola il coefficiente di correlazione campionario
cor(data$`2020`, data$`2021`)

#Si crea lo scatterplot
plot(data$`2020`, data$`2021`, main = "Retta di regressione",
     xlab = "Fertilità nel 2020", ylab = "Fertilità nel 2021",
     col = "red")
abline(lm(data$`2021` ~ data$`2020`), col = "blue")


#________________________________________________________________________
#                     REGRESSIONE LINEARE MULTIPLA
#________________________________________________________________________

#Si calcola la matrice delle covarianze
matrice_covarianze <- cov(data[, -1])

#Si converte la matrice in un dataframe per poterla salvare in Excel
covarianze_df <- as.data.frame(as.matrix(matrice_covarianze))

#Si salva la matrice delle covarianze in un file Excel
write_xlsx(covarianze_df, "matrice_covarianze_fertilita.xlsx")

#Si calcola la matrice delle correlazioni
matrice_correlazioni <- cor(data[, -1])

#Si converte la matrice in un dataframe per poterla salvare in Excel
correlazioni_df <- as.data.frame(as.matrix(matrice_correlazioni))

#Si salva la matrice delle correlazioni in un file Excel
write_xlsx(correlazioni_df, "matrice_correlazioni_fertilita.xlsx")

#Si esegue la regressione lineare
#La variabile dipendente è l'anno 2021
#Le variabili indipendenti sono gli anni dal 2010 al 2020
model <- lm(data$`2021` ~ data$`2010` + data$`2011` + data$`2012` + 
              data$`2013` + data$`2014` + data$`2015` + data$`2016` + 
              data$`2017` + data$`2018` + data$`2019` + data$`2020`)
model

#Si calcola il vettore dei valori stimati
stime <- fitted(model)

#Si calcola il vettore dei residui
residui <- resid(model)

#Si calcola il vettore dei residui standard
residuiStandard <- residui/sd(residui)

#Si costruisce il grafico dei residui in funzione dei valori stimati
plot(stime, residuiStandard, 
     main="Residui standard rispetto ai valori stimati", 
     xlab="Valori stimati", ylab="Residui standard", pch=5, col="red")
abline(h=0, col="blue", lty=2)

#Si calcola il coefficiente di determinazione
summary(model)$r.square
