#REGRESSIONE

library(readxl)
library(ggplot2)

data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

# Visualizzo il dataframe
View(data)


#___________________________________________________________________________


#Matrice delle covarianze
cov(data[, -1])

#Matrice delle correlazioni
cor(data[, -1])

#Eseguo la regressione lineare
#La variabile dipendente è l'anno 2021
#Le variabili indipendenti sono gli anni dal 2010 al 2020
model <- lm(data$`2021` ~ data$`2010` + data$`2011` + data$`2012` + 
              data$`2013` + data$`2014` + data$`2015` + data$`2016` + 
              data$`2017` + data$`2018` + data$`2019` + data$`2020`)
model

#Calcolo il vettore dei valori stimati
stime <- fitted(model)

#Calcolo il vettore dei residui
residui <- resid(model)

#Calcolo il vettore dei residui standard
residuiStandard <- residui/sd(residui)

#Costruisco il grafico dei residui in funzione dei valori stimati
plot(stime, residuiStandard, 
     main="Residui standard rispetto ai valori stimati", 
     xlab="Valori stimati", ylab="Residui standard", pch=5, col="red")
abline(h=0, col="blue", lty=2)

#Coefficiente di determinazione
summary(model)$r.square
