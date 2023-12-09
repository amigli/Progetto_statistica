library(readxl)

data <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

# Visualizzo il dataframe
View(data)

# Rimuovo la prima colonna (dove sono presenti i Paesi)
dati_senza_intestazioni <- data[, -1]

# Converto i dati in un formato numerico
dati_tot <- as.numeric(as.character(unlist(dati_senza_intestazioni)))


#####################################################################
# Calcolo la matrice delle covarianze campionarie
cov(data[, -1]) #(?)

# Calcolo la matrice delle correlazioni campionarie
cor(data[, -1]) #(?)

# Variabile dipendente 2021, 
# variabili indipendenti 2010-2011-2012-2013-2014-2015-2016-2017-2018-2019-2020 #

# Creo il modello di regressione lineare multivariata
model <- lm(data$`2021` ~ data$`2010` + data$`2011` + data$`2012` + data$`2013` + data$`2014` + 
            data$`2015` + data$`2016` + data$`2017` + data$`2018` + data$`2019` + data$`2020`, 
            data = data)

# Visualizzo il modello
summary(model)

# Ottengo i valori predetti dal modello
valori_predetti <- fitted(model)

# Creo un nuovo dataframe uguale a quello iniziale ma con i valori predetti nel 2021
new_data <- data
new_data$`2021` <- valori_predetti

# Arrotondo i valori predetti alla prima circa decimale
new_data$`2021` <- round(new_data$`2021`, digits = 1)

# Visualizzo il nuovo dataframe
View(new_data)

# Ottengo i residui del modello
residui <- resid(model)
residui

#####################################################################
# Confronto i valori predetti con quelli reali
# Creo un dataframe con i valori predetti e quelli reali
confronto <- data.frame(data$Country, data$`2021`, new_data$`2021`)

# Rinomino le colonne
colnames(confronto) <- c("Country", "Valori reali", "Valori predetti")

# Visualizzo il dataframe
View(confronto)

# Calcolo la percentuale di errore
errore <- (abs(confronto$`Valori reali` - confronto$`Valori predetti`)/confronto$`Valori reali`)*100

# Facendo la media degli errori ottenuti, ottengo la percentuale di errore media
mean(errore)

# Verifico se i valori predetti sono maggiori, minori o uguali a quelli reali
confronto$Comparison <- ifelse(confronto$`Valori predetti` > confronto$`Valori reali`, "Maggiore",
                        ifelse(confronto$`Valori predetti`< confronto$`Valori reali`, "Minore", "Uguale"))

# Conto i casi per ogni categoria
table(confronto$Comparison)




