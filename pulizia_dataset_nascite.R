# PULIZIA DATASET NASCITE
# Author: Daniela Amendola

# Si installa il pacchetto per la lettura del file Excel contenente il dataset
install.packages("readxl")

# Si importa la libreria per la lettura del file Excel contenente il dataset
library(readxl)

# Si aggiorna il pacchetto per evitare problemi di incompatibilità
update.packages("readxl")

# Si legge il file Excel e si salva nella variabile "data"
data <- read_excel("dataset/nascite.xlsx")

# Si visualizza il dataset
View(data)

#------------------------- Pulizia ---------------------------#

# Si rinominano le colonne per una manipolazione più agevole
colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9", "Col10", "Col11", "Col12", "Col13", "Col14", "Col15", "Col16")  

# Si eliminano le prime due righe di intestazione
data <- tail(data, -2)

# Le ultime 49 righe contengono il tasso di nascite ogni 1000 abitanti e non sono necessarie per l'analisi
data <- data[1:(nrow(data) - 49), ]

# Si elimina la prima colonna costituita solo dall'intestazione
data <- data[ , -1]

# Si elimina la seconda riga costituita solo dall'intestazione "Country"
data <- data[-2, ]

# Si elimina la terza colonna contenente solo valori nulli
data <- data[,-3]

# Si imposta il valore nella 40esima riga e 1a colonna a NA perché contiene l'intestazione dei paesi nella colonna successiva 
data[40, 1] <- NA

# Si spostano i valori dalla seconda colonna alla prima laddove ci siano valori NA
data$Col2 <- ifelse(is.na(data$Col2), data$Col3, data$Col2)

# Si elimina la seconda colonna che non è più necessaria
data <- data[, -2]

# Si rinominano le colonne nel modo opportuno 
colnames(data) <- c("Country", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")  

# A questo punto, si può eliminare anche la prima riga che contiene gli anni, dato che sono stati impostati come nomi delle colonne
data <- data[-1, ]

#---------------------- Inserimento dati mancanti ------------------------#

# Si inseriscono i dati mancanti
data[1, "2021"] <- "298.5" # Australia
data[4, "2021"] <- "373.7" # Canada
data[5, "2021"] <- "229.1" # Chile
data[20, "2021"] <- "818.5" # Japan
data[21, "2021"] <- "289" # Korea 
data[25, "2021"] <- "1880" # Mexico
data[37, "2021"] <- "677.2" # United Kingdom

# Altri dati mancanti per singoli paesi...
# Argentina
data[39, "2010"] <- "734.5"
data[39, "2011"] <- "739.3"
data[39, "2012"] <- "743.8"
data[39, "2013"] <- "747.4"
data[39, "2014"] <- "749.6"
data[39, "2015"] <- "750.3"
data[39, "2020"] <- "636"
data[39, "2021"] <- "629.4"

# Brazil
data[40, "2019"] <- "2890"
data[40, "2021"] <- "2760"

# India
data[44, "2010"] <- "26600"
data[44, "2011"] <- "26340"
data[44, "2012"] <- "26030"
data[44, "2013"] <- "25740"
data[44, "2014"] <- "24900"
data[44, "2015"] <- "24830"
data[44, "2020"] <- "23140"
data[44, "2021"] <- "23110"

# Peru
data[45, "2010"] <- "617.6"
data[45, "2011"] <- "613.2"
data[45, "2012"] <- "602.8"
data[45, "2013"] <- "592.9"
data[45, "2014"] <- "584.7"
data[45, "2015"] <- "577.6"

# Russia
data[47, "2015"] <- "1940"
data[47, "2016"] <- "1890"
data[47, "2017"] <- "1690"
data[47, "2018"] <- "1610"
data[47, "2019"] <- "1490"
data[47, "2020"] <- "1440"
data[47, "2021"] <- "1400"

# South Africa
data[48, "2021"] <- "1180"

#---------------------- Salvataggio dataset pulito ------------------------#

# Si stampa il dataframe per verificare le modifiche
View(data)

# Si installa il pacchetto per salvare l'excel pulito
install.packages("writexl")

# Si carica il pacchetto
library(writexl)

# Si aggiorna il pacchetto per evitare problemi di incompatibilità
update.packages("writexl")

# Si salva il file Excel
write_xlsx(data, "dataset_puliti/nascite_pulito.xlsx")

#---------------------- Conversione dati e salvataggio ------------------------#

# Essendo i dati in formato character, si convertono in numeric
# Si selezionano le colonne da convertire (escludendo la prima con i nomi dei paesi)
columns_to_convert <- names(data)[-1]

# Si convertono le colonne selezionate in formato numerico
data[columns_to_convert] <- lapply(data[columns_to_convert], function(x) as.numeric(as.character(x)))

# Si arrotondano le colonne convertite alla seconda cifra decimale
data[columns_to_convert] <- round(data[columns_to_convert], 2)

# Si visualizza il dataset
View(data)

# Si salva il file Excel con i dati convertiti
write_xlsx(data, "dataset_puliti/nascite_arrotondato.xlsx")

