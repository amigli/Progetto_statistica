#PULIZIA DATASET FERTILITA'
#Author: Annalaura Miglino

#Si installa il pacchetto per leggere il file Excel che contiene il dataset
install.packages("readxl")

#Viene importata la libreria per leggere il file Excel che contiene il dataset
library(readxl)

#Si aggiorna il pacchetto in modo che non dia problemi di incompatibilità
update.packages("readxl")

#Si legge il file Excel e viene salvato nella variabile data
data <- read_excel("dataset/fertilità.xlsx")

#Si visualizza il contenuto di data in una finestra separata in modo da avere 
#una visualizzazione più completa
View(data)

#Si rinominano le colonne in modo da agevolare la manipolazione e la modifica dei valori
colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9", "Col10", "Col11", "Col12", "Col13", "Col14", "Col15", "Col16", "Col17")  # Assegno nomi più brevi alle colonne

#Si eliminano le prime due righe, in quanto contenenti descrizioni
data <- tail(data, -2)

#Si elimina la prima colonna, in quanto contiene informazioni riguardo le 
#variabili in esame
data <- data[, -1]

#Si rimuove l'ultima colonna perché sono presenti dati mancanti
data <- data[, -ncol(data)]

#Si rimuove l'ultima riga perché sono presenti dati mancanti
data <- data[-nrow(data), ]

#Si rimuove la seconda riga perché contiene l'intestazione della colonna Country
data <- data[-2, ]

#Si imposta il valore nella posizione [40,1 ] a NA perché contiene 
#l'intestazione dei Paesi che sono nella colonna successiva 
data[40, 1] <- NA

#Si elimina la terza colonna in quanto presenta solo valori NA
data <- data[, -3]

#Si spostano i valori dalla seconda colonna (Col3) alla prima (Col2) laddove 
#ci siano valori NA
data$Col2 <- ifelse(is.na(data$Col2), data$Col3, data$Col2)

#Si elimina la seconda colonna in quanto non è più necessaria
data <- data[, -2]

#Si rinominano le colonne nel modo opportuno 
colnames(data) <- c("Country", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")  

#Si elimina la prima riga che contiene gli anni, dato che sono stati impostati 
#come nomi delle colonne
data <- data[-1, ]

#Si stampa il dataframe per verificare che tutto sia stato modificato correttamente
View(data)

#Si installa il pacchetto per salvare l'excel pulito
install.packages("writexl")

#Si carica il pacchetto
library(writexl)

#Si aggiorna il pacchetto in modo che non dia problemi di incompatibilità
update.packages("writexl")

#Viene salvato il file Excel
write_xlsx(data, "dataset_puliti/fertilita_pulito.xlsx")

#Essendo i dati in formato character, vengono convertiti in numeric
#Le colonne da convertire  sono tutte, tranne la prima con i nomi dei Paesi
columns_to_convert <- names(data)[-1]

#Si convertono le colonne selezionate in formato numerico
data[columns_to_convert] <- lapply(data[columns_to_convert], 
                                   function(x) as.numeric(as.character(x)))

#Si arrotondano le colonne convertite alla seconda cifra decimale
data[columns_to_convert] <- round(data[columns_to_convert], 2)

#Viene visualizzato il dataset
View(data)

#Si salva tutto in un file excel
write_xlsx(data, "dataset_puliti/fertilita_arrotondato.xlsx")
