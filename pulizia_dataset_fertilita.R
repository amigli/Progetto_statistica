#installo il pacchetto per leggere il file Excel che contiene il dataset
install.packages("readxl")

#importo la libreria per leggere il file Excel che contiene il dataset
library(readxl)

#aggiorno il pacchetto in modo che non dia problemi di incompatibilità
update.packages("readxl")

#leggo il file Excel e lo salvo nella variabile data
data <- read_excel("dataset/fertilità.xlsx")

#visualizzo il contenuto di data in una finestra separata in modo da avere una visualizzazione più completa
View(data)

#rinomino le colonne in modo da agevolare la manipolazione e la modifica dei valori
colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9", "Col10", "Col11", "Col12", "Col13", "Col14", "Col15", "Col16", "Col17")  # Assegno nomi più brevi alle colonne

#elimino le prime due righe, in quanto contenenti descrizioni
data <- tail(data, -2)

#elimino la prima colonna, in quanto contiene informazioni riguardo le variabili in esame
data <- data[, -1]

# rimuovo l'ultima colonna perché sono presenti dati mancanti
data <- data[, -ncol(data)]

# rimuovo l'ultima riga perché sono presenti dati mancanti
data <- data[-nrow(data), ]

# rimuovo la seconda riga perché contiene l'intestazione della colonna Country
data <- data[-2, ]

# imposto il valore nella 40esima riga e 1a colonna a NA perché contiene l'intestazione dei paesi nella colonna successiva 
data[40, 1] <- NA

#elimino la terza colonna in quanto presenta solo valori NA
data <- data[, -3]

#sposto i valori dalla seconda colonna (Col3) alla prima (Col2) laddove ci siano valori NA
data$Col2 <- ifelse(is.na(data$Col2), data$Col3, data$Col2)

#elimino la seconda colonna in quanto non è più necessaria
data <- data[, -2]

#rinomino le colonne nel modo opportuno 
colnames(data) <- c("Country", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")  

#a questo punto, posso anche eliminare la prima riga che contiene gli anni, dato che sono stati impostati come nomi delle colonne
data <- data[-1, ]

#stampo il dataframe per verificare che tutto sia stato modificato correttamente
View(data)

#installo il pacchetto per salvare l'excel pulito
install.packages("writexl")

#carico il pacchetto
library(writexl)

#aggiorno il pacchetto in modo che non dia problemi di incompatibilità
update.packages("writexl")

#salvo il file Excel
write_xlsx(data, "dataset_puliti/fertilita_pulito.xlsx")

# Essendo i dati in formato character, li converto in numeric
# Seleziono le colonne da convertire (escludendo la prima con i nomi dei paesi)
columns_to_convert <- names(data)[-1]

# Converto le colonne selezionate in formato numerico
data[columns_to_convert] <- lapply(data[columns_to_convert], function(x) as.numeric(as.character(x)))

# Arrotondo le colonne convertite alla seconda cifra decimale
data[columns_to_convert] <- round(data[columns_to_convert], 2)

# Visualizzo il dataset
View(data)

# Salvo il file excel con i dati convertiti
write_xlsx(data, "dataset_puliti/fertilita_arrotondato.xlsx")
