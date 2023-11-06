#installo il pacchetto per leggere il file Excel che contiene il dataset
install.packages("readxl")

#importo la libreria per leggere il file Excel che contiene il dataset
library(readxl)

#aggiorno il pacchetto in modo che non dia problemi di incompatibilità
update.packages("readxl")

#leggo il file Excel e lo salvo nella variabile data
data <- read_excel("C:\\Users\\migli\\Desktop\\Università\\Magistrale\\Statistica e Analisi dei Dati\\progetto_statistica\\dataset\\fertilità.xlsx")

#visualizzo il contenuto di data in una finestra separata in modo da avere una visualizzazione più completa
View(data)

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

#controllo le intestazioni delle colonne in modo da poterle manipolare più facilmente
colonne <- colnames(data)
print(colonne)

#rinomino le colonne in modo da agevolare la manipolazione e la modifica dei valori
colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9", "Col10", "Col11", "Col12", "Col13", "Col14", "Col15")  # Assegno nomi più brevi alle colonne

#sposto i valori dalla seconda colonna alla prima laddove ci siano valori NA
data$Col1 <- ifelse(is.na(data$Col1), data$Col2, data$Col1)
data$Col2 <- NULL

#essendo che la seconda colonna adesso contiene solo valori nulli, posso eliminarla
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
# Imposta una nuova directory di lavoro
setwd("C:\\Users\\migli\\Desktop\\Università\\Magistrale\\Statistica e Analisi dei Dati\\Progetto")
write_xlsx(data, "fertilita_pulito.xlsx")
