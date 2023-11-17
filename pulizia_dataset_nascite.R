#installo il pacchetto per leggere il file Excel che contiene il dataset
install.packages("readxl")

#importo la libreria per leggere il file Excel che contiene il dataset
library(readxl)

#aggiorno il pacchetto in modo che non dia problemi di incompatibilità
update.packages("readxl")

#leggo il file Excel e lo salvo nella variabile data
data <- read_excel("dataset/nascite.xlsx")

#visualizzo il dataset
View(data)

#rinomino le colonne in modo da poterle manipolare più facilmente
colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9", "Col10", "Col11", "Col12", "Col13", "Col14", "Col15", "Col16")  

#elimino le prime due righe che sono di intestazione
data <- tail(data, -2)

#le ultime 49 righe contengono il rate di nascite ogni 1000 abitanti
#essendo che per questa analisi non servono questi dati, vengono eliminati
data <- data[1:(nrow(data) - 49), ]

#elimino la prima colonna, in quanto costituita solo dall'intestazione
data <- data[ , -1]

#elimino la seconda riga, in quanto costituita solo dall'intestazione "Country"
data <- data[-2, ]

#elimino la terza colonna, perché contiene solo valori nulli
data <- data[,-3]

# imposto il valore nella 40esima riga e 1a colonna a NA perché contiene l'intestazione dei paesi nella colonna successiva 
data[40, 1] <- NA

#sposto i valori dalla seconda colonna alla prima laddove ci siano valori NA
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
write_xlsx(data, "dataset_puliti/nascite_pulito.xlsx")