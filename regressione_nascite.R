# REGRESSIONE LINEARE SINGOLA E MULTIVARIATA SUL DATASET DELLE NASCITE
# Author: Daniela Amendola

library(readxl)
library(openxlsx)

data <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

# Si visualizza il dataframe
View(data)

# Si rimuove la prima colonna (dove sono presenti i Paesi)
dati_senza_intestazioni <- data[, -1]

# Si convertono i dati in un formato numerico
dati_tot <- as.numeric(as.character(unlist(dati_senza_intestazioni)))

################################################################################
#---------------------- Regressione lineare singola  --------------------------#
################################################################################

# Si calcola la matrice delle covarianze campionarie
cov(data$'2010', data$'2021')

# Si calcola la matrice delle correlazioni campionarie
cor(data$'2010', data$'2021') 

# Grafico
plot(data$'2010', data$'2021', 
     xlab = "Nascite nel 2010", 
     ylab = "Nascite nel 2021", 
     main = "Retta di regressione",
     col = "red",)
abline(lm(data$'2021' ~ data$'2010'), col = "blue")

################################################################################
#----------------------- Regressione lineare multivariata ---------------------#
################################################################################

# Si calcola la matrice delle covarianze campionarie
matrix_cov <- cov(data[, -1])

# Si converte la matrice delle covarianze in un dataframe
matrix_cov <- as.data.frame(matrix_cov)

# Si inseriscono i nomi delle righe e delle colonne
rownames(matrix_cov) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                   "2017", "2018", "2019", "2020", "2021")
colnames(matrix_cov) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                   "2017", "2018", "2019", "2020", "2021")

# Si salva la matrice delle covarianze in un file Excel
write.xlsx(matrix_cov, "matrice_covarianze_nascite.xlsx", rowNames = TRUE)

# Si calcola la matrice delle correlazioni campionarie
matrix_cor <- cor(data[, -1]) 

# Si converte la matrice delle correlazioni in un dataframe
matrix_cor <- as.data.frame(matrix_cor)

# Si inseriscono i nomi delle righe e delle colonne
rownames(matrix_cor) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                          "2017", "2018", "2019", "2020", "2021")
colnames(matrix_cor) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016",
                          "2017", "2018", "2019", "2020", "2021")

# Si salva la matrice delle correlazioni in un file Excel
write.xlsx(matrix_cor, "matrice_correlazioni_nascite.xlsx", rowNames = TRUE)

#--------------------------------------------------------------------------------#
# Variabile dipendente: 2021                                                     # 
# Variabili indipendenti: 2010-2011-2012-2013-2014-2015-2016-2017-2018-2019-2020 #
#--------------------------------------------------------------------------------#

# Si crea il modello di regressione lineare multivariata
model <- lm(data$`2021` ~ data$`2010` + data$`2011` + data$`2012` + data$`2013` + 
            data$`2014` + data$`2015` + data$`2016` + data$`2017` + data$`2018` + 
            data$`2019` + data$`2020`, data = data)

# Si visualizza il modello
summary(model)

#-------- Coefficiente di determinazione --------#
coeff_det <- summary(model)$r.squared
coeff_det

#-------- Valori stimati --------#
valori_stimati <- fitted(model)
valori_stimati

#-------- Residui--------#
residui <- resid(model)
residui

# Si calcola il vettore dei residui standardizzati
residui_standardizzati <- residui/sd(residui)
residui_standardizzati

#-------- Grafico dei residui --------#
# Si crea il grafico dei residui standardizzati in funzione dei valori stimati
plot(valori_stimati, residui_standardizzati, 
     xlab = "Valori stimati", 
     ylab = "Residui standardizzati", 
     main = "Grafico dei residui standardizzati \nin funzione dei valori stimati",
     col = "red",
     pch = 5,
     cex.lab = 1, 
     cex.axis = 1)
abline(h = 0, col = "blue", lty = 2)
