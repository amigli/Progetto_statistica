library(readxl)

data <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

# Elimino le colonne che non mi servono per l'analisi,
# lascio solo la colonna del 2021 e quella dei Paesi
data <- data[, c("Country", "2021")]

# Seleziono i paesi che hanno un valore nel 2021 inferiore a 100
data <- data[(data$`2021` < 100), ]

View(data)

# Trasformo la colonna del 2021 in un array
data_array <- as.array(data$`2021`)
data_array

################################################################################
#------------------- Criterio del Chi-Quadrato --------------------------------#
################################################################################
#---------- Verifico se è una distribuzione normale

n <- length(data_array) # Numero di elementi
n

m <- mean(data_array) # Media
m

sd <- sd(data_array) # Deviazione standard
sd

# Utilizzo i quantili della distribuzione normale per determinare i sottoinsiemi
a <-  numeric(4)
for(i in 1:4)
  a[i] <- qnorm(0.2 * i, mean = m, sd = sd)
a

# Numero di elementi nei vari intervalli
r <- 5
nint <- numeric(r)
nint[1] <- length(which(data_array < a[1]))
nint[2] <- length(which((data_array >= a[1]) & (data_array < a[2])))
nint[3] <- length(which((data_array >= a[2]) & (data_array < a[3])))
nint[4] <- length(which((data_array >= a[3]) & (data_array < a[4])))
nint[5] <- length(which(data_array >= a[4]))
nint

chi2 <- sum(((nint - n * 0.2)/sqrt(n * 0.2))^2)
chi2

r <- 5
k <- 2
alpha <- 0.05

# Se chi2 è compreso tra i due quantili, allora la popolazione è normale
# altrimenti non è normale
chi2 > qchisq(alpha/2, df=r-k-1) && chi2 < qchisq(1-alpha/2, df=r-k-1)

################################################################################
#----------------------------- Stima puntuale ---------------------------------#
################################################################################

# Calcolo la stima puntuale della media
stima_mu <- mean(data_array)
stima_mu

var <- var(data_array) # Varianza
var

n <- length(data_array) # Numero di elementi
n

# Calcolo la stima puntuale della varianza
stima_sigma2 <- (length(data_array)-1) * var/n
stima_sigma2

################################################################################
#------------------------- Intervalli di confidenza ---------------------------#
################################################################################
               #-------------- Metodo pivotale --------------#
#-------- Intervallo di confidenza per il valore medio con varianza NON nota

mean <- mean(data_array) # Media
mean

sd <- sd(data_array) # Deviazione standard
sd

alpha <- 1 - 0.95 

qt(1-alpha/2, df = n - 1) 

n <- length(data_array) # Numero di elementi
n

# Calcolo l'intervallo di confidenza
lim_inf <- mean - qt(1 - alpha/2, df = n - 1) * sd/sqrt(n)
lim_inf

lim_sup <- mean + qt(1 - alpha/2, df = n - 1) * sd/sqrt(n)
lim_sup

# Verifico se la media è compresa nell'intervallo di confidenza
mean > lim_inf && mean < lim_sup

################################################################################
#-------- Intervallo di confidenza per la varianza con valore medio NON noto

mean <- mean(data_array) # Media
mean

var <- var(data_array) # Varianza
var

n <- length(data_array) # Numero di elementi
n

alpha <- 1 - 0.95
alpha

# Calcolo l'intervallo di confidenza
X2_alpha <- qchisq(alpha/2, df = n - 1)
X2_alpha

X2_1alpha <- qchisq(1 - alpha/2, df = n - 1)
X2_1alpha

lim_inf <- (n - 1) * var / X2_1alpha
lim_inf

lim_sup <- (n - 1) * var / X2_alpha
lim_sup

# Verifico se la varianza è compresa nell'intervallo di confidenza
var > lim_inf && var < lim_sup

################################################################################
#-------------------- Confronto tra due popolazioni ---------------------------#
################################################################################

# Carico il dataset
data2 <- read_excel("dataset_puliti/nascite_arrotondato.xlsx")

# Elimino le colonne che non mi servono per l'analisi,
# lascio solo la colonna del 2010 e quella dei Paesi
data2 <- data2[, c("Country", "2010")]

# Selezioni i nomi dei Paesi che nel 2021 hanno un valore inferiore a 100
nomi_paesi <- data$Country

# Seleziono gli stessi paesi nel dataset del 2010
data2 <- data2[data2$Country %in% nomi_paesi, ]

View(data2)

# Trasformo la colonna del 2010 in un array
data_array2 <- as.array(data2$`2010`)
data_array2

#------------------- Criterio del Chi-Quadrato --------------------------------#
#--------- Verifico se è una distribuzione normale

n2 <- length(data_array2) # Numero di elementi
n2

mean2 <- mean(data_array2) # Media
mean2

sd2 <- sd(data_array2) # Deviazione standard
sd2

# Utilizzo i quantili della distribuzione normale per determinare i sottoinsiemi
a2 <-  numeric(4)
for(i in 1:4)
  a2[i] <- qnorm(0.2 * i, mean = mean2, sd = sd2)
a2

# Numero di elementi nei vari intervalli
r2 <- 5
nint2 <- numeric(r2)
nint2[1] <- length(which(data_array2 < a2[1]))
nint2[2] <- length(which((data_array2 >= a2[1]) & (data_array2 < a2[2])))
nint2[3] <- length(which((data_array2 >= a2[2]) & (data_array2 < a2[3])))
nint2[4] <- length(which((data_array2 >= a2[3]) & (data_array2 < a2[4])))
nint2[5] <- length(which(data_array2 >= a2[4]))
nint2

chi2 <- sum(((nint2 - n2 * 0.2)/sqrt(n2 * 0.2))^2)
chi2

r2 <- 5
k2 <- 2
alpha <- 0.05

# Se chi2 è compreso tra i due quantili, allora la popolazione è normale
# altrimenti non è normale

chi2 > qchisq(alpha/2, df=r2-k2-1) && chi2 < qchisq(1-alpha/2, df=r2-k2-1)

#--------- Intervallo di confidenza per i valori medi con varianze NON note
alpha <- 1 - 0.99

qnorm(1 - alpha/2, mean = 0, sd = 1)

# Numero di elementi
n <- 20

# Medie
mean1 <- mean(data_array)
mean1

mean2 <- mean(data_array2)
mean2

# Deviazioni standard
sigma1 <- sd(data_array)
sigma1

sigma2 <- sd(data_array2)
sigma2

# Calcolo l'intervallo di confidenza
lim_inf <- mean1 - mean2 - qnorm(1 - alpha/2, mean = 0, sd = 1) * sqrt(sigma1^2/n + sigma2^2/n)
lim_inf

lim_sup <- mean1 - mean2 + qnorm(1 - alpha/2, mean = 0, sd = 1) * sqrt(sigma1^2/n + sigma2^2/n)
lim_sup


################################################################################
#------------------------- Verifica delle ipotesi -----------------------------#
################################################################################

#-------- Test unilatera sinistro
alpha <- 0.01
mu0 <-  7000
n <- length(data_array)

qt(1 - alpha, df = n - 1)

# Media 
mean <- mean(data_array)

# Deviazione standard
sd <- sd(data_array)

# Stima statistica del test considerato
stima <- (mean - mu0)/(sd/sqrt(n))
stima

p_value <- 1 - pt(stima, df = n - 1)
p_value

p_value > alpha

# Grafico della densità t di Student
curve(dt(x, df = 5),
      from = -3,
      to = 3,
      axes = FALSE,
      ylim = c(0, 0.5),
      xlab = "",
      ylab = "",
      main = "Densità di Student con 19 gradi di libertà")
text(0, 0.05, 0.99)
text(0, 0.2, "Regione di \naccettazione")
axis(1, c(-3, -1, 0, 1, 3), c("", "-1083.209", "", "2.539483", ""))
vals <- seq(1, 3, length = 100)
x <- c(1, vals, 3, 1)
y <- c(0, dt(vals, df = 5), 0, 0)
polygon(x, y, density = 20, angle = 45)
abline(h = 0)
text(1.5, 0.05, 0.01)
text(2.2, 0.1, "Regione di \nrifiuto")
box()

