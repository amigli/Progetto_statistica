#INFERENZA STATISTICA DATASET FERTILITA'
#Author: Annalaura Miglino

#________________________________________________________________________
#                       OPERAZIONI PRELIMINARI
#________________________________________________________________________

library(readxl)

#Si legge il set di dati
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Si visualizzano i dati
View(data)


#________________________________________________________________________
#                       ELIMINAZIONE PAESI E ANNI
#________________________________________________________________________

#Si eliminano tutti gli anni lasciando solo il 2021
data <- data[, c("Country", "2021")]

#Vengono mantenute solo le righe per alcuni Paesi
data <- data[data$Country %in% c("Austria", "Belgium", "Czech Republic", 
                               "Denmark", "Estonia", "Finland", "France", 
                               "Germany", "Greece", "Hungary", "Ireland", 
                               "Italy", "Latvia", "Lithuania", "Luxembourg", 
                               "Netherlands", "Poland", "Portugal", 
                               "Slovak Republic", "Slovenia", "Spain", 
                               "Sweden", "Bulgaria", "Croatia", "Romania"), ]

#Si trasformano i valori in un array
dataValues <- as.array(data$'2021')


#________________________________________________________________________
#                       CRITERIO DEL CHI-QUADRATO
#________________________________________________________________________

#Si calcola la lunghezza del campione
n <- length(dataValues)
n

#Si calcola la media del campione
m <- mean(dataValues)
m

#Si calcola la deviazione standard del campione
d <- sd(dataValues)
d

#Si usano i quantili della distribuzione normale per determinare i sottoinsiemi
a <- numeric(4)
for (i in 1:4)
  a[i] <- qnorm(0.2*i, mean = m, sd = d)

a

#Si pone uguale a 5 il numero di intervalli
r <- 5

#Si inizializza un vettore numerico per memorizzare le frequenze degli intervalli
nint <- numeric (r)

#Vengono calcolate le frequenze degli intervalli
nint[1] <- length(which(dataValues < a[1]))
nint[2] <- length(which((dataValues >= a[1]) & (dataValues < a[2])))
nint[3] <- length(which((dataValues >= a[2]) & (dataValues < a[3])))
nint[4] <- length(which((dataValues >= a[3]) & (dataValues < a[4])))
nint[5] <- length(which(dataValues >= a[4]))

nint

#Si calcola il valore del test chi-quadro
chi2 <- sum(((nint-n*0.2)/sqrt(n*0.2))^2)
chi2

#Si specifica k e il livello di significatività alpha
k <- 2
alpha <- 0.05

#Si calcolano i valore critici per il test chi-quadro
qchisq(alpha/2, df = r-k-1)
qchisq(1-alpha/2, df= r-k-1)


#________________________________________________________________________
#                             STIMA PUNTUALE
#________________________________________________________________________

#Stimatore valore medio mu
mu <- mean(dataValues)
mu

#Stimatore della varianza sigma^2
sigma2 <- ((n-1)*var(dataValues))/n
sigma2


#________________________________________________________________________
#                           STIMA INTERVALLARE
#________________________________________________________________________

#Caso 1: intervallo per media con varianza non nota
#Si calcola la media del campione
mean(dataValues)

#Si calcola la deviazione standard del campione
sd(dataValues)

#Si imposta il valore di alpha
alpha <- 1-0.99

#Si calcolano gli estremi dell'intervallo di confidenza
mean(dataValues) - qt(1-alpha/2, df=n-1)*sd(dataValues)/sqrt(n)
mean(dataValues) + qt(1-alpha/2, df=n-1)*sd(dataValues)/sqrt(n)

#________________________________________________________________________

#Caso 2: intervallo per varianza con media non nota
#Si calcola la media del campione
mean(dataValues)

#Si calcola la varianza del campione
var(dataValues)

#Si calcolano gli estremi dell'intervallo di confidenza
(n-1)*var(dataValues)/qchisq(1-alpha/2, df = n-1)
(n-1)*var(dataValues)/qchisq(alpha/2, df = n-1)


#________________________________________________________________________
#                         CONFRONTO TRA POPOLAZIONI
#________________________________________________________________________

#Si verifica che sia normale anche la seconda popolazione (del 2010)

#OPERAZIONI PRELIMINARI
#Si legge di nuovo il set di dati perché prima è stato modificato
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Si eliminano tutti gli anni lasciando solo il 2010
data <- data[, c("Country", "2010")]

#Si mantengono solo le righe per alcuni Paesi
data <- data[data$Country %in% c("Austria", "Belgium", "Czech Republic", 
                                 "Denmark", "Estonia", "Finland", "France", 
                                 "Germany", "Greece", "Hungary", "Ireland", 
                                 "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                 "Netherlands", "Poland", "Portugal", 
                                 "Slovak Republic", "Slovenia", "Spain", 
                                 "Sweden", "Bulgaria", "Croatia", "Romania"), ]

#Si trasformano i valori in un array
dataValues2010 <- as.array(data$'2010')

#________________________________________________________________________

#Si effettua il test del chi-quadrato su questa popolazione
#Si calcola la lunghezza del campione
n <- length(dataValues2010)
n

#Si calcola la media del campione
m <- mean(dataValues2010)
m

#Si calcola la deviazione standard del campione
d <- sd(dataValues2010)
d

#Si usano i quantili della distribuzione normale per determinare i sottoinsiemi
a <- numeric(4)
for (i in 1:4)
  a[i] <- qnorm(0.2*i, mean = m, sd = d)

a

#Si pone uguale a 5 il numero di intervalli
r <- 5

#Inizializza un vettore numerico per memorizzare le frequenze degli intervalli
nint <- numeric (r)

#Vengono calcolate le frequenze degli intervalli
nint[1] <- length(which(dataValues2010 < a[1]))
nint[2] <- length(which((dataValues2010 >= a[1]) & (dataValues2010 < a[2])))
nint[3] <- length(which((dataValues2010 >= a[2]) & (dataValues2010 < a[3])))
nint[4] <- length(which((dataValues2010 >= a[3]) & (dataValues2010 < a[4])))
nint[5] <- length(which(dataValues2010 >= a[4]))

nint

#Si calcola il valore del test chi-quadro
chi2 <- sum(((nint-n*0.2)/sqrt(n*0.2))^2)
chi2

#Si specifica k e il livello di significatività alpha
k <- 2
alpha <- 0.05

#Si calcolano i valore critici per il test chi-quadro
qchisq(alpha/2, df = r-k-1)
qchisq(1-alpha/2, df= r-k-1)

#________________________________________________________________________

#CONFRONTO TRA LE DUE POPOLAZIONI

#Si imposta il valore di alpha
alpha <- 1-0.99

#Si calcola il quantile superiore della distribuzione normale standard
#corrispondente al livello di confidenza specificato
qnorm(1-alpha/2, mean = 0, sd = 1)

#La dimensione dei due campioni è uguale
n <- 25

#Si salvano le due medie
m1 <- mean(dataValues)
m2 <- mean(dataValues2010)

#Si salvano le deviazioni standard
s1 <- sqrt(var(dataValues))
s2 <- sqrt(var(dataValues2010))

#Si calcolano gli estremi dell'intervallo di confidenza
m1 - m2 - qnorm(1-alpha/2, mean = 0, sd = 1) * sqrt(s1^2/n + s2^2/n)
m1 - m2 + qnorm(1-alpha/2, mean = 0, sd = 1) * sqrt(s1^2/n + s2^2/n)


#________________________________________________________________________
#                         VERIFICA DELLE IPOTESI
#________________________________________________________________________

#Si impostano i valori di alpha, mu0 ed n
alpha <- 0.01
mu0 <- 1.5
n <- 25

#Si calcola il valore critico inferiore della distribuzione
qt(alpha, df = n-1)

#Si calcola la statistica di test standardizzata
(m1 - mu0)/(s1 / sqrt(n))

#Si calcola il p-value
pvalue <- pt(1.487066, df = n-1)
pvalue

#Viene creato il grafico
curve(dt(x, df = 5), from = -3, to = 3, axes = FALSE,
      ylim = c(0, 0.5), xlab = "", ylab = "", 
      main = "Densità di Student con 24 gradi di libertà")
text(0, 0.05, 0.99)
text(0, 0.2, "Regione di\naccettazione")
axis(1, c(-3, -1, 0, 1, 3), c("", "−2.492159", "", "1.487066", ""))
vals <- seq(-3, -1, length = 100)
x <- c(-3, vals, -1, -3)
y <- c(0, dt(vals, df = 5), 0, 0)
polygon(x, y, density = 20, angle = 45)
abline(h = 0)
text(-1.5, 0.05, expression(0.01))
text(-2.2, 0.1, "Regione di\nrifiuto")
box()