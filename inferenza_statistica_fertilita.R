#INFERENZA STATISTICA

#________________________________________________________________________
#                       OPERAZIONI PRELIMINARI
#________________________________________________________________________

library(readxl)

#Leggo il set di dati
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Visualizzo i dati
View(data)


#________________________________________________________________________
#                       ELIMINAZIONE PAESI E ANNI
#________________________________________________________________________


#Elimino tutti gli anni lasciando solo il 2021
data <- data[, c("Country", "2021")]

#Mantengo solo le righe per alcuni Paesi
data <- data[data$Country %in% c("Austria", "Belgium", "Czech Republic", 
                               "Denmark", "Estonia", "Finland", "France", 
                               "Germany", "Greece", "Hungary", "Ireland", 
                               "Italy", "Latvia", "Lithuania", "Luxembourg", 
                               "Netherlands", "Poland", "Portugal", 
                               "Slovak Republic", "Slovenia", "Spain", 
                               "Sweden", "Bulgaria", "Croatia", "Romania"), ]

#Trasformo i valori in un array
dataValues <- as.array(data$'2021')


#________________________________________________________________________
#                       CRITERIO DEL CHI-QUADRATO
#________________________________________________________________________


#Lunghezza del campione
n <- length(dataValues)
n

#Media del campione
m <- mean(dataValues)
m

#Deviazione standard del campione
d <- sd(dataValues)
d

#Uso i quantili della distribuzione normale per determinare i sottoinsiemi
a <- numeric(4)
for (i in 1:4)
  a[i] <- qnorm(0.2*i, mean = m, sd = d)

a

#Numero di elementi nei vari intervalli
r <- 5
nint <- numeric (r)

nint[1] <- length(which(dataValues < a[1]))
nint[2] <- length(which((dataValues >= a[1]) & (dataValues < a[2])))
nint[3] <- length(which((dataValues >= a[2]) & (dataValues < a[3])))
nint[4] <- length(which((dataValues >= a[3]) & (dataValues < a[4])))
nint[5] <- length(which(dataValues >= a[4]))

nint

#Calcolo X^2
chi2 <- sum(((nint-n*0.2)/sqrt(n*0.2))^2)
chi2

#Calcolo le due misure X^2 con aplha=0.05
k <- 2
alpha <- 0.05

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
#Cacolo la media del campione
mean(dataValues)

#Calcolo la deviazione standard del campione
sd(dataValues)

#Imposto il valore di alpha
alpha <- 1-0.99

#Calcolo gli intervalli
mean(dataValues) - qt(1-alpha/2, df=n-1)*sd(dataValues)/sqrt(n)
mean(dataValues) + qt(1-alpha/2, df=n-1)*sd(dataValues)/sqrt(n)

#________________________________________________________________________

#Caso 2: intervallo per varianza con media non nota
#Calcolo la media del campione
mean(dataValues)

#Calcolo la varianza del campione
var(dataValues)

#Calcolo gli intervalli
(n-1)*var(dataValues)/qchisq(1-alpha/2, df = n-1)
(n-1)*var(dataValues)/qchisq(alpha/2, df = n-1)


#________________________________________________________________________
#                         CONFRONTO TRA POPOLAZIONI
#________________________________________________________________________


#Verifico che sia normale anche la seconda popolazione (sul 2010)


#Operazioni preliminari
#Leggo di nuovo il set di dati perché prima è stato modificato
data <- read_excel("dataset_puliti/fertilita_arrotondato.xlsx")

#Elimino tutti gli anni lasciando solo il 2010
data <- data[, c("Country", "2010")]

#Mantengo solo le righe per alcuni Paesi
data <- data[data$Country %in% c("Austria", "Belgium", "Czech Republic", 
                                 "Denmark", "Estonia", "Finland", "France", 
                                 "Germany", "Greece", "Hungary", "Ireland", 
                                 "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                 "Netherlands", "Poland", "Portugal", 
                                 "Slovak Republic", "Slovenia", "Spain", 
                                 "Sweden", "Bulgaria", "Croatia", "Romania"), ]

#Trasformo i valori in un array
dataValues2010 <- as.array(data$'2010')


#________________________________________________________________________


#Effettuo il test del chi-quadrato su questa popolazione
#Lunghezza del campione
n <- length(dataValues2010)
n

#Media del campione
m <- mean(dataValues2010)
m

#Deviazione standard del campione
d <- sd(dataValues2010)
d

#Uso i quantili della distribuzione normale per determinare i sottoinsiemi
a <- numeric(4)
for (i in 1:4)
  a[i] <- qnorm(0.2*i, mean = m, sd = d)

a

#Numero di elementi nei vari intervalli
r <- 5
nint <- numeric (r)

nint[1] <- length(which(dataValues2010 < a[1]))
nint[2] <- length(which((dataValues2010 >= a[1]) & (dataValues2010 < a[2])))
nint[3] <- length(which((dataValues2010 >= a[2]) & (dataValues2010 < a[3])))
nint[4] <- length(which((dataValues2010 >= a[3]) & (dataValues2010 < a[4])))
nint[5] <- length(which(dataValues2010 >= a[4]))

nint

#Calcolo X^2
chi2 <- sum(((nint-n*0.2)/sqrt(n*0.2))^2)
chi2

#Calcolo le due misure X^2 con aplha=0.05
k <- 2
alpha <- 0.05

qchisq(alpha/2, df = r-k-1)

qchisq(1-alpha/2, df= r-k-1)


#________________________________________________________________________


#Confronto le due popolazioni
alpha <- 1-0.99

qnorm(1-alpha/2, mean = 0, sd = 1)

#La dimensione dei due campioni è uguale
n <- 25

#Salvo le due medie
m1 <- mean(dataValues)
m2 <- mean(dataValues2010)

#Salvo le deviazioni standard
s1 <- sqrt(var(dataValues))
s2 <- sqrt(var(dataValues2010))

m1 - m2 - qnorm(1-alpha/2, mean = 0, sd = 1) * sqrt(s1^2/n + s2^2/n)

m1 - m2 + qnorm(1-alpha/2, mean = 0, sd = 1) * sqrt(s1^2/n + s2^2/n)


#________________________________________________________________________
#                         VERIFICA DELLE IPOTESI
#________________________________________________________________________


alpha <- 0.01
mu0 <- 1.5
n <- 25

qt(alpha, df = n-1)

#La media ce l'ho con m1, la deviazione standard con s1

(m1 - mu0)/(s1 / sqrt(n))

#Calcolo il p-value
pvalue <- pt(1.487066, df = n-1)
pvalue

#Grafico
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