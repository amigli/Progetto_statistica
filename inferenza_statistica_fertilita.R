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


#Caso 2: intervallo per varianza con media non nota
#Calcolo la media del campione
mean(dataValues)

#Calcolo la varianza del campione
var(dataValues)

#Calcolo gli intervalli
(n-1)*var(dataValues)/qchisq(1-alpha/2, df = n-1)
(n-1)*var(dataValues)/qchisq(alpha/2, df = n-1)
