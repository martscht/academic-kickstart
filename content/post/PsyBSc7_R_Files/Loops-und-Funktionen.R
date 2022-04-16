#######################
### Loops und Funktionen
# von Julien Irmer, Johanna Schüller und Sebastian Wallot


###Logische Abfragen und Bedingungen "if" und "else"


##if-Abfragen

#Einfache if-Abfrage
a <- 3 #Zunächst definieren wir eine Variable

# mehrere Zeilen
if (a == 3) {
  print("Ja, die Variable a enthält den Wert 3")
}
# eine Zeile
if (a == 3) print("Ja, die Variable a enthält den Wert 3")


#if-Abfrage mit mehreren Möglichkeiten
person = "Monica"
if (person %in%  c("Monica", "Rachel", "Chandler",  "Phoebe", "Ross", "Joey")) {
  print("Yes, this is a character from Friends.")
}
#"%in%" = ist Element der folgenden Auswahl


#Abgleich mit einem Datum
if (weekdays(Sys.Date()) == "Friday") {
  print("Fast Wochenende!")
}


#Verknüpfung logischer Abfragen
if (weekdays(Sys.Date()) == "Saturday" | weekdays(Sys.Date()) == "Sunday") {
  print("Hoch die Hände, Wochenende!")
}


##Abgleich mit mehreren Alternativen: if-else-Abfragen
# mehrere Zeilen
if (weekdays(Sys.Date()) == "Saturday" | weekdays(Sys.Date()) == "Sunday") {
  print("Hoch die Hände, Wochenende!")
}else{
  print("Nur noch wenige Tage bis zum ersehnten Wochenende!")
}

# eine enorm lange Zeile
if (weekdays(Sys.Date()) == "Saturday" | weekdays(Sys.Date()) == "Sunday") print("Hoch die Hände, Wochenende!") else print("Nur noch wenige Tage bis zum ersehnten Wochenende!")

# geschweifte Klammern
if (weekdays(Sys.Date()) == "Saturday" | weekdays(Sys.Date()) == "Sunday")
{
  print("Hoch die Hände, Wochenende!")
}else
{
  print("Nur noch wenige Tage bis zum ersehnten Wochenende!")
}


#else-if-Bedingungen
if (weekdays(Sys.Date()) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) {
  if (weekdays(Sys.time()) == 'Monday') {
    print('Zurück ins Bett...')
  } else if (weekdays(Sys.time()) == 'Wednesday') {
    print('Wuhu, es ist Mitte der Woche!')
  } else if (weekdays(Sys.time()) == 'Friday') {
    print('Yeah, das Wochenende steht bevor!')
  } else {
    print('Es ist irgendein anderer Tag.')
  }
} else {
  print("Hoch die Hände, Wochenende!")
}

x <- sample(1:10, 1) #zufällig gezogene Zahl generieren
if (x > 5) { #wenn x > 5,
  y <- 1     #dann soll y = 1 sein
} else {     #wenn x <= 5,
  y <- 0     #dann soll y = 0 sein
}
x
y

#Funktion "if else"
ifelse(test = weekdays(Sys.Date()) == 'Friday', # die Bedingung
       yes = 'Yeah, das Wochenende steht bevor!', # was getan werden soll, wenn die Bedingung zutrifft
       no = 'Es ist irgendein anderer Tag...') # was getan werden soll, wenn die Bedingung nicht zutrifft


###Loops
#bieten die Möglichkeit, den gleichen R-Code mehrmals anzuwenden, ohne ihn wiederholt schreiben zu müssen


##for-Loops
#Abschnitt eines R-Codes wird für jedes Element in einem vorab festgelegten Objekt durchgeführt
vekt <- c("Hallo!", "Viel Spaß im R Praktikum.", "Viel Erfolg für das weitere Semester.") #Vektor erstellen
for (i in vekt) { #i wird nacheinander als jedes der drei Elemente (hier Wörter/Sätze) des Vektors definiert
  print(i)        #i wird in die Konsole geschrieben
}

load(url("https://pandar.netlify.app/post/mdbf.rda")) #Datensatz von pandaR laden
head(mdbf) #Überblick über Daten verschaffen

#4 negative Adjektive ins Positive umpolen
mdbf$stim4_r[mdbf$stim4 == 1] <- 4
mdbf$stim4_r[mdbf$stim4 == 2] <- 3
mdbf$stim4_r[mdbf$stim4 == 3] <- 2
mdbf$stim4_r[mdbf$stim4 == 4] <- 1

mdbf$stim4_r <- -1 * (mdbf$stim4 - 5) #Alternative

#schnellerer Weg über Loop:
mdbf_r <- mdbf #Kopie des Datensatzes erstellen, um Datenverlust vorzubeugen
neg <- c("stim3", "stim4", "stim5", "stim7", "stim9", "stim11") #Vektor der negativen Items erstellen

for (i in neg) {                        #i wird als Vektor der negativen Items definiert
  mdbf_r[, i] <- -1 * (mdbf_r[, i] - 5) #jedes Item aus dem Datensatz mit der Spalte i wird umgepolt
}
cor(mdbf[, 3], mdbf_r[, 3]) #erfolgreiches Umpolen überprüfen

#ineinandergeschachtelte Loops mit zwei Iterationen
Buchstaben <- c("A", "B", "C")
Zahlen <- c(1,2)
for (i in Buchstaben) {
  for (ii in Zahlen) {
    print(i)
    print(ii)
  }
}



###Funktionen
#eigene Funktionen erstellen

eigene_funktion <- function(argument1, argument2, ...) { #Name der Funktion #Argumente, die die Funktion annehmen soll
  # Durchgeführte Operationen
}

##Beispiel Varianzfunktion
x <- mdbf[, 1]                   #Variable als Vektor festlegen, deren Varianz bestimmt werden soll
n <- length(x)                   #n = Länge des Vektors (Anzahl von Werten/Personen)
s2 <- sum((x - mean(x))^2) / n   #Funktion bestimmen
s2                               #Achtung!: Nur Varianz für ein einzelne Variable bestimmt

empVar <- function(x) {
  n <- length(x)
  s2 <- sum((x - mean(x))^2)/n
  return(s2)                     #definieren, damit Ergebnis ausgegeben werden kann
}

empVar(mdbf[, 1])                #Funktion auf beliebig viele Objekte anwenden
empVar(mdbf[, 2])

empVar <- function(x) {
  n <- length(x)
  s2 <- sum((x - mean(x))^2)/n
  out <- list(s2 = s2, n = n)    #wenn mehrere Ergebnisse ausgegeben werden sollen, müssen diese vorher innerhalb der Funktion zu einem Objekt (meistens einer Liste) zusammengefasst werden
  return(out)
}
empVar(mdbf[, 2])

#gemeinsame Funktion für beide Formeln der Varianz
Vari <- function(x, empirical) {      #empirical gibt an, um welche Varianzfunktion es sich handelt
  n <- length(x)
  if (empirical) {
    s2 <- sum((x - mean(x))^2)/n
  } else {
    s2 <- sum((x - mean(x))^2)/(n-1)
  }
  return(s2)
}

Vari(mdbf[, 2], TRUE)  #Varianz, für wenn empirical = TRUE
Vari(mdbf[, 2], FALSE) #Varianz, für wenn empirical = FALSE

Vari <- function(x, empirical = TRUE) {  #empirical = TRUE als default festlegen
  n <- length(x)
  if (empirical) {
    s2 <- sum((x - mean(x))^2)/n
  } else {
    s2 <- sum((x - mean(x))^2)/(n-1)
  }
  return(s2)
}

Vari(mdbf[, 2])



##Anwendung: Simulationsstudien & Poweranalysen

#Code aus letztem Semester
N <- 20
set.seed(1234)
replicate(n = 10, expr = {X <- rnorm(N)
Y <- rnorm(N)
ttestH0 <- t.test(X, Y, var.equal = TRUE)
ttestH0$p.value})

#Funktion als for-Schleife
mySim <- function(N)
{
  X <- rnorm(N)
  Y <- rnorm(N)
  ttestH0 <- t.test(X, Y, var.equal = TRUE)
  return(ttestH0$p.value)
}
set.seed(1234)
replicate(n = 10, expr = mySim(N = 20))

#empirischen t-Wert mit aufnehmen
mySim2 <- function(N)
{
  X <- rnorm(N)
  Y <- rnorm(N)
  ttestH0 <- t.test(X, Y, var.equal = TRUE)
  return(c("p" = ttestH0$p.value, "t" = ttestH0$statistic))
}
set.seed(1234)
replicate(n = 10, expr = mySim2(N = 20))


#Simulationsstudie

S <- matrix(c(1, .7, .7, 2), 2, 2) # Populationskovarianzmatrix
S
# install.packages("mvtnorm")
library(mvtnorm)
set.seed(1234)
X <- rmvnorm(n = 10^3, mean = c(2, 3), sigma = S)
colMeans(X)
cov(X)

#Residuum
eps <- rnorm(10^3, sd = 1.3)
X1 <- X[,1]
X2 <- X[,2]
Y <- 0.3 + 0.5*X1 + 0.3*X2 + eps
df <- data.frame("X1" = X1, "X2" = X2, "Y" = Y)

#Regressionsanalyse
reg <- lm(Y ~ 1 + X1 + X2, data = df)
coef(reg) # Koeffizienten abgreifen

#als Funktion
myRegSim <- function(N)
{
  S <- matrix(c(1, .7, .7, 2), 2, 2) # Populationskovarianzmatrix
  X <- rmvnorm(n = N, mean = c(2, 3), sigma = S)
  eps <- rnorm(N, sd = 1.3)
  X1 <- X[,1]
  X2 <- X[,2]
  Y <- 0.3 + 0.5*X1 + 0.3*X2 + eps
  df <- data.frame("X1" = X1, "X2" = X2, "Y" = Y)
  reg <- lm(Y ~ 1 + X1 + X2, data = df)
  coef(reg) # Koeffizienten abgreifen
  return(coef(reg))
}
set.seed(1234)
replicate(n = 10, expr = myRegSim(N = 10^3))

#Abspeichern, Transponieren & Schätzen
set.seed(1234)
mySimErg <- t(replicate(n = 10, expr = myRegSim(N = 10^3)))
colMeans(mySimErg)
