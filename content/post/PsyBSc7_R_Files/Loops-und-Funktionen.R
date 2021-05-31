#######################
### Loops und Funktionen
# von Johanna Schüller und Sebastian Wallot


###Logische Abfragen und Bedingungen "if" und "else"


##if-Abfragen

#Einfache if-Abfrage
a = 3 #Zunächst definieren wir eine Variable
if (a == 3) { #Bedingung, die entweder TRUE oder FALSE sein kann
  print("Ja, die Variable a enthält den Wert 3") #Konsequenz, falls Bedingung wahr ist
}

a = 5
if (a == 3) {
  print("Ja, die Variable a enthält den Wert 3")
}

#if-Abfrage mit mehreren Möglichkeiten
person = "Monica"
if (person %in%  c("Monica", "Rachel", "Chandler",  "Phoebe", "Ross", "Joey")) {
  print("Yes, this is a character from Friends.")
}
#"%in%" = ist Element der folgenden Auswahl

#Abgleich mit einem Datum
if (weekdays(Sys.Date()) == 'Thursday') {
  'R Kurs um 8!'
}

#Verknüpfung logischer Abfragen
if (weekdays(Sys.Date()) == 'Saturday' | weekdays(Sys.Date()) == 'Sunday') { #| steht für "oder"
  print("Hoch die Hände, Wochenende!")
}


##Abgleich mit mehreren Alternativen: if-else-Abfragen
if (weekdays(Sys.Date()) == 'Thursday') {
  'R Kurs um 8!'
} else { #Konsequenz, die ausgeführt wird, wenn Bedingung = FALSE ist
  'Ausschlafen'
}


#else-if-Bedingungen
if (weekdays(Sys.Date()) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) {
  if (weekdays(Sys.time()) == 'Monday') {
    'Go back to bed...'
  } else if (weekdays(Sys.time()) == 'Wednesday') {
    'Wuhu, it`s Hump-Day!'
  } else if (weekdays(Sys.time()) == 'Friday') {
    'Yeah, it`s TGIF-Day!'
  } else {
    'It`s some other day!'
  }
} else {
  "It`s the weekend!"
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
ifelse(test = weekdays(Sys.Date()) == 'Friday',  #die Bedingung
       yes = 'Yeah, it`s TGIF-Day!',             #was getan werden soll, wenn die Bedingung zutrifft
       no = 'It some other boring day...')       #was getan werden soll, wenn die Bedingung nicht zutrifft



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
neg <- c(3, 4, 5, 7, 9, 11) #Vektor der negativen Items erstellen
for (i in neg) {                        #i wird als Vektor der negativen Items definiert
  mdbf_r[, i] <- -1 * (mdbf_r[, i] - 5) #jedes Item aus dem Datensatz mit der Spalte i wird umgepolt
}
cor(mdbf[, 3], mdbf_r[, 3]) #erfolgreiches Umpolen überprüfen

#ineinandergeschachtelte Loops mit zwei Iterationen
buchst <- c("A", "B", "C")
zahl <- c(1,2)
for (i in buchst) {
  for (ii in zahl) {
    print(i)
    print(ii)
  }
}


##while-Loops
#der Code wird so lange ausgeführt, wie eine vorab definierte Bedingung erfüllt ist
coin <- c('Kopf', 'Zahl') #Münze erstellen
toss <- NULL              #leeres Objekt für die Aufzeichnung erstellen

while (sum(toss == 'Kopf')<10) {   #sind Kopf-Würfe < 10 ?
  toss <- c(toss, sample(coin, 1))
}
toss #Würfe ansehen


##repeat-Loops
#häufig genutzt, wenn es verschiedene oder veränderliche Abbruchkriterien für den Loop gibt
fibo <- c(1, 1) #Fibonacci-Sequenz bilden (eine Sequenz in der eine Zahl immer die Summe der vorherigen beiden Zahlen ist)

repeat {
  n <- length(fibo)
  fibo <- c(fibo, fibo[n] + fibo[n - 1])
  if (fibo[n+1] > 1000) break            #Sequenz abbrechen, wenn die letzte Zahl z.B. größer als 1000 ist
}
fibo



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
