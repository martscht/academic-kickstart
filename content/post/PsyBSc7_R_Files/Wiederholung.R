#######################
### Wiederholung von Grundlagen in R
# von Johanna Schüller und Martin Schultze


###Objekte und Funktionen

####Objekte
zahl <- 100 #Objekt anlegen
zahl = 100
log(100) #Funktion auf Objekt anwenden
log(zahl)

####Funktionen
args(round) #Argumente, die eine Funktion erwartet
round(1.2859) #Funktion mit default: "digits = 0"
round(1.2859, digits = 2) #default überschreiben, um 2 Nachkommastellen angezeigt zu bekommen
round(digits = 2, x = 1.2859) #Reihenfolge der Argumente kann vertauscht werden, wenn diese explizit benannt werden


###Vektoren und Matrizen

####Vektoren
zahlen <- c(8, 3, 4) #mehrere Zahlen zu einem Vektor kombinieren
zahlen * 3 #Rechenoperation auf Vektor anwenden
str(zahlen) #Klasse eines Vektors ermitteln
class(zahlen) #alternativer Befehl

abfrage <- zahlen == 3 #elementenweise logische Abfrage
str(abfrage) #Hat der Vektor die erwartete Form?

zeichen <- as.character(zahlen) #Elemente eines Vektors in Zeichen umwandeln
str(zeichen)

gender <- c(0, 1, 0, 2, 1, 1, 0, 0, 2) #numerischen Vektor anlegen
str(gender)
gender_factor <- as.factor(gender) #Vektor in Faktor umwandeln -> Werte werden zu Platzhaltern für nominale Kategorien
str(gender_factor)

####Matrizen
mat<- matrix(c(7, 3, 9, 1, 4, 6), ncol = 2) #Matrix anlegen
str(mat)
mat[3, 1] #Element aus der dritten Zeile und ersten Spalte ausgeben lassen

nrow(mat) #Zeilen einer Matrix
ncol(mat) #Spalten einer Matrix
dim(mat) #alternativer Befehl

mat2 <-  matrix(c(8, 2, 11, 3, 5, 9), ncol = 2) #zweite Matrix anlegen

combined <- cbind(mat, mat2) #Matrizen spaltenweise zusammenfügen (analog: zeilenweise über rbind())
combined


###Packages

install.packages("psych") #package vor der ersten Nutzung herunterladen
library(psych) #Paket nach jedem Neustart von R aus der library laden


###Einlesen von Datensätzen

setwd("Ordnerpfad") #lokalen Ordner auf PC als Working Directory definieren
load("Dateiname.R") #R-Datei mit dem Namen "Dateiname" einlesen

load(url("https://pandar.netlify.app/post/mach.rda")) #Datensatz direkt von pandaR laden


###Mit Datensätzen arbeiten

names(mach) #Spaltenüberschriften
head(mach) #die ersten 6 Zeilen
summary(mach) #Zusammenfassung der Daten im Datensatz
describe(mach) #Deskriptivstatistiken

mean(mach$TIPI1) #Mittelwert der Variable TIPI1
mean(mach[,1]) #alternativ: alle Zeilen, erste Spalte

mach[mach$voted == 1, ] #Alle Beobachtungen, die im Item "voted" den Wert 1 haben
mach[mach$religion %in% c(1, 2), ] #Alle Beobachtungen, die im Item "religion" entweder 1 oder 2 aufweisen

mach[,1:10] #Auswahl der ersten 10 Variablen
