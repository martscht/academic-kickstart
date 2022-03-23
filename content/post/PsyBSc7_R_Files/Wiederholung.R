#######################
### Wiederholung von Grundlagen in R
# von Kai Nehler, Johanna Schüller und Martin Schultze


### R-Basics

## Funktionen

sum(1, 2) # Addition durch Funktion
args(round) # Argumente, die die Funktion round erwartet
round(1.2859) #Funktion mit default: "digits = 0"
round(1.2859, digits = 2) #default überschreiben, um 2 Nachkommastellen angezeigt zu bekommen
round(digits = 2, x = 1.2859) #Reihenfolge der Argumente kann vertauscht werden, wenn diese explizit benannt werden


## Objekte

my_num <- sum(3, 4, 1, 2) # Objekt zuweisen
sqrt(my_num) # Objekt in Funktion einbinden
sqrt(sum(3, 4, 1, 2)) # Verschachtelte Funktionen
sum(3, 4, 1, 2) |> sqrt() # Nutzung Pipe



### Vektoren und Matrizen

## Vektoren
zahlen <- c(8, 3, 4) # mehrere Zahlen zu einem Vektor kombinieren
zahlen * 3 # Rechenoperation auf Vektor anwenden
str(zahlen) # Klasse eines Vektors ermitteln

zeichen <- as.character(zahlen) #Elemente eines Vektors in Zeichen umwandeln
str(zeichen)


## Matrizen

mat<- matrix(c(7, 3, 9, 1, 4, 6), ncol = 2) #Matrix anlegen
str(mat)
mat[3, 1] #Element aus der dritten Zeile und ersten Spalte ausgeben lassen

nrow(mat) #Zeilen einer Matrix
ncol(mat) #Spalten einer Matrix
dim(mat) #alternativer Befehl



### Datensätze

## Einlesen von Datensätzen

load("C:/Users/Musterfrau/Desktop/mach.rda") # Datensatz aus lokalem Ordner laden
load(url("https://pandar.netlify.app/post/mach.rda")) # Datensatz aus dem Internet laden


## Überblick im Datensatz

head(mach) # ersten 6 Zeilen
names(mach) # Namen der Variablen
dim(mach) # Anzahl der Zeilen und Spalten


## Einfache Deskriptivstatistik

mean(mach$cvhn)    # Mittelwert
var(mach$cvhn)     # geschätzte Populationsvarianz
mean(mach[,1]) # Alle Zeilen, Spalte 25

table(mach$engnat) # Häufigkeitstabelle
str(mach$engnat)

mach$engnat <- factor(mach$engnat,                # Ausgangsvariable
                      levels = 1:2,               # Faktorstufen
                      labels = c("Ja", "Nein"))   # Bedeutung

str(mach$engnat)                                  # Test der Umwandlung


## Packages

install.packages("psych") #package vor der ersten Nutzung herunterladen
library(psych) #Paket nach jedem Neustart von R aus der library laden
describe(mach$cvhn)



### Zusammenhang und lineare Regression

plot(mach$pvhn, mach$cvhn, xlab = "Positive Sichtweise", ylab = "Negative Sichtweise")
lm(cvhn ~ pvhn, mach) # lineare Regression

model <- lm(cvhn ~ pvhn, mach)  # Objektzuweisung
summary(model)
names(model) #andere Inhalte der Liste


## t-Test

t.test(cvhn ~ engnat,  # abhängige Variable ~ unabhängige Variable
       data = mach, # Datensatz
       paired = FALSE, # Stichproben sind unabhängig
       alternative = "two.sided",        # zweiseitige Testung (Default)
       var.equal = TRUE,                 # Homoskedastizität liegt vor (-> Levene-Test)
       conf.level = .95)                 # alpha = .05 (Default)

ttest <- t.test(cvhn ~ engnat,  # abhängige Variable ~ unabhängige Variable
                data = mach, # Datensatz
                paired = FALSE, # Stichproben sind unabhängig
                alternative = "two.sided",        # zweiseitige Testung (Default)
                var.equal = TRUE,                 # Homoskedastizität liegt vor (-> Levene-Test)
                conf.level = .95)                 # alpha = .05 (Default)
names(ttest)    # alle möglichen Argumente, die wir diesem Objekt entlocken können
ttest$statistic # (empirischer) t-Wert
ttest$p.value   # zugehöriger p-Wert



