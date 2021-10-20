#######################
### Einleitung und Wiederholung KliPPs
# von Kai Nehler


#### Wiederholung in R ----

1 + 2   # Addition
3 == 4   # Logische Abfrage auf Gleichheit


## Funktionen und Argumente
sum(1, 2) # Addition durch Funktion
log(x = 23, base = 10) # Benennung von Argumenten


## Hilfe
?log


## Objekte und das Environment
my_num <- sum(3, 4, 1, 2) # Objekt zuweisen
my_num # Objekt anzeigen
sqrt(my_num) # Objekt in Funktion einbinden
sqrt(sum(3, 4, 1, 2)) # Verschachtelte Funktionen
sum(3, 4, 1, 2) |> sqrt() # Nutzung Pipe
my_vec <- c(1, 2, 3, 4) # Erstellung Vektor


## Daten einlesen und verarbeiten
load(url("https://pandar.netlify.app/post/Depression.rda"))

### Datenscreening
head(Depression) # ersten 6 Zeilen
names(Depression) # Namen der Variablen
dim(Depression) # Anzahl der Zeilen und Spalten
str(Depression) # Informationen zu Variablentypen

is.factor(Depression$Geschlecht) # überprüfen, ob das Objekt ein Faktor ist
levels(Depression$Geschlecht) # verschiedene Stufen
levels(Depression$Geschlecht) <- c("maennlich", "weiblich") # Faktorstufen Bedeutung zuordnen

### Datenextraktion
Depression$Depressivitaet[5] # Extrahieren
Depression$Depressivitaet[c(1, 3:5)] # Mehrfach Extrahieren
Depression[c(1:2), c(1:2)] # Extrahieren aus Matrix
Depression[1, ]   # 1. Zeile, alle Spalten

### Daten verändern
Depression[5, 6]                   # Aktuellen Inhalt abfragen
Depression[5, 6] <- "maennlich"    # Aktuellen Inhalt überschreiben
Depression[, 6]                    # Alle Geschlechter abfragen


## Einfache Analysen

### Einfache Deskriptivstatistiken
mean(Depression$Depressivitaet) # Mittwelert
var(Depression$Depressivitaet) # Varianz

summary(Depression$Depressivitaet) # Zusammenfassung numerisch
summary(Depression$Geschlecht) # Zusammenfassung factor

colMeans(Depression[1:4]) # Spaltenmittelwerte

### Zusammenhang und lineare Regression
plot(Depression$Lebenszufriedenheit, Depression$Depressivitaet, xlab = "Lebenszufriedenheit", ylab = "Depressivitaet")

lm(Depressivitaet ~ Lebenszufriedenheit, Depression) # lineare Regression
model <- lm(Depressivitaet ~ Lebenszufriedenheit, Depression) # Objektzuweisung
summary(model)
names(model) #andere Inhalte der Liste

### Der t-Test
t.test(Depressivitaet ~ Geschlecht,  # abhängige Variable ~ unabhängige Variable
       data = Depression, # Datensatz
       paired = FALSE, # Stichproben sind unabhängig
       alternative = "two.sided",        # zweiseitige Testung (Default)
       var.equal = TRUE,                 # Homoskedastizität liegt vor (-> Levene-Test)
       conf.level = .95)                 # alpha = .05 (Default)

ttest <- t.test(Depressivitaet ~ Geschlecht,  # abhängige Variable ~ unabhängige Variable
                data = Depression, # Datensatz
                paired = FALSE, # Stichproben sind unabhängig
                alternative = "two.sided",        # zweiseitige Testung (Default)
                var.equal = TRUE,                 # Homoskedastizität liegt vor (-> Levene-Test)
                conf.level = .95)                 # alpha = .05 (Default)
names(ttest)    # alle möglichen Argumente, die wir diesem Objekt entlocken können
ttest$statistic # (empirischer) t-Wert
ttest$p.value   # zugehöriger p-Wert
