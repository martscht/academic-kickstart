#######################
### Multiple Regression und Ausreisserdiagnostik
# von Kai Nehler


## Vorbereitungen

### Pakete installieren & laden
install.packages("car")            # Die Installation ist nur einmalig von Nöten!
install.packages("lm.beta")        # Sie müssen nur zu Update-Zwecken erneut installiert werden.
install.packages("MASS")

library(lm.beta)  # Standardisierte beta-Koeffizienten für die Regression
library(car)      # Zusätzliche Funktion für Diagnostik von Datensätzen
library(MASS)     # Zusätzliche Funktion für Diagnostik von Datensätzen

### Daten einladen
load(url("https://pandar.netlify.app/post/Depression.rda"))

### Überblick über die Daten
head(Depression)

levels(Depression$Geschlecht) <- c("maennlich", "weiblich") # Levels anpassen
Depression[5, 6] <- "maennlich"



## (Multiple-) Lineare Regression

### Regressionsmodell
lm(Depressivitaet ~ 1 + Geschlecht + Lebenszufriedenheit, data = Depression)

model <- lm(Depressivitaet ~ 1 + Geschlecht + Lebenszufriedenheit, data = Depression) # Modell als Objekt abspeichern
summary(model)

lm.beta(model) |> summary() # standardisierte Ergebnisse
model |> lm.beta() |> summary() # noch genauer

mean(x = resid(model)) # Mittelwert mit Referenzierung aus dem lm Objekt "model"

summary_model <- summary(lm.beta(model))
summary_model$coefficients # Koeffiziententabelle
names(summary_model)      # weitere mögliche Argumente, die wir erhalten können
summary_model$r.squared  # R^2


### Voraussetzungsprüfung

#### Multikollinearität
cor(as.numeric(Depression$Geschlecht), Depression$Lebenszufriedenheit) # Korrelation der Prädiktoren

vif(model)        # VIF
1/vif(model)      # Toleranz

1/(1-cor(as.numeric(Depression$Geschlecht), Depression$Lebenszufriedenheit)^2) # 1/(1-R^2) = VIF
1-cor(as.numeric(Depression$Geschlecht), Depression$Lebenszufriedenheit)^2 # 1-R^2 = Toleranz

#### Identifikation von Ausreissern und einflussreichen Datenpunkten
##### Hebelwerte
n <- length(residuals(model))   # Anzahl an Personen bestimmen
h <- hatvalues(model)           # Hebelwerte
hist(h, breaks  = 20)
abline(v = 2*(2+1)/n, col = "red")  # Cut-off als große Stichprobe
abline(v = 3*(2+1)/n, col = "blue")  # Cut-off als kleine Stichprobe

##### Cooks Distanz
CD <- cooks.distance(model) # Cooks Distanz
hist(CD, breaks  = 20)
abline(v = 1, col = "red")  # Cut-off bei 1

hist(CD, breaks  = 20, xlim = c(0, 1)) # nochmal Cooks Distanz
abline(v = 1, col = "red")  # Cut-off bei 1

##### Blasendiagramm
# Blasendiagramm mit Hebelwerten, studentisierten Residuen und Cooks Distanz
# In "IDs" werden die Zeilennummern der auffälligen Fälle gespeichert,
# welche gleichzeitig als Zahlen im Blasendiagramm ausgegeben werden
InfPlot <- influencePlot(model)
IDs <- as.numeric(row.names(InfPlot))
# Werte der identifizierten Fälle
InfPlot

# Rohdaten der auffälligen Fälle
Depression[IDs,]
# z-standardisierte Werte der auffälligen Fälle
scale(Depression[,1:4])[IDs,]

