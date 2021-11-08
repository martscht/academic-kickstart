#######################
### Logistische Regression
# von Kai Nehler


## Daten laden
install.packages("haven")
library(haven)
osf <- read_sav(file = url("https://osf.io/prc92/download"))

### Übersicht über die Daten
names(osf)  # Variablennamen im Datensatz
dim(osf)    # Dimensionen des Datensatzes

### Personen mit fehlenden Werten entfernen
missings_id <- which(is.na(osf$ANYDUMMY) | # Fälle identifizieren
                       is.na(osf$GENDER_R) |
                       is.na(osf$Depression_lvl))
length(missings_id)

osf <- osf[-missings_id, ] # Fälle ausschließen
dim(osf) # nach Fallauschluss

### Geschlecht in Faktor umwandeln
osf$GENDER_R <- as.factor(osf$GENDER_R)
levels(osf$GENDER_R) <- c("weiblich", "maennlich")



## Lineare Regression
reg_model <- lm(ANYDUMMY ~ 1 + Depression_lvl, data = osf)
summary(reg_model)



## Generalisiertes Lineares Modell: Logistische Regressionsanalyse

### Fragestellung 1: Depression als Prädiktor
glm_model <- glm(ANYDUMMY ~ 1 + Depression_lvl, family = "binomial", data = osf)
summary(glm_model)

#### Modellvergleich
install.packages("lmtest") # Paket lmtest installieren
library(lmtest)
lrtest(glm_model)

#### Plots
Depressionswerte <- seq(-20, 60, 0.1)
logit <- glm_model$coefficients[1] + glm_model$coefficients[2]*Depressionswerte
plot(x = Depressionswerte, y = logit, type = "l", col = "blue", lwd = 3)

odds <- exp(logit) # Odds
plot(x = Depressionswerte, y = odds, type = "l", col = "blue", lwd = 3)

p <- odds/(1 + odds) # Wahrscheinlichkeit
plot(x = Depressionswerte, y = p, type = "l", col = "blue", lwd = 3)


### Fragestellung 2: Geschlecht als Prädiktor

table(osf$GENDER_R, osf$ANYDUMMY) # Überblick über Kombinationen an Drogenabhängigkeit und Geschlecht

glm_model2 <-  glm(ANYDUMMY ~ 1 + Depression_lvl + GENDER_R, family = "binomial", data = osf) # Geschlecht in das Modell aufnehmen
summary(glm_model2)

exp(glm_model2$coefficients) # Odds-Ratios



### Modellvergleich
lrtest(glm_model2) # neu erstelltes Gesamtmodell mit Null-Modell vergleichen

