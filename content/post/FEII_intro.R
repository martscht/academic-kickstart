#### R-Skript zur 1. Sitzung ####
  # R Chunks wurden aus dem HTML exportiert und sind dort alle enthalten


#### Vorbereitung ----
data(fairplayer, package = 'PsyMSc1')


#### R Grundlagen Wiederbelebung ----

# Namen der Variablen abfragen
names(fairplayer)

# Anzahl der Zeilen und Spalten
nrow(fairplayer)
ncol(fairplayer)

# Struktur des Datensatz - Informationen zur Variablentypen
str(fairplayer)

# Datensatz ansehen
fairplayer

# Skalenwerte erstellen
fairplayer$rat1 <- rowMeans(fairplayer[, c('ra1t1', 'ra2t1', 'ra3t1')],
  na.rm = TRUE)
fairplayer$emt1 <- rowMeans(fairplayer[, c('em1t1', 'em2t1', 'em3t1')],
  na.rm = TRUE)
fairplayer$sit1 <- rowMeans(fairplayer[, c('si1t1', 'si2t1', 'si3t1')],
  na.rm = TRUE)

# Deskriptivstatistik intervallskalierte Variable
summary(fairplayer$rat1)

# Deskriptivstatistik nominalskalierte Variable
summary(fairplayer$grp)

# Standardabweichung bestimmen
sd(fairplayer$rat1, na.rm = TRUE)

# Kovarianzen und Korrelationen
cov(fairplayer$rat1, fairplayer$sit1, use = 'complete')
cor(fairplayer$rat1, fairplayer$sit1, use = 'complete')

# Teildatensatz aus Skalenwerten
scales <- fairplayer[, c('rat1', 'emt1', 'sit1')]

# Korrelationsmatrix
cor(scales, use = 'complete')

# Kovarianzmatrix
cov(scales, use = 'complete')

# Varianzen
diag(cov(scales, use = 'complete'))


#### Wiederholung Regression ----

# Regressionsmodell
mod <- lm(rat1 ~ 1 + sit1 + emt1, fairplayer)

# Regressionsgewichte abrufen
mod

# Scatterplot mit Regressionsgerade
plot(fairplayer$rat1 ~ fairplayer$sit1)
abline(coef(mod)[1], coef(mod)[2])

# Übersicht über Modellergebnisse
summary(mod)

# Koeffiziententabelle extrahieren
summary(mod)$coef

# R-Quadrat extrahieren
summary(mod)$r.squared


### lavaan ----

# Paket laden
  # ggf. mit install.packages('lavaan') installieren
library(lavaan)

# Regression
mod <- 'rat1 ~ 1 + sit1 + emt1'

# Regression mit Residualvarianz
mod <- 'rat1 ~ 1 + sit1 + emt1
  rat1 ~~ rat1'

# Langschreibweise des Regressionsmodells
mod <- '
  # Regression
  rat1 ~ 1
  rat1 ~ sit1
  rat1 ~ emt1
  
  # Residuum
  rat1 ~~ rat1'

# Modellschätzung
fit <- lavaan(mod, fairplayer)

# Ergebnisübersicht
summary(fit)

# R-Quadrat extrahieren
inspect(fit, 'rsquare')

# Anzahl der Beobachtungen
inspect(fit, 'nobs')

# Anzahl der Parameter
inspect(fit, 'npar')