# ---- Partial- & Semipartialkorrelation ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBSc7_R_Files/03_partial.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Marvin Schröder, Luisa Grützmacher & Kai J. Nehler. Skriptkompilierung von Kevin Pommeranz.

#Daten abrufen
load(url("https://pandar.netlify.app/post/Schulleistungen.rda"))
head(Schulleistungen)

#Pakete laden
library(ggplot2)       # für Graphiken

# grafische Darstellung mittels Scatterplot
ggplot(Schulleistungen, aes(x=reading, y=math)) + 
  geom_point() + 
  labs(x= "Leseleistung", y= "Mathematikleistung")

# Korrelationstest
cor.test(Schulleistungen$reading, Schulleistungen$math)


# Regression Leseleistung durch allgemeine Intelligenz
reg.reading.IQ <- lm(reading ~ IQ, data = Schulleistungen)
summary(reg.reading.IQ)

# Residuen in Objekt ablegen (Residuen x)
res.reading.IQ <- reg.reading.IQ$residuals

# Regression Mathematikleistung durch allgemeine Intelligenz
reg.math.IQ <- lm(math ~ IQ, data = Schulleistungen)
summary(reg.math.IQ)

# Residuen in Objekt ablegen (Residuen y)
res.math.IQ <- reg.math.IQ$residuals

# Partialkorrelation durch Residuen
cor.test(res.reading.IQ, res.math.IQ)


## # Paket für Partial- und Semipartialkorrelation
## install.packages("ppcor")

library(ppcor)

# Partialkorrelation mit Funktion

pcor.test(x=Schulleistungen$reading, y=Schulleistungen$math, z=Schulleistungen$IQ)

# Semipartialkorrelation durch Nutzung des Residuums
cor.test(Schulleistungen$reading, res.math.IQ)


# Semipartialkorrelation mit Funktion
spcor.test(x=Schulleistungen$reading, y=Schulleistungen$math, z=Schulleistungen$IQ)

