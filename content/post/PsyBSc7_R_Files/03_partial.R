# ---- Partial- & Semipartialkorrelation ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBsc7_R_Files/03_partial.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Marvin Schröder & Luisa Grützmacher. Skriptkompilierung von Kevin Pommeranz.

#Pakete laden
library(MASS)
library(ppcor)
library(ggplot2)

#Daten abrufen
load(url("https://pandar.netlify.app/post/Schulleistungen.rda"))



# Übersicht Datensatz und Korrelationen aller Variablen im Datensatz
str(Schulleistungen)
cor(Schulleistungen)


# 1a) grafische Darstellung mittels Scatterplot
ggplot(Schulleistungen, aes(x=reading, y=math)) + geom_point() + labs(x= "Leseleistung", y= "Mathematikleistung")

# 1b) Test der Korrelation zwischen Lese- und Mathematikleistung (r~xy~)
cor.test(Schulleistungen$reading, Schulleistungen$math)


# 2a) Regression zur Vorhersage der Leseleistung durch die allgemeine Intelligenz
reg.reading.IQ <- lm(reading ~ IQ, data = Schulleistungen)
summary(reg.reading.IQ)

# Residuen speichern (Residuen x)
res.reading.IQ <- reg.reading.IQ$residuals

# 2b) Regression zur Vorhersage der Mathematikleistung durch die allgemeine Intelligenz
reg.math.IQ <- lm(math ~ IQ, data = Schulleistungen)
summary(reg.math.IQ)

# Residuen speichern (Residuen y)
res.math.IQ <- reg.math.IQ$residuals

# 3a) Test der Korrelation zwischen den Residuen (r~xy.z~)
cor.test(res.reading.IQ, res.math.IQ)


# 3b) Partialkorrelation, d.h. der Zusammenhang von Lese- und Mathematikleistung
# unter Kontrolle der allgemeinen Intelligenz (r~xy.z~)

   #Paket für die Partial- und Semipartialkorrelation laden
   #library(ppcor)

pcor.test(x=Schulleistungen$reading, y=Schulleistungen$math, z=Schulleistungen$IQ)

# Semipartialkorrelation von Lesen- und Mathematikleistung unter Kontrolle der
# allgemeinen Intelligenz auf die Mathematikleistung (Korrelation von x ('reading')
# mit dem Residuum y ('res.math.IQ'))
cor.test(Schulleistungen$reading, res.math.IQ)


# b Semipartialkorrelation von Lese- und Mathematikleistung unter Kontrolle der
# allgemeinen Intelligenz auf die Mathematikleistung
spcor.test(x=Schulleistungen$reading, y=Schulleistungen$math, z=Schulleistungen$IQ)

