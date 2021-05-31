#######################
### Partial- & Semipartialkorrelation
# von Martin Schroeder & Luisa Grützmacher


##Vorbereitung
library(MASS)
library(ppcor)
library(ggplot2)

load(url("https://pandar.netlify.app/post/Schulleistungen.rda")) #Datensatz von pandaR laden


###Partialkorrelation

#Übersicht über Datensatz verschaffen
str(Schulleistungen)


##1) Korrelation zwischen Lese- und Mathematikleistung

#1a) grafische Darstellung mittels Scatterplot
ggplot(Schulleistungen, aes(x=reading, y=math)) + geom_point() + labs(x= "Leseleistung", y= "Mathematikleistung")

#1b) Test der Korrelation zwischen Lese- und Mathematikleistung (r~xy~)
cor.test(Schulleistungen$reading, Schulleistungen$math)
cor(Schulleistungen)


##2) Regression zur Vorhersage von

#2a) der Leseleistung durch die allgemeine Intelligenz
reg.reading.IQ <- lm(reading ~ IQ, data = Schulleistungen)
summary(reg.reading.IQ)
res.reading.IQ <- reg.reading.IQ$residuals #Residuen speichern (Residuen x)

#2b) der Mathematikleistung durch die allgemeine Intelligenz
reg.math.IQ <- lm(math ~ IQ, data = Schulleistungen)
summary(reg.math.IQ)
res.math.IQ <- reg.math.IQ$residuals       #Residuen speichern (Residuen y)


##3) Partialkorrelation (Korrelation zwischen den Residuen)

#3a) Test der Korrelation zwischen den Residuen (r~xy.z~)
cor.test(res.reading.IQ, res.math.IQ)

#3b) direkt: Partialkorrelation, d.h. der Zusammenhang von Lese- und Mathematikleistung unter Kontrolle der allgemeinen Intelligenz (r~xy.z~)
library(ppcor)
pcor.test(x=Schulleistungen$reading, y=Schulleistungen$math, z=Schulleistungen$IQ)



###Semipartialkorrelation
#Einfluss der Drittvariablen wird nur aus einer der beiden Variablen herausgerechnet

##Semipartialkorrelation von Lesen- und Mathematikleistung unter Kontrolle der allgemeinen Intelligenz auf die Mathematikleistung (Korrelation von x ('reading') mit dem Residuum y ('res.math.IQ'))
cor.test(Schulleistungen$reading, res.math.IQ)

##alternativ: Semipartialkorrelation von Lese- und Mathematikleistung unter Kontrolle der allgemeinen Intelligenz auf die Mathematikleistung
spcor.test(x=Schulleistungen$reading, y=Schulleistungen$math, z=Schulleistungen$IQ)
