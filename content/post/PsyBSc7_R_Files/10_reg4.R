# ---- Regression IV: quadratische und moderierte Regression ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBsc7_R_Files/10_reg4.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Julien P. Irmer & Johannes Hartig. Skriptkompilierung von Kevin Pommeranz.



#Auskommentieren um Datensatz zu laden
load(url("https://pandar.netlify.app/post/PISA2009.rda"))

#Packages der Session
library(car)
library(MASS)
library(lm.beta) # erforderlich für standardiserte Gewichte
library(ggplot2)
library(interactions) # für Interaktionsplots in moderierten Regressionen

#### Quadratische Verläufe an Beispiel ----
# Berechnung des Modells und Ausgabe der Ergebnisse
m1 <- lm(Reading ~ HISEI + MotherEdu + Books, data = PISA2009)
summary(lm.beta(m1))

# Residuenplots
residualPlots(m1, pch = 16)

res <- studres(m1) # Studentisierte Residuen als Objekt speichern
df_res <- data.frame(res) # als Data.Frame für ggplot
# Grafisch: Histogramm mit Normalverteilungskurve
library(ggplot2)
ggplot(data = df_res, aes(x = res)) + 
     geom_histogram(aes(y =..density..),
                    bins = 15,                    # Wie viele Balken sollen gezeichnet werden?
                    colour = "blue",              # Welche Farbe sollen die Linien der Balken haben?
                    fill = "skyblue") +           # Wie sollen die Balken gefüllt sein?
     stat_function(fun = dnorm, args = list(mean = mean(res), sd = sd(res)), col = "darkblue") + # Füge die Normalverteilungsdiche "dnorm" hinzu und nutze den empirischen Mittelwert und die empirische Standardabweichung "args = list(mean = mean(res), sd = sd(res))", wähle dunkelblau als Linienfarbe
     labs(title = "Histogramm der Residuen mit Normalverteilungsdichte", x = "Residuen") # Füge eigenen Titel und Achsenbeschriftung hinzu

# Test auf Abweichung von der Normalverteilung mit dem Shpiro Test
shapiro.test(res)

#### Aufnahme eines quadratischen Effekts ----
m1.b <- lm(Reading ~ HISEI + poly(MotherEdu, 2) + Books, data = PISA2009)
summary(lm.beta(m1.b))

# Korrelation von linearem und quadratischem Effekt
cor(PISA2009$MotherEdu, PISA2009$MotherEdu^2)

# Wiederholt mit poly()
cor(poly(PISA2009$MotherEdu, 2))

# Vergleich mit Modell ohne quadratischen Trend
summary(m1.b)$r.squared - summary(m1)$r.squared # Inkrement

# Signifikanzprüfung mit anova()
anova(m1, m1.b)

residualPlots(m1.b, pch = 16)



linear <- .1588
quadratisch <- -.1436

curve(linear * x + quadratisch * x^2, 
      xlim = c(-2, 2))

#### Interaktionsterme ----

load(url("https://pandar.netlify.app/post/Schulleistungen.rda"))
head(Schulleistungen)



Schulleistungen_std <- data.frame(scale(Schulleistungen)) # standardisierten Datensatz abspeichern als data.frame
colMeans(Schulleistungen_std)     # Mittelwert pro Spalte ausgeben
apply(Schulleistungen_std, 2, sd) # Standardabweichungen pro Spalte ausgeben

# moderierte Regression
mod_reg <- lm(reading ~ math + IQ + math:IQ, data = Schulleistungen_std)
summary(mod_reg)

# Grafische Interaktionsdarstellung
library(interactions)
interact_plot(model = mod_reg, pred = IQ, modx = math)
