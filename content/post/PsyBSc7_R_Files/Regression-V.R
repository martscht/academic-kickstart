#######################
###  Regression V: nichtlineare Regression
# von Julien P. Irmer

# Pakete laden ----
library(ggplot2) # Grafiken
library(car)     # Residuenplots
library(MASS)    # studres


## Einführung: Exponentielles Wachstum ----
# gleiches Ergebnis:
10^3
exp(3*log(10))

# gleiches Ergebnis:
log(10^3, base = 10) # Logarithmus von 1000 zur Basis 10
log(10^3)/log(10) # mit ln

# gleiches Ergebnis:
log(9, base = 3) # Logarithmus von 9 zur Basis 3
log(9)/log(3) # mit ln


### Das Modellieren von exponentiellem Wachstum ----
##################
#### Einstellen der Koeffizienten und berechnen von f(x)
####
x <- seq(-1,2,0.1) # x = Variablen (als Zahlen zwischen -1 und 2)
a <- 2   # Vorfaktor, der die Ausprägung an der Stelle x=0 beschreibt
b <- 3   #  Basis des exponentiellen Wachstums
c <- 1.5 # *eigentlich redundanter* Ratenparameter
f <- a*b^(c*x) # f(x), eine exponentiell-wachsende Funktion in x

##################
#### Plot von f(x) vs. x
####
plot(x = x, y = f, type = "l", col = "blue", lwd = 2, main = "Plot von f(x) vs. x") # plotte f(x) gegen x
abline(v = 0, lwd = 0.7) # y-Achse, v = 0 zeichnet eine vertikale Linie bei x = 0
abline(h = a, lty = 3) # im Punkt a schneidet f (das exponentielle Wachstum) die y-Achse (x=0), h = a zeichnet zu y = a eine horizontale Linie

##################
#### Plot von ln(f(x)) vs. x
####
plot(x = x, y = log(f), type = "l", col = "blue", lwd = 2, main = "Plot von ln(f(x)) vs. x") # plotte ln(f(x)) gegen x
abline(v = 0, lwd = 0.7) # y-Achse, v = 0 zeichnet eine vertikale Linie bei x = 0
abline(h = log(a), lty = 3)  # im Punkt log(a) schneidet log(f) (das linearisierte exponentielle Wachstum) die y-Achse (x=0), h =llog(a) zeichnet zu y = log(a) eine horizontale Linie

## Modellierung von exponentiellem Wachstum
### Das Modellieren von exponentiellem Wachstum am Beispiel der Weltbevölkerung von 1800 bis 2020


#### Daten laden ----
load(url("https://pandar.netlify.app/post/WorldPopulation.rda"))

head(WorldPopulation)


ggplot(data = WorldPopulation, aes(x = Year, y = Population))+geom_point()


### Lineares Modell für das Bevölkerungswachstum ----
ggplot(data = WorldPopulation, aes(x = Year, y = Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x")         # plotte linearen Verlauf
m_l <- lm(Population ~ Year, data = WorldPopulation) # linearer Verlauf
summary(m_l)

#########
### Normalverteilung der Residuen?
##
res <- studres(m_l) # Studentisierte Residuen als Objekt speichern
df_res <- data.frame(res) # als Data.Frame für ggplot
# Grafisch: Histogramm mit Normalverteilungskurve
ggplot(data = df_res, aes(x = res)) +
  geom_histogram(aes(y =..density..),
                 bins = 10,                    # Wie viele Balken sollen gezeichnet werden?
                 colour = "blue",              # Welche Farbe sollen die Linien der Balken haben?
                 fill = "skyblue") +           # Wie sollen die Balken gefüllt sein?
  stat_function(fun = dnorm, args = list(mean = mean(res), sd = sd(res)), col = "darkblue") + # Füge die Normalverteilungsdiche "dnorm" hinzu und nutze den empirischen Mittelwert und die empirische Standardabweichung "args = list(mean = mean(res), sd = sd(res))", wähle dunkelblau als Linienfarbe
  labs(title = "Histogramm der Residuen mit Normalverteilungsdichte\n für das lineare Modell", x = "Residuen") # Füge eigenen Titel und Achsenbeschriftung hinzu


### Exponentielles Modell für das Bevölkerungswachstum ----
WorldPopulation$log_Population <- log(WorldPopulation$Population) # Logarithmus der Weltbevölkerung

m_log <- lm(log_Population ~ Year, data = WorldPopulation) # lineares Modell mit log(y) als AV (logarithmische Skala)
summary(m_log)

m_log2 <- lm(I(log(Population)) ~ Year, data = WorldPopulation) # lineares Modell mit log(y) als AV (logarithmische Skala)
summary(m_log2)

ggplot(data = WorldPopulation, aes(x = Year, y = log_Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x", col = "red")+
  labs(title = "Logarithmierte Weltbevölkerung vs. Jahr")

WorldPopulation$pred_Pop_exp <- exp(predict(m_log)) # Abspeichern der retransformierten vorhergesagten Werten (wieder auf der Skala der Weltbevölkerung)
head(WorldPopulation)

ggplot(data = WorldPopulation, aes(x = Year, y = Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x")+         # plotte linearen Verlauf
  geom_line(aes(x = Year, y = pred_Pop_exp), col = "red", lwd = 1.5)

residualPlot(m_log, col = "red") # Residualplot

ggplot(data = WorldPopulation, aes(x = Year, y = log_Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x", col = "red")+
  geom_smooth(method="lm", formula = "y~poly(x,2)", col = "gold3")+
  labs(title = "Logarithmierte Weltbevölkerung vs. Jahr")

### Quadratisch-exponentielles Modell für das Bevölkerungswachstum ----
m_log_quad <- lm(log_Population ~ poly(Year, 2), data = WorldPopulation) # lineares Modell mit log(y) als AV (logarithmische Skala)
summary(m_log_quad)
anova(m_log, m_log_quad)


WorldPopulation$pred_Pop_exp_quad <- exp(predict(m_log_quad)) # Abspeichern der retransformierten vorhergesagten Werten (wieder auf der Skala der Weltbevölkerung)
head(WorldPopulation)

ggplot(data = WorldPopulation, aes(x = Year, y = Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x")+         # plotte linearen Verlauf
  geom_line(aes(x = Year, y = pred_Pop_exp), col = "red", lwd = 1.5)+
  geom_line(aes(x = Year, y = pred_Pop_exp_quad), col = "gold3", lwd = 2)

# nur quadratisch-exponentiell
ggplot(data = WorldPopulation, aes(x = Year, y = Population))+
  geom_point()+
  geom_line(aes(x = Year, y = pred_Pop_exp_quad), col = "gold3", lwd = 2)+
  labs(title = "Beobachtetes und durch das quadratisch-exponentielle Modell\n vorhergesagtes Bevölkerungswachstum")

# Residuenplot
residualPlot(m_log_quad)



## Appendix A {#AppendixA} ----
### Quadratisches Modell für das Bevölkerungswachstum
m_q <- lm(Population ~ poly(Year,2), data = WorldPopulation) # quadratischer Verlauf
summary(m_q)
summary(m_q)$r.squared - summary(m_l)$r.squared  # Inkrement

anova(m_l, m_q)

ggplot(data = WorldPopulation, aes(x = Year, y = Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x")+         # plotte linearen Verlauf
  geom_smooth(method="lm", formula = "y~poly(x,2)", col = "darkblue")  # plotte quadratischen Verlauf

ggplot(data = WorldPopulation, aes(x = Year, y = Population))+
  geom_point()+geom_smooth(method="lm", formula = "y~x")+         # plotte linearen Verlauf
  geom_smooth(method="lm", formula = "y~poly(x,2)", col = "darkblue")+  # plotte quadratischen Verlauf
  geom_line(aes(x = Year, y = pred_Pop_exp), col = "red", lwd = 1.5)


## Appendix B {#AppendixB} ----
### Übersicht über erklärte Varianzanteile
R2 <- rbind(summary(m_l)$r.squared,
            summary(m_q)$r.squared,
            summary(m_log)$r.squared,
            summary(m_log_quad)$r.squared)
rownames(R2) <- c("linear", "quadratisch", "exponentiell (log. Modell)", "quadratisch-exponentiell (quadratisches log. Modell)")
colnames(R2) <- "R^2"
round(R2, 4)
