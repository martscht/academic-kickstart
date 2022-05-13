########################
### Regression II: Modelloptimierung
# von Johannes Hartig, Johanna Schüller & Julien Irmer


load(url("https://pandar.netlify.app/post/Schulleistungen.rda")) #Datensatz von pandaR laden

###Inkrement und Dekrement

##Testen eines Inkrements
#Vergleich eines eingeschränkten Modells mit weniger Prädiktoren gegen ein uneingeschränktes Modell mit zusätzlichen Prädiktoren
m.c <- lm(math ~ reading + female, data = Schulleistungen)      # constrained
m.u <- lm(math ~ reading + female + IQ, data = Schulleistungen) # unconstrained
summary(m.c)
summary(m.u)

#Inkrement = Differenz in R2 aus restringiertem Modell 2 minus R2 aus unrestringiertem Modell 1
summary(m.u)$r.squared - summary(m.c)$r.squared

#Modellvergleich mit der anova-Funktion
anova(m.c, m.u)

#Verglich: F-Test
R2.u <- summary(m.u)$r.squared
R2.c <- summary(m.c)$r.squared
df.diff <- summary(m.u)$df[1] - summary(m.c)$df[1] # Änderung in den df
df.u <- summary(m.u)$df[2] # Freiheitsgrade des uneingeschränkten Modells
F.diff <- ((R2.u - R2.c) / df.diff) /
  ((1 - R2.u) / df.u)
p.diff <- 1-pf(F.diff, df.diff, df.u)
F.diff # F-Wert der Differenz in R^2
p.diff # zugehöriger p-Wert

# Vergleich des Inkrements mit Semipartialkorrelationen
R2.u - R2.c
library(ppcor)
spcor.test(x = Schulleistungen$math, y = Schulleistungen$IQ, z = Schulleistungen[, c("reading", "female")])$estimate^2


##Testen eines Dekrements
#das eingeschränkte Modell mit weniger Prädiktoren wird mit dem uneingeschränkten Modell mit mehr Prädiktoren verglichen

m.u <- lm(math ~ reading + female + IQ, data = Schulleistungen) #uneingeschränktes Modell
m.c <- lm(math ~ reading + IQ, data = Schulleistungen)          #eingeschränktes Modell

summary(m.u)$r.squared - summary(m.c)$r.squared                 #Dekrement bestimmen
anova(m.c, m.u)                                                 #Modellvergleich mit der anova-Funktion



###Schrittweise Selektion von Prädiktoren

# Modell mit allen Prädiktoren
m <- lm(math ~ reading + female + IQ, data = Schulleistungen)
# Optimierung
summary(step(m))

# Optimierung mit BIC
summary(step(m, k=log(nrow(Schulleistungen))))


##Weitere Möglichkeiten
# install.packages("olsrr")
library(olsrr)
# pent = p enter, p-Wert zur Aufnahme ins Modell
# prem = p remove, p-Wert zum Ausschluss aus dem Modell
ols_step_both_p(m, pent = .05, prem = .10, details = TRUE)

##Unterschiede im AIC
model <- lm(math ~ reading + female, data = Schulleistungen)
AIC(model) # vollständiger AIC
extractAIC(model) # erstes Argument ist die Anzahl der Parameter (p)
