# ---- ANOVA I: einfaktorielle ANOVA ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBSc7_R_Files/07_anova1.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Miriam Scheppa-Lahyani, Julien P. Irmer, Sebastian Wallot & Kai J. Nehler. Skriptkompilierung von Kevin Pommeranz.

## # Beispiel zum exemplarischen lokalen Datensatzladen über Musterverzeichnis
## load("C:/Users/Musterfrau/Desktop/conspiracy.rda")

# Datensatz der Sitzung über URL laden
load(url("https://pandar.netlify.app/post/conspiracy.rda"))

#### Datensatz - Betrachtung ----

dim(conspiracy)

# Darstellung der 9 Variablen und ihren ersten 6 Einträgen
head(conspiracy)

## Demographischer Hintergrund
# edu = Bildungsabschluss
# urban = Wohnort
# gender = Geschlecht
# age = Alter

## Subskalen verschwörungstheoretischer Überzeugungen
# GM = government malfeasance
# MG = malevolent global conspiracies
# ET = extraterrestrial cover-up
# PW = personal well-being
# CI = control of information

# Unabhängigkeit der Residuen als gegeben betrachtet
# Prüfung der Homoskedastizität

library(car)
leveneTest(conspiracy$ET ~ conspiracy$urban)

#### ANOVA per Hand ----
# Gruppenmittelwerte ermitteln
mu_k <- aggregate(conspiracy$ET, list(conspiracy$urban), mean)
mu_k

# Variablennamen überschreiben
names(mu_k) <- c('urban', 'ET_mu_k')

# Gruppenmittelwerte werden nach Gruppenzugehörigkeit hinzugefügt
temp <- merge(conspiracy, mu_k, by = 'urban')
dim(temp)
names(temp)

# Gesamtmittelwert ermitteln
mu <- mean(conspiracy$ET)

# Gruppengrößen ermitteln
n_k <- table(conspiracy$urban)

# QS_inn berechnen
QS_inn <- sum((temp$ET - temp$ET_mu_k)^2)

# QS_zw berechnen
QS_zw <- sum(n_k * (mu_k[, 2] - mu)^2)

# Mittlere Quadratsummen für F-Test berechnen
MQS_inn <- QS_inn / (nrow(conspiracy) - nlevels(conspiracy$urban))
MQS_zw <- QS_zw / (nlevels(conspiracy$urban)-1)

# F-Wert
F_wert <- MQS_zw/MQS_inn

# Wahrscheinlichkeit des F-Werts
pf(F_wert, nlevels(conspiracy$urban)-1, nrow(conspiracy) - nlevels(conspiracy$urban), lower.tail = FALSE)



## ### ezANOVA ----
## # Paket installieren
## install.packages("ez")

# Paket laden 
library(ez)

# Personen-ID für ezANOVA erstellen

conspiracy$id <- 1:nrow(conspiracy)

conspiracy$id <- as.factor(conspiracy$id)

# ezANOVA anwenden 
ezANOVA(conspiracy, wid = id, dv = ET, between = urban)

# Eine detailliertere Ausgabe
ezANOVA(conspiracy, wid = id, dv = ET, between = urban, detailed = TRUE)

#### t-Test ----

# Vergleich der Gruppen mit t-tests, Bonferroni-Korrektur wegen Alpha-Kumulierung
pairwise.t.test(conspiracy$ET, conspiracy$urban, p.adjust = 'bonferroni')

#### Tukey Test ----

# aov-Objekt für Tukey
alternative<- aov(ET ~ urban, data = conspiracy)

summary(alternative)

# Tukey anwenden
TukeyHSD(alternative, conf.level = 0.95)

tuk <- TukeyHSD(aov(ET ~ urban, data = conspiracy))
plot(tuk)

# aov-Objekt durch ezANOVA
aov_t <- ezANOVA(conspiracy, wid = id, dv = ET, between = urban, return_aov = T)
names(aov_t)

class(aov_t$aov)

TukeyHSD(aov_t$aov, conf.level = 0.95)
