#######################
### ANOVA I: Einfaktorielle ANOVA
# von Miriam Scheppa-Lahyani, Julien P. Irmer, Sebastian Wallot & Kai Nehler


### Datensatz laden
load(url("https://pandar.netlify.app/post/conspiracy.rda")) #direkt von pandaR laden
head(conspiracy) #Überblick über Datensatz verschaffen


### Hypothesen
# H1: Mindestens zwei Gruppen (rural, suburban, urban) unterscheiden sich im Mittel hinsichtlich ihrer Zustimmung zur Verschwörungtheorie "Extraterrestrial Cover-Up".


### Voraussetzungsprüfung
#1) Unabhängigkeit der Residuen -> durch Studiendesign gewährleistet
#2) Homoskedastizität
library(car)
leveneTest(conspiracy$ET ~ conspiracy$urban) #Levene-Test mit Variable (ET) und Gruppierungsvariable (urban)
#3) Normalverteilung -> wird angenommen



###Einfaktorielle ANOVA "per Hand"

# Gruppenmittelwerte ermitteln
mu_k <- aggregate(conspiracy$ET, list(conspiracy$urban), mean)
names(mu_k) <- c('urban', 'ET_mu_k') # Variablennamen überschreiben
temp <- merge(conspiracy, mu_k, by = 'urban') # neuer Datensatz, der zusätzlich die Mittelwerte pro Gruppe enthält

# Gesamtmittelwert ermitteln
mu <- mean(conspiracy$ET)

# Gruppengrößen ermitteln
n_k <- table(conspiracy$urban)

#Quadratsummen berechnen
QS_inn <- sum((temp$ET - temp$ET_mu_k)^2)
QS_zw <- sum(n_k * (mu_k[, 2] - mu)^2)

#Mittlere Quadratsummen berechnen
MQS_inn <- QS_inn / (nrow(conspiracy) - nlevels(conspiracy$urban))
MQS_zw <- QS_zw / (nlevels(conspiracy$urban)-1)

#F-Wert bestimmen
F_wert <- MQS_zw/MQS_inn
pf(F_wert, nlevels(conspiracy$urban)-1, nrow(conspiracy) - nlevels(conspiracy$urban), lower.tail = FALSE) # p-Wert bestimmen
##p < .05 -> H0 wird verworfen, die Gruppen unterscheiden sich signifikant voneinander



### ezANOVA

# Paket laden
install.packages("ez") # Paket ez installieren
library(ez) # Paket laden

conspiracy$id <- 1:nrow(conspiracy) # ID-Variable erstellen
conspiracy$id <- as.factor(conspiracy$id) # ID-Variable in Faktor umwandeln

ezANOVA(conspiracy, wid = id, dv = ET, between = urban) # wid = Personenvariable, dv = abhängige Variable, between = Gruppierungsvariable, die zwischen Personen unterscheidet
ezANOVA(conspiracy, wid = id, dv = ET, between = urban, detailed = TRUE) # detaillierter Output, um Quadratsummen zu erhalten
# Nullhypothese wird verworfen: Gruppen sind nicht gleich



### Post-Hoc-Analysen

# t-Test mit Bonferroni-Korrektur
pairwise.t.test(conspiracy$ET, conspiracy$urban, p.adjust = 'bonferroni')
#ausschließlich Personen aus `urban` und `suburban` Umgegbungen unterscheiden sich in ihrer Überzeugung bezüglich des Extraterrestrial Cover-Ups

# Tukey-Test (kann nur auf aov-Objekt angewandt werden)
alternative<- aov(ET ~ urban, data = conspiracy) # aov-Objekt anlegen
summary(alternative) # Output des aov-Objekts
TukeyHSD(alternative, conf.level = 0.95) # Tukey's Test

tuk <- TukeyHSD(aov(ET ~ urban, data = conspiracy)) # Ergebnisse als Plot anzeigen lassen
plot(tuk)
##Schließt das Konfidenzintervall für die Mittelwertsdifferenz die Null (gestrichelte Linie) ein, so ist diese Mittelwertsdifferenz statistisch nicht signifikant!
##In unserer Stichprobe kam es zu Mittelwertsunterschieden auf `ET`, da sich die Gruppen `urban` (städtisch) und `suburban` (vorstädtisch) hinsichtlich der Zustimmung zur Überzeugung, dass die Existenz von Außerirdischen geheimgehalten wird, unterscheiden.

#aov-Objekt direkt mit ezANOVA ausgeben lassen
aov_t <- ezANOVA(conspiracy, wid = id, dv = ET, between = urban, return_aov = T)
names(aov_t)
class(aov_t$aov) # Klasse eines Objektes ausgeben lassen
TukeyHSD(aov_t$aov, conf.level = 0.95) # Funktion auf spezifischen Teil der ANOVA anwenden lassen

