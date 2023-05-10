# ---- ANOVA II: zweifaktorielle ANOVA ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBSc7_R_Files/08_anova2.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Julien P. Irmer, Miriam Scheppa-Lahyani & Martin Schultze. Skriptkompilierung von Kevin Pommeranz.



#Auskommentierne um Datensatz zu laden
load(url("https://pandar.netlify.app/post/conspiracy.rda"))

#Übersicht über Datensatz
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

#### Einfaktorielle Anova ----
library(ez)

# Personen-ID für ezANOVA
conspiracy$id <- as.factor(1:nrow(conspiracy))

# Wiederholung von voriger Sitzung
ezANOVA(data = conspiracy, wid = id, dv = ET, between = urban)



# Gruppenspezifische Mittelwerte mit tapply()
tapply(X = conspiracy$ET, INDEX = conspiracy$urban, FUN = mean)
tapply(X = conspiracy$ET, INDEX = conspiracy$edu, FUN = mean)

# Beispiel mit aggregate
# Mithilfe des aggregate-Befehls
aggregate(ET ~ urban, data = conspiracy, mean)
aggregate(ET ~ edu, data = conspiracy, mean)

# Mithilfe des aggregate-Befehls mit anderer Schreibweise (wie bei tapply)
aggregate(conspiracy$ET, list(conspiracy$urban), mean)
aggregate(conspiracy$ET, list(conspiracy$edu), mean)

# Mithilfe des describeBy-Befehls aus dem psych-Paket
library(psych)
describeBy(conspiracy$ET, conspiracy$urban)
describeBy(conspiracy$ET, conspiracy$edu)

#### Mehrfaktorielle ANOVA ----
# Gruppierungskombinationen erstellen
kombi <- conspiracy[, c('urban', 'edu')]

# Kombinationsspezifische Mittelwertetabelle
tapply(X = conspiracy$ET, INDEX = kombi, FUN = mean)

# Darstellung von Werten mit ezStats
ezStats(conspiracy, dv = ET, wid = id, between = c(urban, edu))

# grafische Darstellung der Mittelwerte
ezPlot(conspiracy, dv = ET, wid = id, between = c(urban, edu),
  x = urban, split = edu)

# mehrfaktorielle ANOVA mit ez
ezANOVA(conspiracy, dv = ET, wid = id, between = c(urban, edu), detailed = TRUE)

# adjust wegen signifikantem Levene Test
ezANOVA(conspiracy, dv = ET, wid = id, between = c(urban, edu), detailed = TRUE, white.adjust = TRUE)

#### Post-Hoc Analyse ----
TukeyHSD(aov(ET ~ urban*edu, conspiracy))

# Schönere Darstellung mit emmeans
library(emmeans)

emm <- emmeans(aov(ET ~ urban*edu, conspiracy), ~ urban * edu)

emm

plot(emm)

plot(emm, comparisons = TRUE)

pwpp(emm)

# aov-Objekt mit ezANOVA erzeugen
ez1 <- ezANOVA(conspiracy, dv = ET, wid = id, between = c(urban, edu), detailed = TRUE, return_aov = T)
aov1 <- ez1$aov
emm1 <- emmeans(aov1, ~ urban * edu)
plot(emm1, comparisons = TRUE) # identisch zu oben

#### Kontraste ----
emm

cont1 <- c(1, -1, 0, 0, 0, 0, 0, 0, 0)

contrast(emm, list(cont1))

sum(cont1)
sum(cont1) == 0

cont2 <- c(0, 0, 1, 0, 0, -.5, 0, 0, -.5)

contrast(emm, list(cont1, cont2), adjust = 'bonferroni')
