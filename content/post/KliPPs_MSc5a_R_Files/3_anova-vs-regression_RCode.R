#######################
### ANOVA vs. Regression
# von Julien P. Irmer


## Daten laden
osf <- read.csv(file = url("https://osf.io/zc8ut/download"))
names(osf)

osf <- osf[, c("ID", "group", "stratum", "bsi_post", "swls_post", "pas_post")] # Datensatz kürzen
head(osf)

### Listenweiser Fallausschluss
dim(osf) # vorher
osf <- na.omit(osf)
dim(osf) # nach Fallauschluss


## Regression

### Modellvergleiche
reg <- lm(bsi_post ~ 1 + swls_post + pas_post, data = osf) # Modell 1
summary(reg)

reg0 <-  lm(bsi_post ~ 1, data = osf) # Modell 2
anova(reg0, reg) # Modellvergleich

anova0 <- anova(reg0, reg)
R2 <- anova0$`Sum of Sq`[2] / anova0$RSS[1]
R2 # R^2 mit Hand
summary(reg)$r.squared # R^2 aus dem lm-Objekt
var(predict(reg))/var(osf$bsi_post) # über die Vorhersage von Werten mittels "predict"

F_emp <- (R2/2)/((1-R2)/91)
F_emp # empirischer F-Bruch mit Hand
anova0$F[2] # empirischer F-Bruch aus anova-Objekt


### Kategoriale Prädiktoren

osf$group <- factor(osf$group) # Gruppierungsvariable in Faktor umwandeln
reg_dummy1 <- lm(bsi_post  ~ 1 + group, data = osf)
summary(reg_dummy1)

levels(osf$group) # Levels der Gruppierungsvariable
aggregate(bsi_post ~ group, data = osf, FUN = mean) # Mittelwerte der beiden Gruppen
anova(reg0, reg_dummy1) # Treatmenteffekt
summary(reg_dummy1)$r.squared # R²



## Einfaktorielle ANOVA und t-Test

### ANOVA
library(ez) # Paket laden

osf$ID <- as.factor(osf$ID) # ID-Variable in Faktor umwandeln
ezANOVA(data = osf, wid = ID, dv = bsi_post, between = group) # ANOVA
ezANOVA(data = osf, wid = ID, dv = bsi_post, between = group, detailed = T) # detaillierte Ausgabe

ezANOVA1 <- ezANOVA(data = osf, wid = ID, dv = bsi_post, between = group, detailed = T)
ezANOVA1$ANOVA$ges
summary(reg_dummy1)$r.squared

ezANOVA1$ANOVA$F
anova(reg0, reg_dummy1)$F

### t-Test
ttest1 <- t.test(bsi_post ~ group, data = osf, var.equal = T) # t-Test als Objekt abspeichern
ttest1

ezANOVA1$ANOVA$p
anova(reg0, reg_dummy1)$`Pr(>F)`
ttest1$p.value

# t-Wert in F-Wert transformieren
ezANOVA1$ANOVA$F
anova(reg0, reg_dummy1)$F
ttest1$statistic^2



## Mehrfaktorielle ANOVA

### Quadratsumme Typ I
osf$stratum <- factor(osf$stratum) # Gruppierungsvariable in Faktor umwandeln
ezANOVA1 <- ezANOVA(data = osf, dv = bsi_post, between = c(group, stratum), wid = ID, detailed = T, type = 1) # Quadratsummentyp einstellen
ezANOVA1

ezPlot(data = osf, dv = bsi_post, between = c(group, stratum), wid = ID, x = stratum, split = group)
ezANOVA(data = osf, dv = bsi_post, between = c(stratum, group), wid = ID, detailed = T, type = 1)

reg0 <- lm(bsi_post ~ 1, data = osf)  # Null-Modell (leeres Modell)
reg_g <- lm(bsi_post ~ group, data = osf) # Modell mit Haupteffekt des Treatments
reg_s <- lm(bsi_post ~ stratum, data = osf) # Modell mit Haupteffekt der Diagnose
reg_gs <- lm(bsi_post ~ group + stratum, data = osf) # Modell mit beiden Haupteffekten
reg_gsi <- lm(bsi_post ~ group + stratum + group:stratum, data = osf)  # Modell mit beiden Haupteffekten und Interaktion

anova(reg0, reg_g, reg_gs, reg_gsi) # 4 geschachtelte Modelle gegeneinander testen
anova(reg_gsi) # alternativ

### Quadratsumme Typ II
ezANOVA2 <- ezANOVA(data = osf, dv = bsi_post, between = c(group, stratum), wid = ID, type = 2)
ezANOVA2
ezANOVA(data = osf, dv = bsi_post, between = c(stratum, group), wid = ID, type = 2)

library(car)
Anova(reg_gsi, type = 2) # alternativ

Anova(reg_gs, type = 2) # simulatane Inkrementsprüfung
anova(reg_s, reg_gs) # Inkrement des Treatments
anova(reg_g, reg_gs) # Inkrement der Diagnose

### Quadratsumme Typ III
ezANOVA(data = osf, dv = bsi_post, between = c(group, stratum), wid = ID, type = 3)

options("contrasts") # Einstellungen Kontrastbildung

# verstelle die Art, wie Kontraste bestimmt werden --- Achtung! Immer wieder zurückstellen
reg_gsi_contr.sum <- lm(bsi_post ~ group + stratum + group:stratum, data = osf,
                        contrasts = list("group" = contr.sum, "stratum" = contr.sum))  # contr.sum-Kodierung
Anova(reg_gsi_contr.sum, type = 3)

# Einstellungen zurücksetzen zum Default:
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))
