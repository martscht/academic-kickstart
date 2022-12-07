#######################
### Hierarchische Regression
# von Julien P. Irmer und Johannes Hartig
#
# Pakete laden ----
# Benötigte Pakete --> Installieren, falls nicht schon vorhanden!
library(lme4)         # Für die Mehrebenen-Regression
library(dplyr)        # Komfort-Funktionen für die Datentransformationen
library(ICC)          # Für die einfache Berechung von ICCs
library(ggplot2)      # Für Grafiken
library(interactions) # Zur Veranschaulichung von Moderator-Effekten

# Daten einlesen und vorbereiten
lockdown <- read.csv(url("https://osf.io/dc6me/download"))

# Entfernen der Personen, für die weniger als zwei Messpunkte vorhanden sind
# (Auschluss von Fällen, deren ID nur einmal vorkommt)
lockdown <- lockdown[-which(lockdown$ID %in% names(which(table(lockdown$ID)==1))),]

# Daten aufbereiten, Variablen auswählen extrahieren und in Nummern umwandeln
# Entfernen von Minderjährigen & unbestimmtes Gender mit den Funktionen filter() und select () aus dplyr.
lockdown <- lockdown %>%
  filter((Age >= 18) & (Gender == 1 | Gender == 2)) %>%
  select(c("ID", "Wave", "Age", "Gender", "Income", "EWB","PWB","SWB",
           "IWB","E.threat","H.threat", "Optimism",
           "Self.efficacy","Hope","P.Wisdom","ST.Wisdom","Grat.being",
           "Grat.world","PD","Acc","Time","EWB.baseline","PWB.baseline",
           "SWB.baseline","IWB.baseline"))

# Standardisieren der AVs
lockdown[,c("EWB", "PWB", "SWB", "IWB")] <- scale(lockdown[,c("EWB", "PWB", "SWB", "IWB")])
# Standardisieren möglicher Prädiktoren
lockdown[,c("E.threat", "H.threat", "Optimism", "Self.efficacy", "Hope", "P.Wisdom",
            "ST.Wisdom", "Grat.being", "Grat.world")] <-
  scale(lockdown[,c("E.threat", "H.threat", "Optimism", "Self.efficacy", "Hope", "P.Wisdom",
                    "ST.Wisdom", "Grat.being", "Grat.world")])

# ID in Faktor Umwandeln
lockdown$ID <- as.factor(lockdown$ID)

# ICCs ----
# ICCs auf Basis der Varianzkomponenten mit der Funktion ICCbare
ICCbare(ID, EWB, data = lockdown)
ICCbare(ID, PWB, data = lockdown)
ICCbare(ID, SWB, data = lockdown)
ICCbare(ID, IWB, data = lockdown)

## Grafische Veranschaulichung ----
# Individuelle Verläufe für Psychological Well Being
ggplot(lockdown, aes(x=Wave, y=PWB, color=ID)) +
  theme_bw() + guides(color="none") +
  geom_line()

# Zufälliger Subset von Fällen für eine übersichtlichere Darstellung
IDs.subset <- c("03858ebe", "ddf85cd4", "fab6bb4d", "c7b6e168", "c0661f6a", "f005ee8d", "f037053f", "166a701e",
                "3ff1ffae", "486d63a8", "4b6a0366", "ba2ccd92", "cdbfa68a", "f43569d8", "c0c3cb43")
# Grafik mit dem Subset
ggplot(lockdown[lockdown$ID %in% IDs.subset,], aes(x=Wave, y=PWB, color=ID)) +
  theme_bw() + guides(color="none") +
  geom_line()

# Grafik mit dem Subset, Zeit als UV
ggplot(lockdown[lockdown$ID %in% IDs.subset,], aes(x=Time, y=PWB, color=ID)) +
  theme_bw() + guides(color="none") +
  geom_line()


## Nullmodell ----
# Nulllmodell für PWB
m0 <- lmer(PWB ~ 1 + (1 | ID), data = lockdown)
summary(m0)
# ICC "per Hand" berechnen
sig2_b <- VarCorr(m0)$ID[1]   # Varianz des random intercept = Between-Varianz
sig2_w <- summary(m0)$sigma^2 # Residual-Varianz = Within-Varianz
sig2_b / (sig2_b + sig2_w)    # ICC = Var.b / (Var.b + Var.w)

# Ebene-1-Effekt der Zeit im Lockdown ----
## Fester Effekt ----
PWB.time.fixed <- lmer(PWB ~ 1 + Time + (1 | ID), data = lockdown)
summary(PWB.time.fixed)
# Grafische Veranschaulichung
lockdown$pred <- predict(PWB.time.fixed) # Vorhergesagte Werte im Datensatz speichern
# Grafik mit dem Subset
ggplot(lockdown[lockdown$ID %in% IDs.subset,], aes(x=Time, y=pred, color=ID)) +
  theme_bw() + guides(color="none") +
  geom_line()

## Zufälliger Effekt (random slope) ----
PWB.time.random <- lmer(PWB ~ 1 + Time + (1 + Time | ID), data = lockdown)
summary(PWB.time.random)

# fehlende Konvergenz:
PWB.time.random <- lmer(PWB ~ 1 + Time + (1 + Time | ID), data = lockdown,
                        control = lmerControl(optimizer ="Nelder_Mead"))
summary(PWB.time.random)

# Modellvergleich
anova(PWB.time.fixed, PWB.time.random, refit=FALSE)

# Grafische Veranschaulichung
lockdown$pred <- predict(PWB.time.random) # Vorhergesagte Werte im Datensatz speichern
# Grafik mit dem Subset
ggplot(lockdown[lockdown$ID %in% IDs.subset,], aes(x=Time, y=pred, color=ID)) +
  theme_bw() + guides(color="none") +
  geom_line()

# Histogramm der individuellen Slopes als Summe aus festem Effekt und Residuen
hist(fixef(PWB.time.random)["Time"] + ranef(PWB.time.random)$ID$Time,
     main="Histogramm des Zeiteffekts", xlab = expression(beta[1]),
     breaks = seq(-0.2,0.2,0.025))
abline(v=fixef(PWB.time.random)["Time"], col="blue") # Lage des festen Effektes kennzeichnen

# Effekt von Alter als Ebene-2-Variable ----
# Alter zentrieren
lockdown$Age <- scale(lockdown$Age, scale = FALSE)

## Modell mit Haupteffekt ----
PWB.Age <- lmer(PWB ~ 1 + Age + (1 | ID), data = lockdown)
summary(PWB.Age)

## Modell mit Cross-Level-Interaktion ----
lockdown$Time <- scale(lockdown$Time, scale = FALSE) # Zeit zentrieren
PWB.Age.Time <- lmer(PWB ~ 1 + Age + Time + Age:Time + (1 + Time | ID), data = lockdown)
summary(PWB.Age.Time)

# fehlende Konvergenz:
PWB.Age.Time <- lmer(PWB ~ 1 + Age + Time + Age:Time + (1 + Time | ID), data = lockdown,
                     control = lmerControl(optimizer ="Nelder_Mead"))
summary(PWB.Age.Time)

# Visualisierung
interact_plot(model = PWB.Age.Time, pred = Time, modx = Age)

