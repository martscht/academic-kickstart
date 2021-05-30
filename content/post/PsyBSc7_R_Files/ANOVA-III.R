#######################
### ANOVA III: ANOVA mit Messwiederholung
# von Miriam Scheppa-Lahyani und Martin Schultze

###Datensatz laden
load(url("https://pandar.netlify.app/post/alc.rda")) #direkt von pandaR laden
dim(alc) #Überblick über Datensatz verschaffen
head(alc)

#Personen-ID
table(alc$id)


###Umwandeln des Datensatzes in langes Format
alc_long <- reshape(data = ...,
                    varying = list(...),
                    direction = ...,
                    timevar = 'age',
                    v.names = 'alcuse',
                    times = c(14, 15, 16))
head(alc_long) #Kopfzeilen ausgeben

#Alternative: reshape
alc_long <- reshape(data = alc,
                    varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16')), #Erstellung einer Liste von Vektoren
                    direction = 'long', #Datenformat: lang
                    timevar = 'age', #Zeitvariable expliziten Namen geben
                    times = c(14, 15, 16), #Alter soll bei 14 anfangen
                    v.names = 'alcuse') #Messwiederholte Variable benennen
head(alc_long)

#Rückübertragung in weites Format
alc_wide <- reshape(alc_long,
                    v.names = 'alcuse',
                    timevar = 'age',
                    idvar = 'id',
                    direction = 'wide')
head(alc_wide)


###Einfaktorielle ANOVA mit Messwiederholung

##Deskriptivstatistik
alc_long$age <- as.factor(alc_long$age) #Altersvariable in Faktor umwandeln
library(ez) #ez-Paket laden
ezStats(alc_long, alcuse, id, within = age) #AV = alcuse, Personenvariable = id, Variable, die zwischen den Messungen innerhalb der gleichen Personen unterscheidet = age
ezPlot(alc_long, alcuse, id, within = age, x = age) #Plot

##ezANOVA für Messwiederholungen
#Sphärizitätsannahme
alc$diff_1415 <- alc$alcuse.15 - alc$alcuse.14
alc$diff_1416 <- alc$alcuse.16 - alc$alcuse.14
alc$diff_1516 <- alc$alcuse.16 - alc$alcuse.15
var(alc[, c('diff_1415', 'diff_1416', 'diff_1516')])

ezANOVA(alc_long, alcuse, id, within = age)
#Mauchly-Test zeigt an, dass es bedeutsame Abweichungen von der Sphärizitätsannahme gibt (p < .05)
#-> p-Wert für die ANOVA sollte aus dritter, korrigierter Tabelle entnommen werden
#Es zeigt sich ein signifikanter Unterschied im Alkoholkonsum zwischen dem 14., 15. & 16. Lebensjahr


###Effektgröße & Intraklassenkorrelation
psych::ICC(alc[, c('alcuse.14', 'alcuse.15', 'alcuse.16')]) #Intraklassenkorrelationskoeffizient ICC
#Der in diesem Fall relevante ICC-Typ (`type`) ist als `ICC1` gelistet. In diesem Fall (ICC = .51) bedeutet es also, dass ca. 50% der Unterschiede zwischen Messungen auf stabile Personeneigenschaften zurückgehen.
#Das Alter hat einen kleinen Effekt auf das Ausmaß des Alkoholkonsums bei Jugendlichen


###Kontraste
library(emmeans) #emmeans-Paket laden

# aov-Objekt erzeugen
wdh_aov <- aov(alcuse ~ age + Error(id/age), #Error setzt Personen-ID und Zeitvariable in Beziehung zueinander
               data = alc_long)

# Kontraste vorbereiten
em <- emmeans(wdh_aov, ~ age)
em

##Polynomiale Kontraste
lin_cont <- c(-1, 0, 1) #Kontrastkoeffizienten müssen für linearen Trend so gewählt werden, dass sie mit den Zeitabständen zwischen den wiederholten Messungen korrespondieren

#Linearen Verlauf grafisch prüfen
library(ggplot2) #ggplot2-Paket laden
ezPlot(alc_long, alcuse, id, within = age, x = age) + # ezPlot siehe oben
  theme_minimal() + xlab('Alter') # beliebige ggplot Erweiterungen anfügen
ezPlot(alc_long, alcuse, id, within = age, x = age) + geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE) #Verlauf darstellen, Alter in numerische Variable umwandeln
contrast(em, list(lin_cont))

#Quadratischen Verlauf grafisch prüfen
ezPlot(alc_long, alcuse, id, within = age,
       x = age) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE,
              formula = y ~ x + I(x^2), color = 'red')

#Kontraste prüfen
#entsprechenden Kontrast definieren und die Kontrastprüfung gleichzeitig für den linearen und quadratischen Effekt durchführen
#Bonferroni-Korrektur für p-Werte
alc_long <- reshape(data = alc,
                    varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16')),
                    direction = 'long',
                    timevar = 'age',
                    times = c(14, 15, 16),
                    v.names = 'alcuse')

alc_long$age <- as.factor(alc_long$age)

lin_cont <- c(-1, 0, 1)
qua_cont <- c(1, -2, 1)

wdh_aov <- aov(alcuse ~ age + Error(id/age), data = alc_long)
em <- emmeans(wdh_aov, ~ age)

contrast(em, list(lin_cont, qua_cont),
         adjust = 'bonferroni')
#linearer Kontrast ist signfikant -> wird verworfen
#Wir nehmen an, dass es einen quadratischen Trend in den Daten gibt

#Abkürzung für typische Kontraste
contrast(em, interaction = 'poly', adjust = 'bonferroni')

# Alle paarweisen Vergleiche
contrast(em, method = 'pairwise', adjust = 'bonferroni')
contrast(em, adjust = 'bonferroni') #Vergleich mit dem Mittel
#Alter 14 und 16 weichen vom Mittel ab


###Split-Plot ANOVA für mehrere Gruppen und mehrere Zeitpunkte
# Deskriptive Statistiken
ezStats(alc_long,
        dv = alcuse,
        wid = id,
        within = age,   #zwischen den jährlichen Messungen
        between = coa)  #zwischen den Jugendlichen Gruppen

# Grafische Darstellung
ezPlot(alc_long,
       dv = alcuse,
       wid = id,
       within = age,
       between = coa,
       x = age, split = coa)

#Effekte
#Verändert sich der Alkoholkonsum über die Zeit? (Haupteffekt A)
#Unterscheiden sich Jugendliche von Alkoholikern von Jugendlichen nicht alkoholabhängiger Eltern in ihrem mittleren Alkoholkonsum? (Haupteffekt B)
#Unterscheidet sich die Veränderung mit der Zeit im Alkoholkonsum zwischen den beiden Gruppen von Jugendlichen? (Interaktionseffekt)
ezANOVA(alc_long, dv = alcuse, wid = id, within = age, between = coa)
heplots::boxM(alc[, c('alcuse.14', 'alcuse.15', 'alcuse.16')], group = alc$coa) #Gleichheit der Varianz-Kovarianz-Matrizen der messwiederholten Variablen über alle Gruppen hinweg prüfen
