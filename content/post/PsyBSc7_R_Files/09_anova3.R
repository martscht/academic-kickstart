# ---- ANOVA III: Varianzanalyse mit Messwiederholung ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBsc7_R_Files/09_anova3.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Miriam Scheppa-Lahyani, Julien P. Irmer, Kai J. Nehler & Martin Schultze. Skriptkompilierung von Kevin Pommeranz.



# Ausklammern um Datensatz zu laden
load(url("https://pandar.netlify.app/post/alc.rda"))

# Datensatz anschauen
dim(alc)
head(alc)

# Kontrollieren, dass jede id nur einmal vorkommt
table(alc$id)

#### Datensatz zu Langformat transformieren ----
alc_long <- reshape(data = alc,
  varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16')),
  direction = 'long')

head(alc_long)

## # Theoretisches Beispiel
## varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16'),
##   c('weeduse.14', 'weeduse.15', 'weeduse.16'))

# Werte betrachten
alc_long[alc_long$id == 1, ]

# timevar bestimmen
alc_long <- reshape(data = alc,
  varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16')),
  direction = 'long',
  timevar = 'age')

head(alc_long)

# Alter korrekt kodieren per times
alc_long <- reshape(data = alc,
  varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16')),
  direction = 'long',
  timevar = 'age',
  times = c(14, 15, 16))

head(alc_long)

# Umbenennung von varying-Variablen
alc_long <- reshape(data = alc,
  varying = list(c('alcuse.14', 'alcuse.15', 'alcuse.16')),
  direction = 'long',
  timevar = 'age',
  times = c(14, 15, 16),
  v.names = 'alcuse')

head(alc_long)

#### Rückübertragung in breites Format ---

alc_wide <- reshape(alc_long, 
            v.names = 'alcuse', 
            timevar = 'age', 
            idvar = 'id', 
            direction = 'wide')
head(alc_wide)

#### Einfaktorielle ANOVA mit Messwiederholung ----

# Deskriptivstatistik
library(ez)
ezStats(alc_long, alcuse, id, within = age)

alc_long$age <- as.factor(alc_long$age)

ezStats(alc_long, alcuse, id, within = age)

ezPlot(alc_long, alcuse, id, within = age,
  x = age)

alc$diff_1415 <- alc$alcuse.15 - alc$alcuse.14
alc$diff_1416 <- alc$alcuse.16 - alc$alcuse.14
alc$diff_1516 <- alc$alcuse.16 - alc$alcuse.15
var(alc[, c('diff_1415', 'diff_1416', 'diff_1516')])

ezANOVA(data = alc_long, dv = alcuse, wid = id, within = age)

# Effektgröße und ICC

psych::ICC(alc[, c('alcuse.14', 'alcuse.15', 'alcuse.16')])

#### Kontraste ----

library(emmeans)

# aov-Objekt erzeugen
wdh_aov <- aov(alcuse ~ age + Error(id/age), 
  data = alc_long)
wdh_aov

# Kontraste vorbereiten
em <- emmeans(wdh_aov, ~ age)
em

lin_cont <- c(-1, 0, 1)

library(ggplot2)

# ezPlot siehe oben
ezPlot(alc_long, alcuse, id, within = age,
  x = age) +
# beliebige ggplot Erweiterungen anfügen
  theme_minimal() +
  xlab('Alter')

ezPlot(alc_long, alcuse, id, within = age,
  x = age) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE)

contrast(em, list(lin_cont))

ezPlot(alc_long, alcuse, id, within = age,
  x = age) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE) +
  geom_smooth(aes(x = as.numeric(age)), method = 'lm', se = FALSE,
    formula = y ~ x + I(x^2), color = 'red')

lin_cont <- c(-1, 0, 1)
qua_cont <- c(1, -2, 1)

contrast(em, list(lin_cont, qua_cont),
  adjust = 'bonferroni')

contrast(em, interaction = 'poly')

contrast(em, interaction = 'poly',
  adjust = 'bonferroni')

# Alle paarweisen Vergleiche
contrast(em, method = 'pairwise',
  adjust = 'bonferroni')

# Vergleiche mit dem Mittel
contrast(em,
  adjust = 'bonferroni')

#### Split-Plot ANOVA

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

ezANOVA(alc_long, 
  dv = alcuse, 
  wid = id, 
  within = age, 
  between = coa)

heplots::boxM(alc[, c('alcuse.14', 'alcuse.15', 'alcuse.16')], group = alc$coa)

ezANOVA(alc_long, 
  dv = alcuse, 
  wid = id, 
  within = age, 
  between = coa)
