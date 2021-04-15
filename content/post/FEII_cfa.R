#### R-Skript zur 3. Sitzung ####
# R Chunks wurden aus dem HTML exportiert und sind dort alle enthalten


#### Vorbereitung ----
# lavaan laden
library(lavaan)

#### Datensatz ----

# Daten laden
data(conspiracy, package = 'PsyMSc1')

# Überblick
conspiracy

# Allgemeine Struktur des Datensatzes
str(conspiracy)

# Namen aller Variable im Datensatz
names(conspiracy)

# Anzahl der Zeilen (Personen) und Spalten (Variablen)
nrow(conspiracy)
ncol(conspiracy)

# Die ersten 6 Zeilen ansehen
head(conspiracy)


#### Einfaktorielles Modell ----

# Modellsyntax
mod1 <- '# Faktorladungen
GC =~ Q2 + Q7 + Q12

# Residualvarianzen
Q2 ~~ Q2
Q7 ~~ Q7
Q12 ~~ Q12

# Latente Varianz
GC ~~ GC'

# Modell schätzen
fit1 <- lavaan(mod1, conspiracy)


# Ergebniszusammenfassung
summary(fit1)


# Empirische Kovarianzmatrix
inspect(fit1, 'sampstat')

# Anzahl der Parameter
inspect(fit1, 'npar')

# Auflistung der freien Parameter
inspect(fit1, 'free')

## Zweiter Versuch

# Modell mit fixierter Ladung
mod1 <- '# Faktorladungen
GC =~ 1*Q2 + Q7 + Q12

# Residualvarianzen
Q2 ~~ Q2
Q7 ~~ Q7
Q12 ~~ Q12

# Latente Varianz
GC ~~ GC'

# Modell mit fixierter latenter Varianz
mod1b <- '# Faktorladungen
GC =~ Q2 + Q7 + Q12

# Residualvarianzen
Q2 ~~ Q2
Q7 ~~ Q7
Q12 ~~ Q12

# Latente Varianz
GC ~~ 1*GC'

# Modellschätzung
fit1 <- lavaan(mod1, conspiracy)

# Liste freier Parameter
inspect(fit1, 'free')

# Ergebniszusammenfassung
summary(fit1)


## Exkurs: Grafik erstellen (mit ggplot2):
library(ggplot2)

tmp <- inspect(fit1, 'est')
plottable <- data.frame(GC = seq(0, 5, .01))
plottable <- cbind(plottable, t(tmp$lambda %*% plottable$GC))
plottable <- reshape(plottable,
  varying = list(c('Q2', 'Q7', 'Q12')),
  v.names = 'manifest',
  timevar = 'variable',
  times = c('Q2', 'Q7', 'Q12'),
  idvar = 'GC',
  direction = 'long')
plottable$variable <- factor(plottable$variable, levels = c('Q2', 'Q7', 'Q12'))

ggplot(plottable, aes(x = GC, y = manifest, group = variable)) +
  geom_line(aes(color = variable)) +
  theme_minimal() +
  labs(y = 'Manifeste Variablen', x = 'Latente Variable (GC)',
    color = 'Variable')
## ende Exkurs

# R-Quadrat / Reliabilität
inspect(fit1, 'rsquare')

# Erweiterte Ergebniszusammenfassung
summary(fit1, rsq = TRUE)


# Modellsyntax mit Mittelwertsstruktur
  # Intercepts werden immer mit ~1 angesprochen
mod2 <- '
# Faktorladungen
GC =~ 1*Q2 + Q7 + Q12

# Residualvarianzen
Q2 ~~ Q2
Q7 ~~ Q7
Q12 ~~ Q12

# Latente Varianz
GC ~~ GC

# Intercepts
Q2 ~ 1
Q7 ~ 1
Q12 ~ 1'

# Modellschätzung
fit2 <- lavaan(mod2, conspiracy)

# Überprüfen
fit2

# Modellzusammenfassung
summary(fit2)


## Exkurs: Grafik erstellen (mit ggplot2):
tmp <- inspect(fit2, 'est')

plottable <- data.frame(GC = seq(-2.5, 2.5, .01))
plottable <- cbind(plottable, t(tmp$lambda %*% plottable$GC + as.vector(tmp$nu)))
plottable <- reshape(plottable,
  varying = list(c('Q2', 'Q7', 'Q12')),
  v.names = 'manifest',
  timevar = 'variable',
  times = c('Q2', 'Q7', 'Q12'),
  idvar = 'GC',
  direction = 'long')
plottable$variable <- factor(plottable$variable, levels = c('Q2', 'Q7', 'Q12'))

ggplot(plottable, aes(x = GC, y = manifest, group = variable)) +
  geom_line(aes(color = variable)) +
  theme_minimal() +
  labs(y = 'Manifeste Variablen', x = 'Latente Variable (GC)',
    color = 'Variable') +
  geom_vline(xintercept = 0, lty = 2)
## Ende Exkurs


# Vereinfachte Modellsyntax
mod1_simple <- 'GC =~ Q2 + Q7 + Q12'

# Modellschätzung mit cfa()
fit1_simple <- cfa(mod1_simple, conspiracy)

# Ergebniszusammenfassung
summary(fit1_simple)

# cfa() mit Mittelwertsstruktur
fit2_simple <- cfa(mod1_simple, conspiracy,
  meanstructure = TRUE)

# Ergebniszusammenfassung
summary(fit2_simple)


#### Zweifaktorielles Modell ----

# Modellsyntax
mod_two <- '
  GC =~ Q2 + Q7 + Q12
  CI =~ Q5 + Q10 + Q15'

# Modellschätzung
fit_two <- cfa(mod_two, conspiracy,
  meanstructure = TRUE)

# Allgemeiner Überblick
summary(fit_two)

# Reliabilität der sechs Aussagen
inspect(fit_two, 'rsquare')

# Nutzen Sie die Hilfe um zu sehen, was inspect ausgeben kann!
?inspect

# Latent Korrelationsmatrix (3 Versionen)
inspect(fit_two, 'cor.lv')
inspect(fit_two, 'std')      # hier in $psi
inspect(fit_two, 'std.lv')   # hier in $psi

# Parametertabelle
parameterEstimates(fit_two)

# Parameter in Objekt ablegen
para <- parameterEstimates(fit_two)

# Spezifische Elemente werden in R mit [Zeile, Spalte] ausgewählt
para[1, ]     # erste Zeile, alle Spalten
para[, 1]     # erste Spalte, alle Zeilen

# Das logische "ist gleich" kann genutzt werden um einen Filter-Vektor zu erstellen
letters[1:10] == 'c'

# Namen der Spalten von para
names(para)

# Parametertypen werden anhand der Spalte $op unterschieden
para$op

# Auswahl der Intercepts
para[para$op == '~1', ]


# Konfidenzintervalle
summary(fit_two, ci = TRUE)


#### Modelfit ----

# Manifeste Kovarianzmatrizen
# Empirische Matrix
inspect(fit_two, 'sampstat')$cov

# Modellimplizierte Matrix
inspect(fit_two, 'cov.ov')

# Fit Statistiken
modelfit <- inspect(fit_two, 'fit.measures')

# Namen der Fitstatistiken
names(modelfit)

# LogLikelihood
modelfit['logl']

# LogLikelihood unrestringiertes Modell
modelfit['unrestricted.logl']

# Freiheitsgrade
modelfit['df']

# Empirische Prüfgröße
emp <- 2 * (modelfit['unrestricted.logl'] - modelfit['logl'])
emp

# p-Wert
pchisq(emp, 8, lower.tail = FALSE)

# Modeltest im Output
# Aus inspect:
modelfit[c('chisq', 'df', 'pvalue')]

# Aufruf des lavaan Objekts
fit_two

# RMSEA und SRMR
modelfit[c('rmsea', 'srmr')]

# CFI, TLI, NNFI
modelfit[c('cfi', 'tli', 'nnfi')]

## Cutoffs für Modelfit
# ggf. ezCutoffs installieren
install.packages('ezCutoffs')

# Paket laden
library(ezCutoffs)

# Cutoffs ermitteln
cutoff <- ezCutoffs(mod_two, conspiracy)

# Vergleich von Cutoffs und Empirischen Werten
cutoff


## Misfit lokalisieren

# Residuen der Kovarianzmatrizen
inspect(fit_two, 'residuals')$cov

# Standardisierte Fassung
residuals(fit_two, 'standardized')$cov

# Als Objekt anlegen
resi <- residuals(fit_two, 'standardized')$cov

# Varianzen in der Diagonale
diag(resi)

# Innerhalb des GC
resi[1:3, 1:3]

# Die Varianzen und Kovarianzen zu CI-Items extrahieren:
resi[4:6, 4:6]

# Logische Matrix, ob etwas vom Betrag > 2 ist
abs(resi[4:6, 4:6]) > 2

# Kreuzkorrelationen
resi[4:6, 1:3]


# Modifikationsindizes
modindices(fit_two, sort. = TRUE, minimum.value = 5)

# Modifikationsindizes, ohne Aufbereitung
modindices(fit_two)


## Modellvergleiche

# Modell mit Querladung
mod_three <- '
  GC =~ Q2 + Q7 + Q12 + Q5
  CI =~ Q5 + Q10 + Q15'

# Schätzung
fit_three <- cfa(mod_three, conspiracy,
  meanstructure = TRUE)

# Allgemeine Zusammenfassung
summary(fit_three)

# Modellpassung
inspect(fit_three, 'fitmeasures')

# Modellvergleich via LRT
lavTestLRT(fit_two, fit_three)

# Modell mit Residualkovarianz
mod_four <- '
  GC =~ Q2 + Q7 + Q12
  CI =~ Q5 + Q10 + Q15

  Q10 ~~ Q15'

# Mdoellschätzung
fit_four <- cfa(mod_four, conspiracy,
  meanstructure = TRUE)

# Modellvergleich
lavTestLRT(fit_two, fit_four)


#### Appendix: Hierarchie der Messmodelle ----

# Daten laden
data(stat_test, package = 'PsyMSc1')

# Überblick über Datenlage
summary(stat_test)


## tau-kongenerisch
# Modell
mod1 <- 'stat =~ test1 + test2 + test3'

# Schätzung
fit1 <- cfa(mod1, stat_test,
  meanstructure = TRUE)

# Ergebniszusammenfassung
summary(fit1)


## essentiell tau-äquivalent
# Modell
mod2 <- 'stat =~ 1*test1 + 1*test2 + 1*test3'

# Schätzung
fit2 <- cfa(mod2, stat_test,
  meanstructure = TRUE)

# Ergebniszusammenfassung
summary(fit2)

# Modellvergleich
lavTestLRT(fit1, fit2)


## tau-äquivalent
# Modell
mod3 <- 'stat =~ 1*test1 + 1*test2 + 1*test3
  test1 ~ (alp)*1
  test2 ~ (alp)*1
  test3 ~ (alp)*1'

# Schätzung
fit3 <- cfa(mod3, stat_test,
  meanstructure = TRUE)

# Ergebniszusammenfassung
summary(fit3)

# Modellvergleich
lavTestLRT(fit2, fit3)


## essentiell tau-parallel
# Modell
mod4 <- 'stat =~ 1*test1 + 1*test2 + 1*test3
  test1 ~~ (eps)*test1
  test2 ~~ (eps)*test2
  test3 ~~ (eps)*test3'

# Schätzung
fit4 <- cfa(mod4, stat_test,
  meanstructure = TRUE)

# Modellvergleich
lavTestLRT(fit2, fit4)


## tau-parallel
# Modell
mod5 <- 'stat =~ 1*test1 + 1*test2 + 1*test3

  test1 ~ (alp)*1
  test2 ~ (alp)*1
  test3 ~ (alp)*1

  test1 ~~ (eps)*test1
  test2 ~~ (eps)*test2
  test3 ~~ (eps)*test3'

# Schätzung
fit5 <- cfa(mod5, stat_test,
  meanstructure = TRUE)

# Modellzusammenfassung
summary(fit5, rsq = TRUE)

# Modellvergleiche
lavTestLRT(fit1, fit2, fit3, fit5)
lavTestLRT(fit1, fit2, fit4, fit5)
