#### Syntax zum ggplotting Intro ####
#### Volles Skript: https://pandar.netlify.app/post/ggplotting-intro/ ####

### Beispieldaten ----

# Daten laden
load(url('https://pandar.netlify.com/post/edu_exp.rda'))

# Daten ansehen
head(edu_exp)


### ggplot Grundprinzipien ----

# Paket laden
library(ggplot2)

# Spanien auswählen
esp <- subset(edu_exp, geo == 'esp')


### Schichten ----

# Grundschicht
ggplot(esp)

# Schicht mit Daten (Achsen)
ggplot(esp, aes(x = Year, y = Primary))

# Liniendiagramm
ggplot(esp, aes(x = Year, y = Primary)) +
  geom_line()

# Punkte plus Linien
ggplot(esp, aes(x = Year, y = Primary)) +
  geom_line() +
  geom_point()


### Plots als Objekte ----

# Grundrezept als Objekt anlegen
basic <- ggplot(esp, aes(x = Year, y = Primary))

# Objekt plus Geometrie kombinieren
basic + geom_point()


### Farben und Ästhetik

# Einfache Farbe
ggplot(esp, aes(x = Year, y = Primary)) +
  geom_point(color = 'blue')

# Variablenfärbung
ggplot(esp, aes(x = Year, y = Primary)) +
  geom_point(aes(color = Primary))


### Gruppierte Abbildungen ----

# Datenauswahl
sel <- subset(edu_exp, geo %in% c('gbr', 'fra', 'ita', 'esp', 'pol'))

# Punktediagramm
ggplot(sel, aes(x = Year, y = Primary)) +
  geom_point()

# Gruppiertes Punktediagramm
ggplot(sel, aes(x = Year, y = Primary)) +
  geom_point(aes(color = Country))

# Gruppierte Punkte und Linien
ggplot(sel, aes(x = Year, y = Primary)) +
  geom_point(aes(color = Country)) +
  geom_line(aes(color = Country))

# Gruppierung für den gesamten Plot
ggplot(sel, aes(x = Year, y = Primary, color = Country)) +
  geom_point() + geom_line()


### Mehrere Variablen ----

# Variablen als Schichten
ggplot(esp, aes(x = Year)) +
  geom_line(aes(y = Primary), color = 'red') +
  geom_line(aes(y = Secondary), color = 'green') +
  geom_line(aes(y = Tertiary), color = 'blue')


# Daten Umstrukturierung
sel_long <- reshape(sel, direction = 'long',
  varying = c('Primary', 'Secondary', 'Tertiary'),
  v.names = 'Expense',
  timevar = 'Type',
  times = c('Primary', 'Secondary', 'Tertiary'))

# Inspizieren
head(sel_long)

# Mehrere Variablen als Gruppen
subset(sel_long, geo == 'esp') |>
  ggplot(aes(x = Year, y = Expense, color = Type)) +
    geom_line() + geom_point()


### Facetting ----

# Bildungstypen als Facets
ggplot(sel_long, aes(x = Year, y = Expense, color = Country)) +
  geom_point() + geom_line() +
  facet_wrap(~ Type)

# Facet Grid für Typen und Länder
ggplot(sel_long, aes(x = Year, y = Expense, color = Country)) +
  geom_point() + geom_line() +
  facet_grid(Type ~ Country)

# Freie Skalierung
ggplot(sel_long, aes(x = Year, y = Expense, color = Country)) +
  geom_point() + geom_line() +
  facet_grid(Type ~ Country, scales = 'free')

