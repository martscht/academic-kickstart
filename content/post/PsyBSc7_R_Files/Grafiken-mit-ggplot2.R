#######################
### Grafiken mit ggplot2
# von Martin Schultze


### Paket ggplot2 laden
library(ggplot2)

### Datensatz laden
load(url('https://pandar.netlify.com/post/edu_exp.rda'))
head(edu_exp)


## Schichten
edu_2013 <- subset(edu_exp, Year == 2013) # Subset mit den Daten für 2013 erstellen

ggplot(edu_2013) # 1.Schicht: Daten
ggplot(edu_2013, aes(x = Primary, y = Index)) # 2.Schicht: Ästhetik
ggplot(edu_2013, aes(x = Primary, y = Index)) + geom_point() # 3.Schicht: geometrische Form

cor.test(edu_2013$Index, edu_2013$Primary) # Korrelation zwischen Ausgaben für die Grundschulbildung und dem erreichten Bildungsindex


## Plots als Objekte
basic <- ggplot(edu_2013, aes(x = Primary, y = Index)) # Plot als Objekt abspeichern
basic + geom_point() # Objekt aufrufen


## Farben und Ästhetik
ggplot(edu_2013, aes(x = Primary, y = Index)) + geom_point(color = 'blue') # Farbe der Punkte zu Blau ändern
ggplot(edu_2013, aes(x = Primary, y = Index)) + geom_point(aes(color = Primary)) # Farbe der Punkte je nach Ausprägung auf Variable "Primary" bunt färben


## Gruppierte Abbildungen
ggplot(edu_2013, aes(x = Primary, y = Index)) + geom_point(aes(color = Region)) # Farbe der Punkte je nach Region einfärben
ggplot(edu_2013, aes(x = Primary, y = Index, color = Region)) + geom_point() # Allgemeine Gruppierung


## Faceting
edu_sel <- subset(edu_exp,  Year %in% c(1998, 2003, 2008, 2013)) # Datensatz mit Informationen aus 1998, 2003, 2008 und 2013 erstellen
edu_sel$Year <- as.factor(edu_sel$Year) # Variable in Faktor umwandeln

ggplot(edu_sel, aes(x = Primary, y = Index, color = Region, pch = Year)) + geom_point() # Plot

ggplot(edu_sel, aes(x = Primary, y = Index, color = Region)) + geom_point() + facet_wrap(~ Year) # Plot anhand des Jahres in Gruppen aufteilen



### Abbildungen anpassen

## Themes
scatter <- ggplot(edu_2013, aes(x = Primary, y = Index, color = Region)) + geom_point() # Grundanleitung der Abbildung für 2013 in Objekt ablegen

scatter + theme_minimal() # Theme verändern

install.packages('ggthemes') # Sammlung von Themes in ggplot
library(ggthemes)

theme_set(theme_minimal()) # Theme als neue Voreinstellung definieren
theme_set(theme_grey()) # ursprüngliche Voreinstellung


## Beschriftung
ggplot(edu_2013, aes(x = Primary, y = Index, color = Region)) +
  geom_point() +
  labs(x = 'Spending on Primary Eduction', # x-Achse beschriften
       y = 'UNDP Education Index', # y-Achse beschriften
       color = 'World Region') +
  ggtitle('Impact of Primary Education Investments', '(Data for 2013)') # Abbildungstitel und -untertitel


edu_2013$Region <- factor(edu_2013$Region, levels = c('africa', 'americas', 'asia', 'europe'), labels = c('Africa', 'Americas', 'Asia', 'Europe')) # Variable "Region" in Faktor umwandeln

scatter <- ggplot(edu_2013, aes(x = Primary, y = Index, color = Region)) +
  geom_point() +
  labs(x = 'Spending on Primary Eduction',
       y = 'UNDP Education Index',
       color = 'World Region') +
  ggtitle('Impact of Primary Education Investments', '(Data for 2013)')

scatter

## Farbpaletten
scatter + scale_color_grey() # Grautöne

gu_colors <- c('#00618f', '#e3ba0f', '#ad3b76', '#737c45', '#c96215') # Farben der Goethe Universität
scatter + scale_color_manual(values = gu_colors) # Farbpalette auf Objekt anwenden



### Verschiedene Plots
scatter + geom_smooth() # LOESS-Glättung auf scatter-Objekt anwenden

ggplot(edu_2013, aes(x = Primary, y = Index)) +
  geom_point(aes(color = Region)) +
  geom_smooth() +
  labs(x = 'Spending on Primary Eduction',
       y = 'UNDP Education Index',
       color = 'World Region') +
  ggtitle('Impact of Primary Education Investments', '(Data for 2013)')

scatter + geom_smooth(method = 'lm', se = FALSE) # Regressionen einfügen und Standardfehler unterdrücken
