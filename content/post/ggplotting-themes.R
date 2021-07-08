#### Syntax zu Hübscheren Grafiken mit ggplot ####
#### Volles Skript: https://pandar.netlify.app/post/ggplotting-themes/ ####


### Vorbereitung ----

# Daten laden
load(url('https://pandar.netlify.com/post/edu_exp.rda'))

# Paket laden
library(ggplot2)

# Einfacher Scatterplot
edu_2013 <- subset(edu_exp, Year == 2013)

ggplot(edu_2013, aes(x = Primary, y = Index, color = Wealth)) +
  geom_point()


### Legende ----

# Ausprägungen der Wohlstandsvariable
unique(edu_2013$Wealth)

# Umkodierung
edu_2013$Wealth <- factor(edu_2013$Wealth,
  levels = c('high_income', 'upper_middle_income', 'lower_middle_income', 'low_income'),
  labels = c('High', 'Upper Mid.', 'Lower Mid.', 'Low'))

# Neuer Scatterplot
ggplot(edu_2013, aes(x = Primary, y = Index, color = Wealth)) +
  geom_point()

# Ohne fehlende Were
subset(edu_2013, !is.na(Wealth)) |>
  ggplot(aes(x = Primary, y = Index, color = Wealth)) +
    geom_point()


### Beschriftung ----

# labs() und ggtitle()
scatter <- ggplot(edu_2013, aes(x = Primary, y = Index, color = Wealth)) +
  geom_point() +
  labs(x = 'Spending on Primary Eduction',
    y = 'UNDP Education Index',
    color = 'Country Wealth\n(GDP per Capita)') +
  ggtitle('Impact of Primary Education Investments', '(Data for 2013)')

scatter


### Vorgefertigte Themes ----

# minmal
scatter + theme_minimal()

# ggthemes verwenden
install.packages('ggthemes')
library(ggthemes)

# Ganz in Tuftes Sinn
scatter + theme_tufte()

# Im Stil von 538
scatter + theme_fivethirtyeight()

# Global Theme festlegen
theme_set(theme_minimal())

# Zum zurücksetzen des Themes (nicht ausgeführt)
## theme_set(theme_grey())


### Elemente Anpassen ----

# Argumente von element_text()
args(element_text)

# Scatter mit neuem Titel
scatter +
  theme(plot.title = element_text(size = 18, hjust = .5))

# Scatter mit neuem Untertitel
scatter +
  theme(plot.title = element_text(size = 18, hjust = .5),
    plot.subtitle = element_text(hjust = .5))

# Achsen einzeichnen
scatter +
  theme(plot.title = element_text(size = 18, hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.line = element_line(color = 'black'))


### Eigene Themes ----

# Aktuelles, globales Theme anpassen
theme_update(plot.title = element_text(size = 18, hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.line = element_line(color = 'black'))

# Scatterplot ausführen
scatter

# Theme definieren
theme_pandar <- function() {
  theme_minimal() %+replace%
    theme(plot.title = element_text(size = 18, hjust = .5),
      plot.subtitle = element_text(hjust = .5),
      axis.line = element_line(color = 'black'))
}

# Original Layout
scatter + theme_grey()

# Eigene Theme
scatter + theme_pandar()

# Eigene Theme global festlegen
theme_set(theme_pandar())

### Farben ----

# Kontinuierliche Färbugn
ggplot(edu_2013, aes(x = Primary, y = Index, color = log(Income))) +
  geom_point()

# Greyscale
scatter + scale_color_grey()

# Pandar-Farben
pandar_colors <- c('#00618f',  '#737c45', '#e3ba0f', '#ad3b76')

# Eigene Farben einsetzen
scatter +
  scale_color_manual(values = pandar_colors)


### Eigene Farbpalette ----

# Eigene Palette definieren
scale_color_pandar <- function(discrete = TRUE, ...) {
  pal <- colorRampPalette(pandar_colors)
  if (discrete) {
    discrete_scale('color', 'pandar_colors', palette = pal, ...)
  } else {
    scale_color_gradientn(colors = pal(4), ...)
  }
}

# und einsetzen
scatter + scale_color_pandar()

# binäre Wohlstandsvariable
edu_2013$Wealth_bin <- edu_2013$Wealth
levels(edu_2013$Wealth_bin) <- list('High' = c('High', 'Upper Mid.'),
  'Low' = c('Lower Mid.', 'Low'))

# Anwendung der Palette auf weniger Ausprägungen
ggplot(edu_2013, aes(x = Primary, y = Index, color = Wealth_bin)) +
  geom_point() +
  scale_color_pandar()

# Anwendung der Palette auf mehr Ausprägungen
ggplot(edu_2013, aes(x = Primary, y = Index, color = log(Income))) +
  geom_point() +
  scale_color_pandar(discrete = FALSE)

