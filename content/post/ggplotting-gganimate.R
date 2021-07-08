#### Syntax zu gganimate - Animierte Grafiken in ggplot ####
#### Volles Skript: https://pandar.netlify.app/post/ggplotting-gganimate/ ####

### Vorbereitende Schritte ----

# Daten laden
load(url('https://pandar.netlify.com/post/edu_exp.rda'))

# Wohlstand rekodieren
edu_exp$Wealth <- factor(edu_exp$Wealth,
  levels = c('high_income', 'upper_middle_income', 'lower_middle_income', 'low_income'),
  labels = c('High', 'Upper Mid.', 'Lower Mid.', 'Low'))

# Paket laden
library(ggplot2)

# Theme sourcen
source('https://pandar.netlify.com/post/ggplotting-theme-source.R')


### Infomrationsgewinn durch Animation ----

# Linienverlauf
static <-
  subset(sel, !is.na(Primary)) |>
  ggplot(aes(x = Year, y = Primary, color = Country)) +
  geom_point() + geom_line() +
  ggtitle('Spending on Primary Education') +
  theme_pandar() + scale_color_pandar()

fluid <- static + transition_reveal(Year) +
  ggtitle('Spending on Primary Education', 'Year {frame_along}')

animated <- animate(fluid,
  fps = 20,
  nframe = diff(range(sel$Year))*10,
  end_pause = 40,
  renderer = gifski_renderer(),
  height = 5, width = 7, units = 'in', res = 300)


# Traditioneller Plot
sel <- subset(edu_exp, geo %in% c('gbr', 'fra', 'ita', 'esp', 'pol'))
subset(sel, !is.na(Primary)) |>
  ggplot(aes(x = Year, y = Primary, color = Country)) +
  geom_point() + geom_line() +
  ggtitle('Spending on Primary Education') +
  theme_pandar() + scale_color_pandar()


### Statische Grundlage ----

# Wiederholter Scatterplot
subset(edu_exp, Year == 2013) |>
  ggplot(aes(x = Primary, y = Index, color = Wealth)) +
    geom_point() +
    labs(x = 'Spending on Primary Eduction',
    y = 'UNDP Education Index',
    color = 'Country Wealth\n(GDP per Capita)') +
    ggtitle('Impact of Primary Education Investments', subtitle = 'Data for 2013') +
    theme_pandar() + scale_color_pandar()

# Scatter alle Jahre
static <- ggplot(edu_exp, aes(x = Primary, y = Index, color = Wealth)) +
  geom_point() +
  labs(x = 'Spending on Primary Eduction',
  y = 'UNDP Education Index',
  color = 'Country Wealth\n(GDP per Capita)') +
  ggtitle('Impact of Primary Education Investments') +
  theme_pandar() + scale_color_pandar()
static

# Übersicht mit facet_wrap()
static + facet_wrap(~ Year, scales = 'free')

# Trim für Datenlage
trimmed <- subset(edu_exp, Year < 2017 & Year > 1997)
static <- ggplot(trimmed, aes(x = Primary, y = Index, color = Wealth)) +
  geom_point() +
  labs(x = 'Spending on Primary Eduction',
  y = 'UNDP Education Index',
  color = 'Country Wealth\n(GDP per Capita)') +
  ggtitle('Impact of Primary Education Investments') +
  theme_pandar() + scale_color_pandar()


### Grundidee von gganimate ----

# Spanien 2008 und 2009
subset(trimmed, Year %in% c(2008, 2009) & geo == 'esp')


### Animierter Scatterplot: Erster Versuch ----

# Pakete installieren
install.packages('gganimate')
install.packages('gifski')

# Paket laden
library('gganimate')

# Transition einfügen
fluid <- static + transition_time(Year)

# Animieren und speichern
animated <- animate(fluid)
anim_save('step1.gif', animated)


### Animierter Scatterplot: Mehr Versuche ----

# Daten aus Indien
subset(trimmed, geo == 'ind', select = c('Country', 'Wealth', 'Year', 'Primary', 'Index'))

# Missings entfernen
nomiss <- subset(trimmed, !(is.na(Primary) | is.na(Index)))

# Indien ohne missings
subset(nomiss, geo == 'ind', select = c('Country', 'Wealth', 'Year', 'Primary', 'Index'))

# Statischer Plot
static <- ggplot(nomiss, aes(x = Primary, y = Index, color = Wealth, group = Country)) +
  geom_point() +
  labs(x = 'Spending on Primary Eduction',
  y = 'UNDP Education Index',
  color = 'Country Wealth\n(GDP per Capita)') +
  ggtitle('Impact of Primary Education Investments') +
  theme_pandar() + scale_color_pandar()

# Animieren und speichern
fluid <- static + transition_time(Year)
animated <- animate(fluid)
anim_save('step2.gif', animated)

# Enter und Exit animieren
fluid <- static + transition_time(Year) +
  enter_fade(alpha = .1) + exit_fade(alpha = .1)

# Beispielframe inspizieren
plot(fluid, 2)

# Animieren und speichern
animated <- animate(fluid)
anim_save('step3.gif', animated)


### Feinschliff Funktionialität ----

# shadow_wake
fluid <- static + transition_time(Year) +
  enter_fade(alpha = .1) + exit_fade(alpha = .1) +
  shadow_wake(.5)

# Beispielframe inspizieren
plot(fluid, 44)

# Bouncy (elastic-in)
fluid <- static + transition_time(Year) +
  enter_fade(alpha = .1) + exit_fade(alpha = .1) +
  ease_aes('elastic-in')

# Achsenverschiebung
fluid <- static + transition_time(Year) +
  enter_fade(alpha = .1) + exit_fade(alpha = .1) +
  view_follow()

# Animieren und speichern
animated <- animate(fluid)
anim_save('step4.gif', animated)

# Zeitanzeige
fluid <- static + transition_time(Year) +
  enter_fade(alpha = .1) + exit_fade(alpha = .1) +
  ggtitle('Impact of Primary Education Investments', 'Year: {frame_time}')

# Animieren und speichern
animated <- animate(fluid)
anim_save('step5.gif', animated)


### Animations-Optionen

# Mehr frames (längeres gif)
animated <- animate(fluid,
  nframes = 200, fps = 10)

# Pause zu Beginn und Ende
animated <- animate(fluid,
  nframes = 200, fps = 10,
  start_pause = 20,
  end_pause = 20)

# Animieren und speichern
animated <- animate(fluid)
anim_save('step6.gif', animated)


### Bar Chart Races ----

# Rangplätze ermitteln
edu_exp$Index_Rank <- ave(-edu_exp$Index, edu_exp$Year, FUN = function(x) rank(x, ties.method = 'first'))

# Top 10 1997
subset(edu_exp, Index_Rank < 11 & Year == 1997)

# Regionen umkodieren
edu_exp$Region <- factor(edu_exp$Region,
  levels = c('europe', 'asia', 'americas', 'africa'),
  labels = c('Europe', 'Asia', 'Americas', 'Africa'))

# Top 10 auswählen
top10 <- subset(edu_exp, Index_Rank < 11)

# Statischer plot
static <- ggplot(top10,
  aes(y = Index_Rank, x = Index, fill = Region, group = Country))

# Erweiterung um Bars
static <- static +
  geom_rect(aes(ymin = Index_Rank - .4, ymax = Index_Rank + .4,
  xmin = 0, xmax = Index))

# Ländernamen hinzufügen
static <- static +
  geom_text(hjust = 'right', aes(label = Country), color = 'white')

# y-Achse umkehren (1. Platz oben) und Balken bis an die x-Achse ziehen
static <- static +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reverse()

# Titel und Label vergeben, Theme einführen, Beschriftung der y-Achse entfernen
static <- static +
  ggtitle('Top Spots in the Education Race') +
  labs(y = element_blank(), x = 'UNDP Education Index') +
  theme_pandar() + scale_fill_pandar(drop = FALSE) +
  theme(axis.text.y = element_blank())

# Vorschau im facet_wrap()
static + facet_wrap(~ Year, scales = 'free')

# Grundanimation
fluid <- static +
  transition_states(Year, transition_length = 30, state_length = 1,
    wrap = FALSE)

# Zeitangabe hinzufügen
fluid <- fluid +
  ggtitle('Top Spots in the Education Race', subtitle = 'Year: {closest_state}')

# Balken nach unten verschwinden lassen
fluid <- fluid +
  enter_drift(y_mod = -1) + exit_drift(y_mod = -1)

# Balken verblassen lassen
fluid <- fluid +
  enter_fade(alpha = .1) + exit_fade(alpha = .1)

# Beispielframe
plot(fluid, 12)

# Animieren und speichern
animated <- animate(fluid,
  fps = 20, duration = 15,
  end_pause = 20)
anim_save('barchartrace.gif', animated)
