#### Syntax zum ggplotting Intro ####
#### Volles Skript: https://pandar.netlify.app/post/ggplotting-daten/ ####


### Daten von Gapminder ----

# dplyr laden
library(dplyr)

# Geografische Informationen
raw_geo <- read.csv('https://github.com/open-numbers/ddf--gapminder--systema_globalis/raw/master/ddf--entities--geo--country.csv')

# Populationsdaten
pop <- read.csv('https://github.com/open-numbers/ddf--gapminder--systema_globalis/raw/master/countries-etc-datapoints/ddf--datapoints--population_total--by--geo--time.csv')

# Lebenserwartung
expect <- read.csv('https://github.com/open-numbers/ddf--gapminder--systema_globalis/raw/master/countries-etc-datapoints/ddf--datapoints--life_expectancy_at_birth_with_projections--by--geo--time.csv')

# Einkommen (GDP / Person)
income <- read.csv('https://github.com/open-numbers/ddf--gapminder--systema_globalis/raw/master/countries-etc-datapoints/ddf--datapoints--income_per_person_gdppercapita_ppp_inflation_adjusted--by--geo--time.csv')

# Investition in Bildung (nach Bildungsstufe getrennt)
primary <- read.csv('https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/countries-etc-datapoints/ddf--datapoints--expenditure_per_student_primary_percent_of_gdp_per_person--by--geo--time.csv')
secondary <- read.csv('https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/countries-etc-datapoints/ddf--datapoints--expenditure_per_student_secondary_percent_of_gdp_per_person--by--geo--time.csv')
tertiary <- read.csv('https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/countries-etc-datapoints/ddf--datapoints--expenditure_per_student_tertiary_percent_of_gdp_per_person--by--geo--time.csv')


#### Verschiedene Informationen zusammenführen ----

# Geografische Daten kürzen
geo <- transmute(raw_geo, geo = country, Country = name, Wealth = income_groups, Region = world_4region)
names(geo)

# Geografische und demografische Daten zusammenführen
geo <- right_join(geo, pop, by = 'geo') |>
  full_join(expect, by = c('geo', 'time')) |>
  full_join(income, by = c('geo', 'time'))

# Bildungsausgaben zusammenführen
edu <- full_join(primary, secondary, by = c('geo', 'time')) |>
  full_join(tertiary, by = c('geo', 'time'))

# Gesamtdatensatz erstellen
edu_exp <- full_join(geo, edu, by = c('geo', 'time'))

# Daten einschränken und leere Zeilen entfernen
edu_exp <- filter(edu_exp, time < 2019 & time > 1996) |>
  filter(!is.na(Country))


# Variablen umbenennen
names(edu_exp)
names(edu_exp)[-c(1:4)] <- c('Year', 'Population', 'Expectancy', 'Income', 'Primary', 'Secondary', 'Tertiary')


### Überblick über die finalen Daten ----

head(edu_exp)

