#######################
### Grafiken mit ggplot2
# von Martin Schultze


##Datensätze laden
confirmed_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')

#Daten aufbereiten: von breites (Spalte pro Tag) in langes Format (Zeile pro Tag) übertragen
confirmed <- na.omit(confirmed_raw) #Datensatz ohne fehlende Werte anlegen

confirmed_long <- reshape(confirmed,                           #Datensatz
                          varying = names(confirmed)[-c(1:4)], #Variablen, die wiederholt gemessen wurden
                          v.names = 'Confirmed',               #Name, unter dem die Variablen zusammengefasst werden sollen
                          timevar = 'Day',                     #Variable, die Wiederholungen kennzeichnet
                          idvar = names(confirmed)[1:4],       #Variablen, die sich über Wiederholungen nicht ändern
                          direction = 'long')                  #Zielformat des neuen Datensatzes

head(confirmed_long) #Überblick über neuen, "langen" Datensatz verschaffen
rownames(confirmed_long) <- NULL #Variablennamen entfernen
head(confirmed_long)

#analog für anderen Datensatz
deaths <- na.omit(deaths_raw) #Datensatz ohne fehlende Werte anlegen

deaths_long <- reshape(deaths,                              #Datensatz
                          varying = names(deaths)[-c(1:4)], #Variablen, die wiederholt gemessen wurden
                          v.names = 'Deaths',               #Name, unter dem die Variablen zusammengefasst werden sollen
                          timevar = 'Day',                  #Variable, die Wiederholungen kennzeichnet
                          idvar = names(deaths)[1:4],       #Variablen, die sich über Wiederholungen nicht ändern
                          direction = 'long')               #Zielformat des neuen Datensatzes

rownames(deaths_long) <- NULL
head(deaths_long)

##Daten zusammenführen (merge)
long <- merge(confirmed_long, deaths_long,                                      #Datensätze, die zusammengeführt werden sollen
              by = c('Province.State', 'Country.Region', 'Lat', 'Long', 'Day')) #Variablen, anhand derer gleiche Fälle identifiziert werden können
head(long)

##Daten zusammenfassen (aggregate)
covid <- aggregate(cbind(Confirmed, Deaths) ~ Country.Region + Day, #AVs ~ UV1 + UV2
                   data = long,                                     #Datensatz
                   FUN = 'sum')                                     #Summe bilden über verschiedene Staaten/Provinzen/Regionen eines Landes
head(covid)

tail(covid[covid$Country.Region == 'Germany', ], 10) #Daten der letzten 10 Tage für Deutschland



###ggplot2-Grundprinzipien
library(ggplot2)


##Schichten
covid_de <- covid[covid$Country.Region == 'Germany', ] #neuen Datensatz erstellen, der nur die Daten für Deutschland beinhaltet

ggplot(covid_de,                       #Datensatz
       aes(x = Day, y = Confirmed)) +  #Variablen für x- und y-Achse bestimmen
  geom_line() +                        #geometrische Form: Linie
  geom_point()                         #geometrische Form: Punkte

basic <- ggplot(covid_de, aes(x = Day, y = Confirmed)) #Anleitung für Erstellung der Grafik erstellen
basic + geom_point()


##Farben und Ästhetik
ggplot(covid_de, aes(x = Day, y = Confirmed)) +
  geom_point(color = 'blue') #alle Datenpunkte werden blau

ggplot(covid_de, aes(x = Day, y = Confirmed)) +
  geom_point(aes(color = Confirmed)) #Farbe soll von der Ausprägung der Variable "Confirmed" abhängig gemacht werden


##Gruppierte Abbildungen
covid_sel <- covid[covid$Country.Region %in% c('France', 'Germany', 'Italy', 'Spain', 'United Kingdom'), ] #Datensatz mit 5 Ländern

ggplot(covid_sel, aes(x = Day, y = Confirmed, color = Country.Region)) + #jedem Land wird eine Farbe zugeordnet
  geom_point() + geom_line()                                             #Ästhetik wird für alle Gruppierungen übernommen


##Faceting
#eine Abbildung wird anhand von Ausprägungen auf einer oder mehr Variablen in verschiedene Abbildungen unterteilt
ggplot(covid_sel, aes(x = Day, y = Confirmed)) +
  geom_point() + geom_line() +
  facet_wrap(~ Country.Region)                   #Plot wird anhand der unabhängigen Variablen hinter der Tilde in Gruppen eingeteilt


##mehrere variablen
ggplot(covid_de, aes(x = Day)) +
  geom_line(aes(y = Confirmed), color = 'darkblue') + #erste Variable
  geom_line(aes(y = Deaths), color = 'darkred')       #zweite Variable



###Abbildungen anpassen

##Themes
ggplot(covid_sel, aes(x = Day, y = Confirmed, color = Country.Region)) +
  geom_line() + geom_point() +
  theme_light() #Theme hinzufügen

##Beschriftung
ggplot(covid_sel, aes(x = Day, y = Confirmed, color = Country.Region)) +
  geom_line() + geom_point() +
  theme_light() +
  labs(x = 'Tage seit dem 22.1.', y = 'Bestätigte Fälle', color = 'Land') + #Achsenbeschriftungen
  ggtitle('COVID-19 Infektionen',                                           #Titel
          paste('Stand:', Sys.Date()))                                      #Untertitel: hier wird über `Sys.Date()` das aktuelle Datum abgefragt und durch `paste` mit "Stand:" zusammengeklebt

##Farbpaletten
ggplot(covid_sel, aes(x = Day, y = Confirmed, color = Country.Region)) +
  geom_line() + geom_point() +
  theme_light() +
  scale_color_grey() #Grautöne festlegen

gu_colors <- c('#00618f', '#e3ba0f', '#ad3b76', '#737c45', '#c96215') #Farben in einem Objekt festlegen
ggplot(covid_sel, aes(x = Day, y = Confirmed, color = Country.Region)) +
  geom_line() + geom_point() +
  theme_light() +
  scale_color_manual(values = gu_colors) #selbsständig Farben über Objekt zuweisen



###verschiedene Plots


##Balkendiagramme
covid_today <- covid_sel[covid_sel$Day == max(covid_sel$Day), ] #neuen Datensatz mit nur den heutigen Fallzahlen
covid_today

ggplot(covid_today, aes(x = Country.Region, y = Confirmed)) +
  geom_bar(stat = 'identity') #geometrische Form: Balken #als zu berechnende Statistik Variable "identity" benutzen


##Histogramme
covid_global <- covid[covid$Day == max(covid$Day), ] #Datensatz mit Anzahl der bestätigten Fälle bisher über die Länder hinweg
ggplot(covid_global, aes(x = Confirmed)) +
  geom_histogram() #geometrische Form: Histogramm


##Boxplots
ggplot(covid_global, aes(y = Confirmed)) +
  geom_boxplot() #geometrische Form: Boxplot


##Plots mit Trendlinien
ggplot(covid_sel, aes(x = Day, y = Confirmed)) +
  geom_point() + #geometrische Form: Scatterplot
  geom_smooth()  #geometrische Form: globale Trendlinie

ggplot(covid_sel, aes(x = Day, y = Confirmed, color = Country.Region)) + #Trends sollen länderspezifisch eingezeichnet werden
  geom_point() +
  geom_smooth() #länderspezifische Trends
