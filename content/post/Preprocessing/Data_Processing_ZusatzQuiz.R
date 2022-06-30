### Datensatz: Corona-Pandemie 2020
confirmed_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')

### Long format ---
missings <- which(apply(confirmed_raw[,1:4],1,function(x) any(is.na(x)))) # find out, if any meta data are not complete
confirmed <- confirmed_raw[-missings,] # remove incomplete data
confirmed_long <- reshape(confirmed,
  varying = names(confirmed)[-c(1:4)],
  v.names = 'Confirmed',
  timevar = 'Day',
  idvar = names(confirmed)[1:4],
  direction = 'long', ids = 1:prod(dim(confirmed)))

missings <- which(apply(deaths_raw[,1:4],1,function(x) any(is.na(x)))) # find out, if any meta data are not complete
deaths <- deaths_raw[-missings,] # remove incomplete data
deaths_long <- reshape(deaths,
  varying = names(deaths)[-c(1:4)],
  v.names = 'Deaths',
  timevar = 'Day',
  idvar = names(deaths)[1:4],
  direction = 'long')

### Merged data ----
long <- merge(confirmed_long, deaths_long,
  by = c('Province.State', 'Country.Region', 'Lat', 'Long', 'Day'))

### Full data ----
covid <- aggregate(cbind(Confirmed, Deaths) ~ Country.Region + Day, data = long, FUN = 'sum')

### Only data until Day 100 ----
covid <- covid[covid$Day < 101, ]

### Subsets ----
covid <- covid[covid$Country.Region %in% c('France', 'Germany', 'Italy', 'Spain', 'United Kingdom'), ]

rm(confirmed, confirmed_long, confirmed_raw, deaths, deaths_long, deaths_raw, long, missings)
