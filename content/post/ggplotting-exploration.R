#### Workshop ggplotting, Juli 2021
#### Martin Schultze & Janine Buchholz
#### Material: https://pandar.netlify.app/extras/#ggplotting



### Vorbereitung ----
# Pakete installieren, Datensatz laden

# Pakete
# install.packages(c("ggplot2", "datasauRus", "GGally", "tabplot"))

library(ggplot2)

# Datensatz
load(url('https://pandar.netlify.com/post/edu_exp.rda'))

# Daten ansehen
head(edu_exp)




### Faceting ----

# facet_grid
edu_exp |>
  subset(Year == 2016) |>
  ggplot(aes(x=Income, y=Expectancy)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  facet_grid(. ~ Region)

# Der (lustige!) Datensatz
library(datasauRus)
str(datasaurus_dozen)

# Grafik ohne faceting:
ggplot(datasaurus_dozen, aes(x=x, y=y)) +
  geom_point()

# Grafik mit faceting:
ggplot(datasaurus_dozen, aes(x=x, y=y)) +
  geom_point() +
  facet_wrap(~dataset)

# Grafik mit faceting und Farbe
ggplot(datasaurus_dozen, aes(x=x, y=y)) +
  geom_point(aes(colour=dataset), show.legend=F) +
  facet_wrap(~dataset)




### Funktion ----
# Effizientes Erstellen einer Serie von ggplot-Grafiken


gm.plot <- function(which.country, show.mean=FALSE){

  dipfblau <- rgb(102,153,204, max=255)
  tmp.data <- subset(edu_exp, Country == which.country)
  tmp.mw   <- mean(tmp.data$Expectancy)
  
  tmp.plot <- ggplot(tmp.data, aes(x=Year, y=Expectancy)) +
    geom_line(size=1.5, show.legend=F) +
    geom_point(size=2, show.legend=F) +
    xlim(1997, 2017) +
    ylim(20, 85) +
    labs(x="Jahr", y="Lebenserwartung", 
         title=paste0("Lebenserwartung in ", which.country, " im Zeitverlauf"),
         subtitle=paste0("Kontinent: ", tmp.data$Region[1]))

  if(show.mean==TRUE){
    tmp.plot <- tmp.plot +
      geom_hline(aes(yintercept=tmp.mw), lty=2, col=dipfblau) +
      annotate("text", 1960, tmp.mw+3, fontface='italic',
               label=paste0("Mittelwert = ", round(tmp.mw, 2)), col=dipfblau)
  }
  return(tmp.plot)
}

# Funktion pruefen
gm.plot("Germany", TRUE)

# Anwenden der Funktion auf alle Elemente des Vektors "countries"
dir.create("./gapminder-Laender-Plots")
countries <- unique(edu_exp$Country)
countries

for(c in 1:length(countries)){
  gm.plot(countries[c], show.mean = TRUE)
  ggsave(paste0("./gapminder-Laender-Plots/Plot-", countries[c], ".png"),
                width=24, height=12, units="cm", dpi=200)
  print(paste0("Grafik erstellt fuer: ", countries[c], " (", c, "/", length(countries), ")"))
}




### ggpairs ----

library(GGally)

# Der Arbeitsdatensatztz: Auswahl von Variablen
edu_exp_sel <- edu_exp[, c("Income", "Expectancy", "Index", "Region")]

# Die Funktion ggpairs()
library(GGally)
ggpairs(edu_exp_sel, columns = 1:3)
ggpairs(edu_exp_sel, columns = 1:3, aes(color = Region, alpha = .5))



### tableplot ----

# library(devtools)
# install_github("mtennekes/tabplot")
library(tabplot)

# Teildatensatz mit wenigen Variablen erzeugen
edu_exp_2 <- subset(edu_exp, select = c("Country", "Region", "Index", "Expectancy", "Population", "Income"))

# Default: Alle Variablen enthalten, sortiert nach der ersten (hier nicht sehr sinnvoll)
tableplot(edu_exp_2)


# Default-Darstellung mit sinnvoller Variablenauswahl
tableplot(edu_exp_2,
          select=c(Region, Index, Expectancy, Income))

# Sortiert nach Income
tableplot(edu_exp_2,
          select=c(Region, Index, Expectancy, Income), 
          sortCol = Income)

# Sortiert nach Lebenserwartung, reinzoomen in die "oberen" 20%
tableplot(edu_exp_2,
          select=c(Region, Index, Expectancy, Income), 
          sortCol = Expectancy,
          from = 0, to = 20)


