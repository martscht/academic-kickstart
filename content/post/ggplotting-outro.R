#### Workshop ggplotting, Juli 2021
#### Martin Schultze & Janine Buchholz
#### Material: https://pandar.netlify.app/extras/#ggplotting



### Vorbereitung ----
# Pakete installieren, Datensatz laden

# Pakete
# install.packages(c("ggplot2", "gridExtra", "dplyr", "FSA", "viridis))

library(ggplot2)

# Datensatz
load(url('https://pandar.netlify.com/post/edu_exp.rda'))

# Daten ansehen
head(edu_exp)



### Grafiken anordnen mit grid.arrange() ----


#install.packages("gridExtra")
library(gridExtra)

p.histo <- edu_exp |> 
  subset(Region=="asia" & Year == 2016) |> 
  ggplot(aes(x=Income)) +
  geom_histogram() +
  labs(title = "Income in Asia (2016)")

p.scatter <- edu_exp |> 
  subset(Region=="asia" & Year == 2016) |> 
  ggplot(aes(x=Income, y=Expectancy)) +
  geom_point() +
  geom_smooth() +
  labs(title = "GDP and life expectancy (Asia, 2016)")

grid.arrange(p.histo, p.scatter, nrow=2)
grid.arrange(p.histo, p.scatter, ncol=2)

ggsave("C:/Workshops/R/zwei-scatterplots.png",
       plot = grid.arrange(p.histo, p.scatter, ncol = 1))



### Ueberlappende Datenpunkte vermeiden ----

# alpha
########

# Problem: ueberlappende Punkte
ggplot(edu_exp, aes(x=Income, y=Expectancy)) +
  geom_point(aes(colour = Region, size = Population), show.legend = FALSE) 

# Transparenz mit dem Argument alpha
ggplot(edu_exp, aes(x=Income, y=Expectancy)) +
  geom_point(aes(colour = Region, size = Population), show.legend = FALSE, alpha = 0.1) 


# jitter: mtcars-Beispiel
#########################

# Problem: Ueberlappende Punkte mit identischen Werten 
ggplot(mtcars, aes(x=cyl, y=gear)) +
  geom_point() 

# geom_jitter mit den Default-Einstellungen
ggplot(mtcars, aes(x=cyl, y=gear)) +
  geom_point() + 
  geom_jitter()

# geom_jitter mit Kontrolle des Ausma? der Streuung
ggplot(mtcars, aes(x=cyl, y=gear)) +
  geom_point() + 
  geom_jitter(width=.2, height=.2)



# jitter: violin-plot
#####################

library(dplyr)
library(FSA)
library(viridis)

# neuer Datensatz mit Deskriptivstatistiken
plotdat_desc <- edu_exp %>% 
  filter(Year == 2016) %>% 
  group_by(Region) %>%
  summarize(
    "mean" = mean(Income, na.rm=T),
    "se" = se(Income, na.rm=T),
    "min" = min(Income, na.rm=T),
    "max" = max(Income, na.rm=T),
    "sd" = sd(Income, na.rm=T)
  ) %>%
  arrange(mean) %>%
  mutate(order = as.factor(1:n()))

# Originaldatensatz und Deskriptivstatistiken zusammenführen
usedat <- edu_exp %>%
  filter(Year == 2016) %>% 
  right_join(., plotdat_desc, by = "Region")

# Violin-Plot
ggplot(data = plotdat_desc, aes(x = order, y = mean, fill=order)) +
  geom_violin(
    data= usedat, 
    aes(x = order, y = Income, colour=order), 
    size=1, adjust = 2, draw_quantiles = c(.25, .75), alpha=.3)+
  geom_jitter(
    data = usedat, 
    aes(x = order, y = Income), 
    height = 0, width = .2, 
    size = 2, alpha=.2, fill= plasma(1)) +
  geom_errorbar( aes(ymax= mean+(1.96*se), ymin=mean+(-1.96*se)), size=1, width = 0.3, colour=plasma(1)) +  
  geom_point(size = 4, colour=plasma(1), shape=21)+
  theme_classic() +
  theme(
    plot.title = element_text(size=22, hjust=.5),
    axis.title = element_text(size=18),
    axis.text = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position = "none") +
  scale_x_discrete(labels=plotdat_desc$Region, name = "Region") +
  scale_y_continuous(labels=scales::comma, name = "Income (GDP/person)") +
  scale_fill_viridis_d(option="plasma") +
  scale_colour_viridis_d(option="plasma")  +
  labs(title = "Comparison of Income in 2016")



### Effizientes Arbeiten im ggplot-Befehl ----

ggplot(edu_exp, aes(x=Income, y=Primary)) +
  geom_point() +
  ggtitle("Interesting Plot") +
  # theme_classic() +
  NULL


### plotfetti :-) ----

set.seed(1)
data.frame(id = as.factor(1:100), x = rnorm(100), y = rnorm(100)) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(col = id), size = 5, alpha = .4, show.legend = FALSE) + 
  theme_void()


