# ---- Regression mit nominalskalierten Prädiktoren ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBSc7_R_Files/13_reg6.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Der Autor dieses Skripts ist Martin Schultze. Skriptkompilierung von Kevin Pommeranz.

# Datensatz laden
source("https://pandar.netlify.app/post/Preprocessing/Data_Processing_punish.R")

head(punish)

#### Regresssion mit Dummy-Kodierung ----
mod1 <- lm(severe ~ country, punish)
summary(mod1)

class(punish$country)

contrasts(punish$country)

# Gruppenmittelwerte
tapply(punish$severe, punish$country, mean)

# Koeffizienten
coef(mod1)[1] # USA
coef(mod1)[1] + coef(mod1)[2] # China

t.test(punish$severe ~ punish$country, var.equal = TRUE)

#### Effektkodierung ----

contrasts(punish$country) <- contr.sum(2)

contrasts(punish$country)

mod1b <- lm(severe ~ country, punish)
summary(mod1b)

# Mittel der Gruppenmittelwerte
tapply(punish$severe, punish$country, mean) |> mean()

#### Kombination dichotomer Prädiktoren ----

contrasts(punish$country) <- contr.treatment(2)
dimnames(contrasts(punish$country))[[2]] <- 'China'

tab <- data.frame(country = c('U.S', 'U.S', 'China', 'China'),
  bribe = c('group', 'individual', 'group', 'individual'))

tab$mod1 <- predict(mod1, tab)
tab

library(ggplot2)

pred_plot <- ggplot(tab, aes(x = bribe, 
  group = country, color = country)) + 
  geom_point(aes(y = mod1)) + geom_line(aes(y = mod1)) +
  theme_minimal() +
  ylim(c(4, 5.5))

pred_plot

mod2 <- lm(severe ~ country + bribe, punish)
summary(mod2)

tab$mod2 <- predict(mod2, tab)

pred_plot <- pred_plot +
  geom_point(aes(y = tab$mod2)) + geom_line(aes(y = tab$mod2), lty = 2)
pred_plot

# Übersicht über die Vorhersagten Werte
tab

# Unterschied der Bestechungstypen in den beiden Ländern
tab$mod2[2] - tab$mod2[1]
tab$mod2[4] - tab$mod2[3] # identisch


mod3 <- lm(severe ~ country + bribe + country:bribe, punish)
summary(mod3)

tab$mod3 <- predict(mod3, tab)

pred_plot <- pred_plot +
  geom_point(aes(y = tab$mod3)) + geom_line(aes(y = tab$mod3), lty = 3)
pred_plot

tab

summary(mod3)$coefficients

## Modellvergleiche

anova(mod1, mod2, mod3)

# Deskriptiv
summary(mod2)$r.squared - summary(mod1)$r.squared
summary(mod3)$r.squared - summary(mod2)$r.squared

#### Nominalskalierte Prädiktoren mit mehr als zwei Kategorien

table(punish$age)

mod4 <- lm(severe ~ age, punish)
summary(mod4)

contrasts(punish$age)

tab <- data.frame(age = levels(punish$age))
tab$mod4 <- predict(mod4, tab)
tab

#### Kombination nominal- und intervallskalierter Prädiktoren ----

usa <- subset(punish, country == 'U.S')

mod5 <- lm(severe ~ gains, usa)
summary(mod5)

mod6 <- lm(severe ~ gains + bribe, usa)
summary(mod6)

# Scatterplot erstellen
scatter <- ggplot(usa, aes(x = gains, y = severe, color = bribe)) + 
  geom_point()

# Regressionsgerade aus mod6 hinzufügen
scatter + 
  # Kollektive Bestechung
  geom_abline(intercept = coef(mod6)[1], slope = coef(mod6)[2], 
    color = '#F8766D') +
  # Individuelle Bestechung
  geom_abline(intercept = coef(mod6)[1] + coef(mod6)[3], slope = coef(mod6)[2], 
    color = '#00BFC4')

mod7 <- lm(severe ~ gains + bribe + gains:bribe, usa)
summary(mod7)

scatter + 
  # Kollektive Bestechung
  geom_abline(intercept = coef(mod7)[1], slope = coef(mod7)[2], 
    color = '#F8766D') +
  # Individuelle Bestechung
  geom_abline(intercept = coef(mod7)[1] + coef(mod7)[3], slope = coef(mod7)[2] + coef(mod7)[4], 
    color = '#00BFC4')

library(reghelper)
simple_slopes(mod7)
