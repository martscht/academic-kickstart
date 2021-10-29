#######################
### ANCOVA und Moderierte Regression
# von Julien P. Irmer


## Daten laden
osf <- read.csv(file = url("https://osf.io/zc8ut/download"))
osf <- osf[, c("ID", "group", "stratum", "bsi_post", "swls_post", "pas_post")]

# Missings ausschließen
missings_ind <- which(is.na(osf$pas_post))
osf <- osf[-missings_ind, ]
head(osf) # finaler Datensatz



## Vorbereitung

# Skalenniveaus anpassen: Factors bilden
osf$group <- factor(osf$group)
osf$stratum <- factor(osf$stratum)

# Zentrieren
osf$swls_post <- scale(osf$swls_post, center = T, scale = F)
osf$pas_post <- scale(osf$pas_post, center = T, scale = F)



## Kovarianzanalyse: ANCOVA

# Regressionsmodell
reg_swl <- lm(bsi_post ~ 1 + swls_post, data = osf) # Symptomschwere durch Lebenszufriedenheit vorhersagen
summary(reg_swl) # Ergebnisse der Regressionsanalyse

# ANCOVA
reg_ancova <- lm(bsi_post  ~  1 + group + swls_post, data = osf) # Gruppierungsvariable hinzufügen
summary(reg_ancova)

library(car)
Anova(reg_ancova) # gesammelte Signifikanzentscheidungen pro Variable


### Generalisierte ANCOVA

# Treatmenteffekt
reg_gen_ancova <- lm(bsi_post  ~  1 + group + swls_post  + group:swls_post,
                     data = osf)
summary(reg_gen_ancova)
Anova(reg_gen_ancova, type = 2) # Alternative

# Diagnoseeffekt
reg_gen_ancova_s <- lm(bsi_post ~ stratum + swls_post + stratum:swls_post, data = osf)
Anova(reg_gen_ancova_s)



## Moderierte Regression

# Modell
mod_reg <- lm(bsi_post ~ swls_post + pas_post + swls_post:pas_post, data = osf)
summary(mod_reg)

# Simple-Slopes
library(interactions)
interact_plot(model = mod_reg, pred = pas_post, modx = swls_post)


### Absicherung gegen quadratische Effekte und Multikollinearität

# Quadratische Effekte in das Regressionsmodell aufnehmen
mod_quad_reg <- lm(bsi_post ~ swls_post + pas_post + swls_post:pas_post + I(swls_post^2) + I(pas_post^2), data = osf)
summary(mod_quad_reg)

# Simple-Slopes
interact_plot(model = mod_quad_reg, pred = pas_post, modx = swls_post)


### Umgang mit quadratischen und Interaktionswerten

# Interaktionseffekte in das Regressionsmodell aufnehmen
quad_reg <-  lm(bsi_post ~ swls_post + pas_post  + I(swls_post^2) + I(pas_post^2), data = osf)
anova(quad_reg, mod_quad_reg)


