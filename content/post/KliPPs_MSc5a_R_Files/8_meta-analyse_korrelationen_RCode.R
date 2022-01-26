#######################
### Meta-Analyse: Korrelationen
# von Julien P. Irmer


## Vorbereitung
library(metafor)

## Übersicht über den Datensatz verschaffen
head(dat.molloy2014)
summary(dat.molloy2014$ri)

## Grafische Veranschaulichung der Beziehung zwischen der Medikamenteneinnahme und der Gewissenhaftigkeit
boxplot(dat.molloy2014$ri)

## Fisher's z-Transformation
data_transformed <- escalc(measure="ZCOR",                # z-Transformation
                           ri=ri,                         # beobachtete Korrelationskoeffizienten
                           ni=ni,                         # Stichprobengröße pro Studie
                           data=dat.molloy2014,           # Datensatz
                           var.names = c("z_ri", "v_ri")) # Namen der neu zu erstellenden Variablen
head(data_transformed)

data_transformed_2 <- escalc(measure="ZCOR",              # z-Transformation
                           ri=dat.molloy2014$ri,          # beobachtete Korrelationskoeffizienten
                           ni=dat.molloy2014$ni,          # Stichprobengröße pro Studie
                           var.names = c("z_ri", "v_ri")) # Namen der neu zu erstellenden Variablen
head(data_transformed_2)

data_transformed$v_ri[1:4]     # die ersten 4 Einträge betrachten
1/(dat.molloy2014$ni - 3)[1:4]

plot(x = data_transformed$ri, y = data_transformed$z_ri,
     xlab = "r", ylab = "z",
     main = "Fisher's z-Transformation")


## Random Effects Model
REM <- rma(yi = z_ri, vi = v_ri, data=data_transformed)
summary(REM)

REM$b    # mittlere Schätzung b
REM$tau2 # tau²
predict(REM, transf=transf.ztor) # Retransformation

pred_REM <- predict(REM, transf=transf.ztor)
names(pred_REM)
pred_REM$pred # retransformierter gepoolter Korrelationskoeffizient


## Weitere Moderatoren und Psychometrische Metaanalysen
df <- data.frame(r = c(0.3, 0.3, 0.5, 0.4),
                 RelX = c(0.6, 0.8, 1, 1),
                 RelY = c(0.5, 0.7, 0.8, 1),
                 n = c(65, 65, 34, 46))
head(df)

df$r_correct <- df$r/sqrt(df$RelX*df$RelY) # Minderungskorrektur
head(df)
