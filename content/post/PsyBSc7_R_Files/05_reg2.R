# ---- Regressionsanalyse II ----
#Dieses Skript stammt von https://pandar.netlify.app/post/PsyBsc7_R_Files/05_reg2.R, von der PandaR-Website der Goethe Universität Frankfurt.
#Die Autoren dieses Skripts sind Julien P. Irmer, Johannes Hartig & Johanna Schüller. Skriptkompilierung von Kevin Pommeranz.

# Datensatz laden
load(url("https://pandar.netlify.app/post/Schulleistungen.rda"))

m.c <- lm(math ~ reading + female, data = Schulleistungen)      # constrained
m.u <- lm(math ~ reading + female + IQ, data = Schulleistungen) # unconstrained
summary(m.c)
summary(m.u)

# Inkrement = Differenz in R2 aus restringiertem Modell 2 minus R2 aus unrestringiertem Modell 1
summary(m.u)$r.squared - summary(m.c)$r.squared

# Modellvergleich mit der anova-Funktion
anova(m.c, m.u)

R2.u <- summary(m.u)$r.squared
R2.c <- summary(m.c)$r.squared
df.diff <- summary(m.u)$df[1] - summary(m.c)$df[1] # Änderung in den df
df.e <- summary(m.u)$df[2] # Fehlerfreiheitsgrade des uneingeschränkten Modells
F.diff <- ((R2.u - R2.c) / df.diff) /
  ((1 - R2.u) / df.e)
p.diff <- 1-pf(F.diff, df.diff, df.e)
F.diff # F-Wert der Differenz in R^2
p.diff # zugehöriger p-Wert  

R2.u - R2.c # Inkrement
library(ppcor)
sp <- spcor.test(x = Schulleistungen$math, y = Schulleistungen$IQ, z = Schulleistungen[, c("reading", "female")])
sp
sp$estimate^2 # ebenfalls Inkrement!

m.u <- lm(math ~ reading + female + IQ, data = Schulleistungen) # unconstrained
m.c <- lm(math ~ reading + IQ, data = Schulleistungen) # constrained

summary(m.u)$r.squared - summary(m.c)$r.squared
# Modellvergleich mit der anova-Funktion
anova(m.c, m.u)

# Modell mit allen Prädiktoren
m <- lm(math ~ reading + female + IQ, data = Schulleistungen)
# Optimierung
summary(step(m, direction = "both"))

out <- summary(step(m)) |> capture.output()
begin <- "Start"; end <- "Df"
out[grep(pattern = begin, out):(grep(pattern = end, out)-1)] |> paste(collapse = "\n") |> cat()

begin <- "Df"; end <- "Step"
out[grep(pattern = begin, out):(grep(pattern = end, out)-1)] |> paste(collapse = "\n") |> cat()

begin <- "Step"; end <- "Call"
out[grep(pattern = begin, out):(grep(pattern = end, out)-1)] |> paste(collapse = "\n") |> cat()

# Optimierung mit BIC
summary(step(m, direction = "both", k=log(nrow(Schulleistungen))))

# install.packages("olsrr")
library(olsrr)
ols_step_both_p(m, pent = .05, prem = .10, details = TRUE)

model <- lm(math ~ reading + female, data = Schulleistungen)
AIC(model)
extractAIC(model) # erstes Argument ist die Anzahl der Parameter (p)

logLik(model)

LL <- logLik(model) # Loglikelihood des Modells
p <- length(coef(model))+1 # betas + sigma
n <- nrow(Schulleistungen) # Stichprobengröße (nur so möglich, wenn keine Missings!)
sigma <-summary(model)$sigma * sqrt((n-3)/n) # Korrektur um die Freiheitsgrade df_e = n - (Anzahl beta-Gewichte)
LL
-n/2*log(2*pi) - n*log(sigma) - n/2

myAIC <- -2*LL[1] + 2*p
myAIC
AIC(model)

extractAIC(model)[2] + n + n*log(2*pi) + 2
myAIC
AIC(model)

model1 <- lm(math ~ reading + female, data = Schulleistungen)
model2 <- lm(math ~ reading , data = Schulleistungen)

AIC(model1) - AIC(model2)
extractAIC(model1) - extractAIC(model2)
