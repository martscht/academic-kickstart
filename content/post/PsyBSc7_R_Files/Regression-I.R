#######################
### Regression I
# von Kai Nehler, Marvin Schröder und Luisa Grützmacher


# Datensatz laden
load(url("https://pandar.netlify.app/post/Schulleistungen.rda"))
names(Schulleistungen)



### Lineare Regression

## Berechnung der Regressionsgewichte "per Hand"

#Vektor Y
y <- Schulleistungen$math
head(y)

# Matrix X vorbereiten
X <- as.matrix(Schulleistungen[,c("reading", "IQ")])

# Matrix X erweitern
constant <- rep(1, nrow(X))
X <- cbind(constant, X)
head(X)


#1. Berechnung der Kreuzproduktsumme X’X in R
X.X <- t(X) %*% X        # X' erhalten Sie durch t(X)
X.X

#2. Berechnung der Inversen (mit Regel nach Sarrus) in R
solve(X.X)

#3. Berechnung des Kreuzproduksummenvektors X`y in R
X.y <- t(X) %*% y
X.y

#4. Berechnung des Einflussgewichtsvektor in R
b.hat <- solve(X.X) %*% X.y     # Vektor der geschätzten Regressionsgewichte
b.hat


y.hat <- as.vector(X %*% b.hat) # Vorhersagewerte für jede einzelne Person
head(y.hat)

#Berechnung der standardisierten Regressionsgewichte
y_s <- scale(y) # Standardisierung y
X_s <- scale(X) # Standardisierung X
head(X_s)
X_s[,1] <- 1    # Einsenvektor wieder auffüllen

#Kombination der Einzelschritte zur Bestimmung der Regressionsgewichte
b_hat_s <- solve(t(X_s)%*% X_s) %*% t(X_s)%*%y_s #Regressionsgewichte aus den standardisierten Variablen
round(b_hat_s, 3)



##Berechnung des globalen Signifikanztests

#Determinationskoeffizient R2
Q_t <- sum((y - mean(y))^2)          # Totale Quadratsumme
Q_d <- sum((y_hat - mean(y))^2)    # Regressionsquadratsumme
Q_e <- sum((y - y_hat)^2)          # Fehlerquadratsumme
round(Q_t,2) == round(Q_d + Q_e, 2)

R2 <- Q_d / (Q_d + Q_e)            # Determinationskoeffizient R^2
# Alternativ Q_d / Q_t



#F-Wert
n <- length(y)                     # Fallzahl (n=100)
m <- ncol(X)-1                     # Zahl der Prädiktoren (m=2)
F_omn <- (R2/m) / ((1-R2)/(n-m-1)) # empirischer F-Wert
F_omn

F_krit <- qf(.95, df1=m, df2=n-m-1)  # kritischer F-Wert (alpha=5%)
F_krit < F_omn  # Vergleich durch logische Überprüfung
p <- 1-pf(F_omn, m, n-m-1)           # p-Wert
p < 0.05


###Berechnung der Regression mit lm-Funktionen in R


#Regressionsanalyse mit lm
reg <- lm(math ~ reading + IQ, data = Schulleistungen)
summary(reg)

install.packages("lm.beta") #Paket installieren (wenn nötig)
library(lm.beta)

# Ergebnisausgabe einschließlich standardisierter Koeffizienten mit lm.beta
reg_s <- lm.beta(reg)
summary(reg_s)         # reg |> lm.beta() |> summary()
