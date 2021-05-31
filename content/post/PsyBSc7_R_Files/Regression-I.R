#######################
### Regression I
# von Marvin Schröder und Luisa Grützmacher


load(url("https://pandar.netlify.app/post/Schulleistungen.rda")) #Datensatz laden


###Lineare Regression

##Berechnung der Regressionsgewichte "per Hand"

#Vektor Y
y <- Schulleistungen$math
head(y)

#Matrix X vorbereiten (Spalten mit beiden Prädiktoren + Spalte mit Einsen anfügen)
X <- as.matrix(Schulleistungen[,c("reading", "IQ")])
X <- cbind(rep(1,nrow(X)), X)
head(X)


#1. Berechnung der der Kreuzproduktsumme X’X in R
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
y.s <- scale(y) # Standardisierung y
X.s <- scale(X) # Standardisierung X
X.s[,1] <- 1    # Einsenvektor ist nach Standardisierung zunächst NaN, muss wieder gefüllt werden

#Kombination der Einzelschritte zur Bestimmung der Regressionsgewichte
b.hat.s <- solve(t(X.s)%*% X.s) %*% t(X.s)%*%y.s #Regressionsgewichte aus den standardisierten Variablen
round(b.hat.s, 3)


##Berechnung des globalen Signifikanztests

#Determinationskoeffizient R2
Q.d <- sum((y.hat - mean(y))^2)    # Regressionsquadratsumme
Q.e <- sum((y - y.hat)^2)          # Fehlerquadratsumme
R2 <- Q.d / (Q.d + Q.e)            # Determinationskoeffizient R^2

#F-Wert
n <- length(y)                       # Fallzahl (n=100)
m <- ncol(X)-1                       # Zahl der Prädiktoren (m=2)
F.omn <- (R2/m) / ((1-R2)/(n-m-1))   # F-Wert
F.krit <- qf(.95, df1=m, df2=n-m-1)  # kritischer F-Wert (alpha=5%)
p <- 1-pf(F.omn, m, n-m-1)           # p-Wert


###Berechnung der Regression mit lm-Funktionen in R

#Paket installieren (wenn nötig)
#install.packages("lm.beta", repos = "http://cran.us.r-project.org")
library(lm.beta)

#Regressionsanalyse mit lm
reg <- lm(math ~ reading + IQ, data = Schulleistungen)

#Ergebnisausgabe einschließlich standardisierter Koeffizienten mit lm.beta
summary(lm.beta(reg))
