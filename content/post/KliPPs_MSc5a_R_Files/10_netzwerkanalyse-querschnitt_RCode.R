#######################
### Netzwerkanalyse im Querschnitt
# von Kai Nehler


## Daten laden
raw_data <- readRDS(url("https://osf.io/awz3d/download"))
head(raw_data) # Übersicht

## Daten aufbereiten
names(raw_data) <- c("observe", "describe", "awaren.", "nonjudg.",
                     "nonreact.", "interest",  "emotions",  "sleep",
                     "tired",  "appetite", "selfim.",
                     "concentr.", "speed")


## Netzwerkschätzung

install.packages("bootnet") # Paket installieren
library(bootnet) # Paket laden

cor_net <- estimateNetwork(raw_data, default = "cor") # Schätzung des Netzwerkes mit Korrelationen
summary(cor_net) # Übersicht der Ergebnisse

plot(cor_net) # Plot des Netzwerks
cor_net$graph # Korrelationsmatrix

pcor_net <- estimateNetwork(raw_data, default = "pcor") # Schätzung des Netzwerks mit Partialkorrelationen
summary(pcor_net) # Übersicht der Ergebnisse

plot(pcor_net) # Plot des Netzwerks


### Regularisierung und Netzwerkauswahl

reg_net <- estimateNetwork(raw_data, default = "EBICglasso", # Schätzung des Netzwerks mit Lasso-Regulation und Tuning 0.5
                           nlambda = 100, tuning = 0.5)
summary(reg_net) # Übersicht der Ergebnisse

reg_net$results$optnet # Gewichte der Kanten in bestem Netzwerk
reg_net$results$lambda # Bestrafungsparameter
reg_net$results$results$wi[,,100] # komplett leeres Netzwerk
reg_net$results$ebic # EBICs

reg_net2 <- estimateNetwork(raw_data, default = "EBICglasso", # Schätzung des Netzwerks mit Lasso-Regulation und Tuning 2
                            nlambda = 100, tuning = 2)
summary(reg_net2) # Übersicht der Ergebnisse

plot(reg_net) # Plot des Netzwerks



## Zentralitätsindizes

library(qgraph) # Paket laden

centrality_indices <- centrality(graph = reg_net$graph) # Zentralitätsindizes berechnen

centrality_indices$OutDegree # Degree bzw. Strength der Knoten

centralityPlot(reg_net, include = c("Strength")) # Plot der Stärke der Knoten als z-Werte
centralityPlot(reg_net, scale = "raw", include = c("Strength")) # Plot der Stärke der Knoten als Rohwerte



## Bootstrap

set.seed(2022) # Seed festlegen
boot1 <- bootnet(reg_net, nBoots = 100, nCores = 1) # Bootstrap mit 100 Durchführungen
plot(boot1, order = "sample", labels = F) # Plot

set.seed(2022)
boot2 <- bootnet(reg_net,   nBoots = 300,
                 statistics = c("strength"),
                 type = "case", caseMin = 0.05,
                 caseMax = 0.75, caseN = 15,
                 nCores = 1)
plot(boot2, c("strength"))

corStability(boot2, cor = 0.7) # CS-Werte bestimmen
