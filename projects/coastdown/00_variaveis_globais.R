library(ggplot2)
library(jtools)
library(tidyr)
library(plotly)

#### Variáveis do veículo
M <- 1200   # Massa [kg]
F0 <- 150   # N
F2 <- 0.4   # N.s²/m²
nominal_coefficients <- c(f0 = F0, f2 = F2)

#### Variáveis ambiente
AMB_TEMP <- 20 # Temperatura Ambiente
AMB_PRESS <- 101.325 # Pressão ambiente

#### Variáveis de teste
V0 <- 100/3.6 # Velocidade inicial (m/s)
Vf <- 30/3.6 # Velocidade final (m/s)