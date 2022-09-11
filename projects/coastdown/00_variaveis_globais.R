library(ggplot2)
library(jtools)
library(tidyr)
library(plotly)
library(spectral)

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

#### Definição da pista ####

## Uniform numbers generated track
set.seed(40)
track <- runif_track_height(n = 4000, min = -0.007, max = 0.007, dS = 1)
plot_track(track)

## Normal Distribution generated track
# set.seed(42)
# track <- rnorm_track_height(n = 3000, mean = 0, sd = 0.0049, dS = 1) 
# plot_track(track)


## Constant Grade Track
# track <- constant_track_height(n = 3000, theta_before = 0, theta_after = 0.0049,dist_change = 600, dS = 1)
# plot_track(track)

## Ibiai track
# track_raw <- get_track_data('ibiai') %>% track_calc()
# track <- track_raw %>% filt_grade(2.6, 2.48) %>% track_calc()
# plot_track(track)