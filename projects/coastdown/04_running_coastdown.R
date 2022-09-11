source('00_variaveis_globais.R')
source('02_coastdown_functions.R')
source('03_track_functions.R')

corrected_coefficients <- coefficients_correction(nominal_coefficients, AMB_TEMP, AMB_PRESS)

v0 <- V0 
vf <- Vf 


get_theta <- splinefun(track$path, track$theta, method = "fmm")
get_height <- splinefun(track$path, track$height, method = "fmm")

# Coastdown analítico - como seria sem inclinação de pista
ca <- coastdown_analytical(nominal_coefficients, M, V0, Vf)
# coast_force_calc(M, V0, Vf, 5/3.6, ca)
# Coastdown simulado com inclinçãoa de pista

fwd_fun <- coastdown_sp_func(M, nominal_coefficients, V0, Vf, track, 'forward')
bwd_fun <- coastdown_sp_func(M, nominal_coefficients, V0, Vf, track, 'backward')

starting_points <- seq(500,2500,length.out = 25)
coef_fwd <- lapply(fwd_fun,X = starting_points)
coef_fwd <- do.call(rbind.data.frame, coef_fwd)
coef_fwd$starting_points <- starting_points
coef_fwd <- coef_fwd %>% na.omit()

starting_points <- seq(1000,3500,length.out = 25)
coef_bwd <- lapply(bwd_fun,X = starting_points)
coef_bwd <- do.call(rbind.data.frame, coef_bwd)
coef_bwd$starting_points <- starting_points
coef_bwd <- coef_bwd %>% na.omit()

coeffs <- crossing(fwd_sp = coef_fwd$starting_points,
                   bwd_sp = coef_bwd$starting_points)

coeffs_complete <- left_join(coeffs, coef_fwd, by = c("fwd_sp" = "starting_points")) %>% 
  left_join(coef_bwd, by = c("bwd_sp" = "starting_points"), suffix = c('_fwd','_bwd')) %>% 
  mutate(f0_avg = (f0_fwd + f0_bwd)/2,
         f2_avg = (f2_fwd + f2_bwd)/2)


