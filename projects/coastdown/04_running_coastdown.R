source('00_variaveis_globais.R')
source('02_coastdown_functions.R')
source('03_track_functions.R')

corrected_coefficients <- coefficients_correction(nominal_coefficients, AMB_TEMP, AMB_PRESS)

v0 <- V0 
vf <- Vf 

# Definição da pista
set.seed(42)
# track <- runif_track_height()
track <- get_track_data('ibiai')

get_theta <- splinefun(track$path, track$theta, method = "fmm")
get_height <- splinefun(track$path, track$height, method = "fmm")

# Coastdown analítico - como seria sem inclinação de pista
ca <- coastdown_analytical(nominal_coefficients, M, V0, Vf)
# coast_force_calc(M, V0, Vf, 5/3.6, ca)
# Coastdown simulado com inclinçãoa de pista

fwd_fun <- coastdown_sp_func(M, nominal_coefficients, V0, Vf, track, 'forward')
bwd_fun <- coastdown_sp_func(M, nominal_coefficients, V0, Vf, track, 'backward')

starting_points <- seq(200,1800,length.out = 20)
coef_fwd <- lapply(fwd_fun,X = starting_points)
coef_fwd <- do.call(rbind.data.frame, coef_fwd)
coef_fwd$starting_points <- starting_points
coef_fwd <- coef_fwd %>% na.omit()

starting_points <- seq(999,2999,length.out = 20)
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

# Plots 
p1 <- track %>% 
  ggplot(aes(x = path, y = height))+
  geom_line()+
  theme_apa()+
  geom_point(data = data.frame(path = coeffs_complete$fwd_sp, height = get_height(coeffs_complete$fwd_sp)), color = 'red')+
  geom_point(data = data.frame(path = coeffs_complete$bwd_sp, height = get_height(coeffs_complete$bwd_sp)), color = 'blue')+
  labs(x = 'Posição [m]', y = 'Altitude relativa [m]')

p2 <- ggplot()+
  geom_point(coef_fwd, mapping = aes(x = starting_points, y = f0, color = 'FWD'), color = 'red')+
  geom_point(coef_bwd, mapping = aes(x = starting_points, y = f0, color = 'BWD'), color = 'blue')+
  theme_apa()

p3 <- ggplot()+
  geom_point(coef_fwd, mapping = aes(x = starting_points, y = f2, color = 'FWD'), color = 'red')+
  geom_point(coef_bwd, mapping = aes(x = starting_points, y = f2, color = 'BWD'), color = 'blue')+
  theme_apa()

gridExtra::grid.arrange(p1,p2,p3, nrow = 3)

ggplotly(coeffs_complete %>% 
           mutate(f0_delta = 1-f0_avg/F0,
                  f2_delta = 1-f2_avg/F2) %>% 
           # filter(fwd_sp <= 1500 & bwd_sp >= 1400) %>% 
           ggplot(aes(x = fwd_sp, y = bwd_sp, fill = f0_delta))+
           geom_tile()+
           geom_text(aes(label = scales::percent(f2_delta,.1)), size = 2.6)+
           theme_apa())

ggplotly(coeffs_complete %>% 
           mutate(f0_delta = 1-f0_avg/F0,
                  f2_delta = 1-f2_avg/F2) %>% 
           # filter(fwd_sp <= 1500 & bwd_sp >= 1400) %>% 
           ggplot(aes(x = fwd_sp, y = bwd_sp, fill = f2_delta))+
           geom_tile()+
           geom_text(aes(label = scales::percent(f2_delta,.1)), size = 2.6)+
           theme_apa())

