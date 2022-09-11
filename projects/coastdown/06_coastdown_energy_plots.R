source('05_coastdown_cycle_energy.R')

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

p1 <- coeffs_complete %>% 
  mutate(f0_delta = 1-f0_avg/F0,
         f2_delta = 1-f2_avg/F2) %>% 
  filter(abs(f0_delta) <= 0.08) %>% 
  ggplot(aes(x = fwd_sp, y = bwd_sp, fill = f0_delta))+
  geom_tile()+
  geom_text(aes(label = scales::percent(f2_delta,.1)), size = 2.6)+
  scale_fill_gradient2()+
  theme_apa()


p2 <- coeffs_complete %>% 
  mutate(f0_delta = 1-f0_avg/F0,
         f2_delta = 1-f2_avg/F2) %>% 
  filter(abs(f2_delta) <= 0.08) %>% 
  ggplot(aes(x = fwd_sp, y = bwd_sp, fill = f2_delta))+
  geom_tile()+
  geom_text(aes(label = scales::percent(f2_delta,.1)), size = 2.6)+
  scale_fill_gradient2()+
  theme_apa()

pgrid <- gridExtra::grid.arrange(p1,p2, ncol = 2)
ggplotly(p1)
ggplotly(p2)

#### Energy ####

ggplotly(coeffs_complete %>% 
           mutate(nominal_energy = coast_energy(nominal_coefficients['f0'],nominal_coefficients['f2'], city$v, hwy$v),
                  sp_energy_calc = coast_energy(f0_avg,f2_avg, city$v, hwy$v),
                  energy_diff = 1 - sp_energy_calc/nominal_energy) %>% 
           ggplot(aes(x = fwd_sp, y = bwd_sp, fill = energy_diff))+
           geom_tile()+
           geom_text(aes(label = scales::percent(energy_diff,.1)), size = 2.6)+
           scale_fill_gradient2()+
           theme_apa())

ggplotly(coeffs_complete %>% 
           mutate(nominal_energy = coast_energy(nominal_coefficients['f0'],nominal_coefficients['f2'], city$v, hwy$v),
                  sp_energy_calc = coast_energy(f0_avg,f2_avg, city$v, hwy$v),
                  energy_diff = 1 - sp_energy_calc/nominal_energy) %>% 
           filter(abs(energy_diff) <= 0.02) %>% 
           ggplot(aes(x = fwd_sp, y = bwd_sp, fill = energy_diff))+
           geom_tile()+
           geom_text(aes(label = scales::percent(energy_diff,.1)), size = 2.6)+
           scale_fill_gradient2()+
           theme_apa())
