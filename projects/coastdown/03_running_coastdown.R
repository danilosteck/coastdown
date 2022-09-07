source('00_variaveis_globais.R')
source('02_coastdown_functions.R')


corrected_coefficients <- coefficients_correction(nominal_coefficients, AMB_TEMP, AMB_PRESS)

v0 <- V0 
vf <- Vf 

###########
set.seed(30)
track <- runif_track_height()
# track <- constant_track_height()
hist(track$tangent)
plot(track$path,track$height_var, type = 'l')
plot(track$path,track$height, type = 'l')
scales::percent((max(track$height)-min(track$height))/(max(track$path)-min(track$path)),.01)
scales::percent(max(abs(track$tangent)),.01)

get_theta <- splinefun(track$path, track$theta, method = "fmm")
get_height <- splinefun(track$path, track$height, method = "fmm")

ca <- coastdown_analytical(nominal_coefficients, M, v0, vf)
###############
coast_data <- coast_force(M, nominal_coefficients, 100/3.6, 30/3.6, track = track, dt = 0.1, sp = 100, direction = 'forward')
coast_data_bw <- coast_force(M, nominal_coefficients, 100/3.6, 30/3.6, track = track, dt = 0.1, sp = max(coast_data$dist), direction = 'backward')

plot(coast_data$time, coast_data$speed, type = 'l'); 
lines(coast_data_bw$time, coast_data_bw$speed, col = 'blue')
lines(ca$time, ca$speed, col = 'red')

plot(coast_data$dist, coast_data$grade_force, type = 'l'); 

coast_data_1300 <- coast_force(1300, nominal_coefficients, 100/3.6, 30/3.6, track = track, dt = 0.1)
plot(coast_data$time, coast_data$total_force, type = 'l'); lines(coast_data_1300$time, coast_data_1300$total_force, col = 'blue')

##############
coast_force_calc(M,100/3.6,30/3.6,5/3.6,coast_data)
coast_force_calc(M,100/3.6,30/3.6,5/3.6,coast_data_bw)

###########

# cd_startpos(20)
cd_startpos_bwd(1350)
starting_points <- seq(200,1800,length.out = 20)
cd_res <- lapply(cd_startpos_fwd,X = starting_points)
cd_res <- do.call(rbind.data.frame,cd_res)
par(mfrow = c(1,3))
plot(starting_points,cd_res$f0/nominal_coefficients['f0'])
plot(starting_points,cd_res$f2/nominal_coefficients['f2'])
plot(track$path, track$height, type = 'l');points(starting_points, get_height(starting_points), col = 'red')
par(mfrow = c(1,1))
plot(cd_res$f0/nominal_coefficients['f0'],cd_res$f2/nominal_coefficients['f2'])
lines(c(0.9,1,1),c(1,1,0.8),col = 'red')
points(c(1,1), col = 'red')

starting_points <- seq(999,2999,length.out = 20)
cd_res_bwd <- lapply(cd_startpos_bwd,X = starting_points)
cd_res_bwd <- do.call(rbind.data.frame,cd_res_bwd)
par(mfrow = c(1,3))
plot(starting_points,cd_res_bwd$f0/nominal_coefficients['f0'])
plot(starting_points,cd_res_bwd$f2/nominal_coefficients['f2'])
plot(track$path, track$height, type = 'l');points(starting_points, get_height(starting_points), col = 'red')
par(mfrow = c(1,1))
plot(cd_res_bwd$f0/nominal_coefficients['f0'],cd_res_bwd$f2/nominal_coefficients['f2'])
lines(c(0.9,1,1),c(1,1,0.8),col = 'red')
points(c(1,1), col = 'red')
