source('01_get_track_height_gmaps_elevation_api.R')
library(dplyr)

track_calc <- function(track){
  track <- track %>%  
    mutate(height = as.numeric(height),
           height_var = dplyr::lag(height, 1, order_by = -path),
           dh = height_var - height,
           ds = dplyr::lag(path, 1, order_by = -path) - path,
           theta = if_else(ds == 0, 0, asin(dh/((ds^2 + dh^2)^0.5))),
           tangent = if_else(ds == 0, 0, dh/ds)) %>%
    na.omit()
  return(track)
}

coefficients_correction <- function(nominal_coefficients, amb_temp, amb_press){
  f0c <- nominal_coefficients['f0']*(1 + 8.6e-3*(amb_temp - 20))
  f2c <- 101.325*(amb_temp + 273.15)/(amb_press * 293.15)*(nominal_coefficients['f2'] - 2.503e-4 * nominal_coefficients['f0']) + 2.503e-4*f0c
  corrected_coefficients <- c(f0c, f2c)
  return(corrected_coefficients)
}

runif_track_height <- function(n = 3000, min = -0.007, max = 0.007, dS = 1){
  # track_height_rate <- rnorm(n = n, mean = mean, sd = sd)
  track_height_rate <- runif(n = n, min = min, max = max)
  dS <- dS # step de medição da pista em metros
  S <- seq(0, length(track_height_rate)-1)*dS
  H <- cumsum(track_height_rate)*dS
  require(signal)
  bf <- butter(2, 1/10, type = 'low')
  H_filt <- signal::filter(bf, H)
  plot(S, H, type = 'l')
  lines(S, H_filt, col = 'red')
  
  track <- track_calc(data.frame(path = S, height = H))
  
  return(track)
}

rnorm_track_height <- function(n = 3000, mean = -0.0049, sd = 0.0049, dS = 1){
  # track_height_rate <- rnorm(n = n, mean = mean, sd = sd)
  track_height_rate <- rnorm(n = n, mean = mean, sd = sd)
  dS <- dS # step de medição da pista em metros
  S <- seq(0, length(track_height_rate)-1)*dS
  H <- cumsum(track_height_rate)*dS
  require(signal)
  bf <- butter(2, 1/10, type = 'low')
  H_filt <- signal::filter(bf, H)
  plot(S, H, type = 'l')
  lines(S, H_filt, col = 'red')
  
  track <- track_calc(data.frame(path = S, height = H))
  
  return(track)
}

constant_track_height <- function(n = 3000, theta_before = 0, theta_after = 0.0049,dist_change = 600, dS = 1){
  # track_height_rate <- rnorm(n = n, mean = mean, sd = sd)
  S <- seq(0, n-1, by = dS)
  track_height_rate <- if_else(S <= dist_change, theta_before, theta_after)
  H <- cumsum(track_height_rate)*dS
  track <- track_calc(data.frame(path = S, height = H))
  return(track)
}

coast_force <- function(m, coef, v0, vf, track, dt = 0.1, sp = 0, direction = 'forward'){
  t <- 0
  v <- v0
  s <- sp
  theta <- get_theta(s)
  height <- get_height(s)
  i <- 1
  erro <- 'nao'
  while(v >= vf & erro != 'sim'){
    rr_force <- (coef['f0']+coef['f2']*v^2)
    grade_force <- m*9.807*sin(theta)
    total_force = rr_force + grade_force
    accel = -total_force/m
    if(accel >= 0 | s > max(track$path) | s < min(track$path)){erro = 'sim'}else(erro = 'nao')
    if(i == 1){
      coastdown <- data.frame(iter = i, time = t, dist = s, speed = v, accel = accel,
                              height = height, theta = theta, 
                              rr_force = rr_force, grade_force = grade_force,
                              total_force = total_force, erro = erro)
    }else{
      coastdown <- rbind(coastdown, data.frame(iter = i, time = t, dist = s, speed = v, accel = accel,
                              height = height, theta = theta, 
                              rr_force = rr_force, grade_force = grade_force,
                              total_force = total_force, erro = erro))
    }
    if(direction == 'forward'){s = s + v*dt}else{s = s - v*dt}
    v = v + accel*dt
    t <- t + dt
    theta = get_theta(s)
    height = get_height(s)
    
    # print(paste0('i = ',i,';', 'v = ', v))
    i = i + 1
  }
  return(coastdown)
}

coastdown_analytical <- function(nominal_coefficients, m, v0, vf){
  m1 <- (nominal_coefficients['f0']/nominal_coefficients['f2'])^0.5; 
  m2 <- (nominal_coefficients['f0']*nominal_coefficients['f2'])^0.5
  time <- seq(0,300, by = 0.1)
  speed <- m1*tan((m*atan(v0/m1)-m2*time)/m)
  coast_ts_analytical <- data.frame(time = time, speed = speed) %>% dplyr::filter(speed >= vf)
  return(coast_ts_analytical)
}


coast_force_calc <- function(m, v0, vf, dv, coast_data){
  get_coast_time <- splinefun(x = coast_data$speed, y = coast_data$time, method = 'fmm')
  coast_times <- sapply(seq(v0,vf,-dv), get_coast_time)
  # coast_times <- c(0,3.8,7.9,12.2,16.6,21.7,27.1,33.1,40.6,48.9,56.6,64.3,72.7,83.1,94.5)
  calc_times <- data.frame(time = coast_times, speed = seq(v0,vf,-dv)) %>% 
    mutate(timelag = dplyr::lag(time,1), accel = dv/(timelag-time), avgspeed = speed+(dv/2),
           sqrd_avg_spd = avgspeed^2,
           frth_avg_spd = avgspeed^4,
           accel_sqrd_spd = accel*avgspeed^2) %>% 
    na.omit()
  param_calc_coef <- calc_times %>% 
    na.omit() %>% 
    summarise_all(sum) %>% 
    rename(A = accel, B = avgspeed, C = sqrd_avg_spd, D = frth_avg_spd, E = accel_sqrd_spd) %>% 
    select(A,B,C,D,E)
  # print(param_calc_coef)
  n <- length(coast_times)
  # print(n)
  f0 <- with(param_calc_coef, m*(D*A-C*E)/((n-1)*D-C^2))
  f2 <- with(param_calc_coef, m*((n-1)*E-C*A)/((n-1)*D-C^2))
  params <- data.frame(f0 = -f0, f2 = -f2)
  return(params)
}


cd_startpos_fwd <- function(sp){
  coast_data <- coast_force(M, nominal_coefficients, 100/3.6, 30/3.6, track = track, dt = 0.1, sp, 'forward')
  if('sim' %in% coast_data$erro){cd_result <- data.frame(f0 = NA, f2 = NA)}else{
    cd_result <- coast_force_calc(M,100/3.6,30/3.6,5/3.6,coast_data)
  }
  return(cd_result)
}
cd_startpos_bwd <- function(sp){
  coast_data <- coast_force(M, nominal_coefficients, 100/3.6, 30/3.6, track = track, dt = 0.1, sp, 'backward')
  if('sim' %in% coast_data$erro){cd_result <- data.frame(f0 = NA, f2 = NA)}else{
    cd_result <- coast_force_calc(M,100/3.6,30/3.6,5/3.6,coast_data)
  }
  return(cd_result)
}

