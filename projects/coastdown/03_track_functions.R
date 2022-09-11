
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
  
  track <- track_calc(data.frame(path = S, height = H_filt))
  
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
  
  track <- track_calc(data.frame(path = S, height = H_filt))
  
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

plot_track <- function(track){
  grade_hist <- hist(track$tangent)
  track_height_profile <- plot(track$path,track$height, type = 'l')
  track_grade <- plot(track$path, track$tangent, type = 'l')
  out <- list()
  out[['grade_hist']] <- grade_hist
  out[['track_height_profile']] <- track_height_profile
  out[['track_grade']] <- track_grade
  return(out)
}

filt_grade <- function(track, filt_order, filt_freq){
  grade_filt <- Re(filter.fft(track$tangent,track$path,fc=filt_freq,BW=filt_order,n=3))
  H0 <- track$height[1]
  dH <- (track$ds)*grade_filt
  H <- cumsum(dH)
  track$height <- H
  track$tangent <- grade_filt
  return(track)
}

get_max_grade <- function(track){
  max_grade <- scales::percent((max(track$height)-min(track$height))/(max(track$path)-min(track$path)),.01)
  max_tangent <- scales::percent(max(abs(track$tangent)),.01)
  return(c(max_grade = max_grade, max_tangent = max_tangent))
}

