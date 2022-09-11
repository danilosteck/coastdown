source('04_running_coastdown.R')

if(!dir.exists('./ext_data')){
  dir.create('./ext_data')
}

if(!file.exists('./ext_data/city_ftp.txt')){
  download.file(url = 'https://www.epa.gov/sites/default/files/2015-10/ftpcol.txt', destfile = './ext_data/city_ftp.txt')
}

if(!file.exists('./ext_data/highway_ftp.txt')){
  download.file(url = 'https://www.epa.gov/sites/default/files/2015-10/hwycol.txt', destfile = './ext_data/highway_ftp.txt')
}

city <- read.table('./ext_data/city_ftp.txt',skip = 2, col.names = c('t','v')) %>% 
  mutate(v = v*1.609/3.6)
hwy <- read.table('./ext_data/highway_ftp.txt',skip = 2, col.names = c('t','v')) %>% 
  mutate(v = v*1.609/3.6)

coast_energy <- function(f0, f2, city_spd, hwy_spd, city_pct = 0.55){
  city_energy <- f0*sum(city_spd) + f2*sum(city_spd^3)
  hwy_energy <- f0*sum(hwy_spd) + f2*sum(hwy_spd^3)  
  coast_energy <- city_pct*city_energy + (1-city_pct)*hwy_energy
  return(coast_energy)
}

