#install if necessary
#install.packages('remotes')
install.packages('nngeo')
remotes::install_github("michaeldorman/nngeo")

library(sf)
library(nngeo)

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES/puntos/shp_puntos_Muga_divididos')

ahub <- st_read('0_sum_exp_cons_hum.shp')
1origin <- st_read('0_ben_exp_cons_hum.shp')

#guardem hubname, distance i azimut a origin
hubname <- st_nearest_feature(origin,hub)
origin$hubname <- hubname
origin$dist = st_distance(origin, hub[hubname, ], by_element = TRUE)
origin$az = st_azimuth(origin, hub[hubname, ])


print(as.data.frame(ahub)[1,1:2])
