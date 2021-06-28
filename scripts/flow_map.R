library(ggplot2)
library(sf)
library(nngeo)

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES')


dist <- st_read('csv depurados/hubdistance.csv', 
                options=c("X_POSSIBLE_NAMES=x","Y_POSSIBLE_NAMES=y"),
                crs=25831)
hub <- st_read('csv depurados/hubs_layer.csv',
                options=c("X_POSSIBLE_NAMES=x","Y_POSSIBLE_NAMES=y"),
               crs=25831)

#seleccionem els punts del SE bio
dist_bio <- subset(dist, SE =='bio')
hub_bio <- subset(hub, SE=='bio')

#dibuixem els mapes per bio
plot(st_union(st_geometry(dist_bio), st_geometry(hub_bio)), col = NA)
plot(st_geometry(dist_bio), col = "grey", add = TRUE)
plot(st_geometry(hub_bio), col = "red", add = TRUE)
plot(st_connect(dist_bio, hub_bio), col = "grey", add = TRUE)