library(sf)
library(stringr)

#setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES/puntos/shp_puntos_Muga_divididos')

#creem una llista amb tots els shapefiles de dir
dir <- "C:/Users/jpueyo/OneDrive - ICRA/Water ES/puntos/shp_puntos_Muga_divididos"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)
# print(ff)
# #creem un loop que carrega tots els shapes de ff i si són 'ben' els afegeix a un vector
# origins <- 1
# i <- 1
# for (shp in ff){
#   #carreguem el shape
#   object <- st_read(shp)
#   #creem un camp únic numèric
#   object$id <- c(1:nrow(object))
#   #arreglem el nom de shp i l'assignem a l'objecte
#   shapefile <- shp
#   shapefile <- gsub(dir,"",shapefile)
#   shapefile <- gsub("/","",shapefile)
#   shapefile <- gsub(".shp","",shapefile)
#   shapefile <- gsub("`","",shapefile)
#   assign(shapefile, object)
#   #si el shp és de beneficiaris, l'afegim a un vector
#   if (grepl('ben',shapefile)){
#     origins[i] <- shapefile
#     i <- i+1
#   }
# }

#Executem un loop on es carreguen els shapes i es fan els càlculs de dos en dos
for (shp in ff){
  #carreguem el shape si és ben i creem id
  if (grepl('ben',shp)){
    origin <- st_read(shp)
    origin$id <- c(1:nrow(origin))
  }
  #calculem hubname i l'afegim a origin
  hubname <- st_nearest_feature(origin,hub)
  origin$hubname <- hubname
  #calculem les distàncies
  dist_matrix <- st_distance(origin,hub, by_element=FALSE)
  #recuperem el valor mínim de cada filera i el guardem a origin
  dist <- 1
  for (i in 1:nrow(dist_matrix)){
    dist[i] <- min(dist_matrix[i,])
  }
  origin$dist <- dist
  #arreglem el nom de shp i l'assignem a origin
  shapefile <- shp
  shapefile <- gsub(dir,"",shapefile)
  shapefile <- gsub("/","",shapefile)
  shapefile <- gsub(".shp","",shapefile)
  shapefile <- gsub("`","",shapefile)
  assign(shapefile, origin)
}

