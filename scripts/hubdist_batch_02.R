#install if necessary
#don't install nngeo automatically from Rstudio, use the command with remotes
#install.packages('remotes')
#remotes::install_github("michaeldorman/nngeo")
#install.packages('Orcs')
#install.packages('dplyr')

library(sf)
library(nngeo)
library(sf)
library(stringr)
library(Orcs)
library(dplyr)

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES/')

#creem una llista amb tots els shapefiles de dir
dir <- "C:/Users/jpueyo/OneDrive - ICRA/Water ES/puntos/shp_puntos_Muga_divididos/"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)

#creem un vector amb tots els shapefiles de ben
origins <- 1
i <- 1
for (shp in ff){
  if (grepl('ben',shp)){
    origins[i] <- shp
    i <- i+1
  }
}
#eliminem els shapes de ben que no es corresponen amb un hub
origins <- origins[origins != origins[139]] #8_ben_urb_riego aquest el borrem perquè dins de sum no hi geometries
origins <- origins[origins != origins[127]] #6_r_ben_enp_bio
origins <- origins[origins != origins[126]] #5_r_ben_enp_riego
origins <- origins[origins != origins[125]] #5_r_ben_enp_cons_hum

#creem un vector amb tots els shapefiles de sum
hubs <- 1
i <- 1
for (ori in origins){
  hubs[i] <- gsub("ben","sum",ori)
  i <- i+1
}

#carreguem en un loop origin i hub per parells i fem els càlculs
for (i in 1:length(origins)){
  #carreguem la capa d'origens
  print(gsub("dir","",origins[i]))
  origin <- st_read(origins[i])
  origin$id <- c(1:nrow(origin))
  
  #creem camps amb coordenades pel csv resultant
  origin$x <- st_coordinates(origin)[,1]
  origin$y <- st_coordinates(origin)[,2]
  
  #arreglem camps amb noms equivocats -> "categooria" "nuumero" "nuero" "nuemro"
  if (length(origin$categooria)>0){
  origin$categoria <- origin$categooria
  origin <- select(origin, -categooria)
  }
  if (length(origin$nuumero)>0){
    origin$numero <- origin$nuumero
    origin <- select(origin, -nuumero)
  }
  if (length(origin$nuero)>0){
    origin$numero <- origin$nuero
    origin <- select(origin, -nuero)
  }
  if (length(origin$nuemro)>0){
    origin$numero <- origin$nuemro
    origin <- select(origin, -nuemro)
  }
  
  #carreguem el hub corresponent
  hub <- st_read(hubs[i])
  hub$id <- c(1:nrow(hub))
  hubname <- st_nearest_feature(origin,hub)
  
  #fem els cálculs
  origin$hubname <- hubname
  origin$dist = st_distance(hub[hubname,], origin, by_element = TRUE)
  origin$az = st_azimuth(hub[hubname,], origin)

  #canviem el nom de origin pel del shape de beneficiaris
  shapefile <- origins[i]
  shapefile <- gsub(dir,"",shapefile)
  shapefile <- gsub("/","",shapefile)
  shapefile <- gsub(".shp","",shapefile)
  shapefile <- gsub("`","",shapefile)
  origin$shp <- shapefile
  assign(shapefile, origin)
}

results <- as.data.frame(`0_ben_exp_bio`)
for (i in ls()[2:141]){
  results <- bind_rows(results,as.data.frame(get(i)))
}

#eliminem camps sobrants
results <- select(results, -geometry)
results <- select(results, -path)
results <- select(results, -layer)

write.csv2(results, file='hubdistance_all.csv')

#proves
# plot(density(results$dist), xlim=c(0,40000))
# plot(density(results$dist))
# 
# prova <- st_read(origins[1])
# prova$shp <- origins[1]     
