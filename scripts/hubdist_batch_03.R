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
  hub$x <- st_coordinates(hub)[,1]
  hub$y <- st_coordinates(hub)[,2]

  #fem els cálculs
  hubname <- st_nearest_feature(origin,hub)
  origin$hubname <- hubname
  origin$dist = st_distance(hub[hubname,], origin, by_element = TRUE)
  origin$az = st_azimuth(hub[hubname,], origin)

  #creem un camp de id únic per origin amb el nom del shp
  shapefile <- origins[i]
  shapefile <- gsub(dir,"",shapefile)
  shapefile <- gsub("/","",shapefile)
  shapefile <- gsub(".shp","",shapefile)
  shapefile <- gsub("`","",shapefile)
  origin$shp <- shapefile
  origin$id_ori <- paste(shapefile,origin$id, sep="_")


  #creem un camp de id únic en el shp origin per identificar el hubname
  shapehub <- hubs[i]
  shapehub <- gsub(dir,"",shapehub)
  shapehub <- gsub("/","",shapehub)
  shapehub <- gsub(".shp","",shapehub)
  shapehub <- gsub("`","",shapehub)
  hub$shp <- shapehub
  origin$id_hub <- paste(shapehub,origin$hubname, sep="_")
  hub$id_hub <- paste(shapehub,hub$id, sep="_")
  
  #assignem els noms als dos objectes origin i hub
  assign(shapefile, origin)
  assign(shapehub, hub)
}

#creem un data frame amb tots els origin
ori_results <- as.data.frame(`0_ben_exp_bio`)
for (i in ls(pattern="ben")[2:length(ls(pattern = "ben"))]){
  ori_results <- bind_rows(ori_results,as.data.frame(get(i)))
}

#eliminem camps sobrants
ori_results <- select(ori_results, -geometry)
ori_results <- select(ori_results, -path)
ori_results <- select(ori_results, -layer)

nd <- as.character(c(10,12,16,17,18,19,20,22,23,24,26,5,8))
dd <- as.character(c(0,1,11,13,14,15,2,21,3,4,6,7,9))
ori_results$depend <- "buit"

#creem els camps a partir del camp shp
for (i in 1:nrow(ori_results)){
  ori_results$tipo[i] <- "ben"
 SE_list <- str_split(ori_results$shp[i], "_")
  ori_results$sector[i] <- as.numeric(SE_list[[1]][1])
  ori_results$SE[i] <- (str_split(ori_results$shp[i], "_"))[[1]][length(SE_list[[1]])]
  if (SE_list[[1]][length(SE_list[[1]])]=="bio"){ori_results$categoria[i] <- 'regulating'}
  if (SE_list[[1]][length(SE_list[[1]])]=="hum"){ori_results$categoria[i] <- 'provisioning'}
  if (SE_list[[1]][length(SE_list[[1]])]=="estetic"){ori_results$categoria[i] <- 'cultural'}
  if (SE_list[[1]][length(SE_list[[1]])]=="recreat"){ori_results$categoria[i] <- 'cultural'}
  if (SE_list[[1]][length(SE_list[[1]])]=="hid"){ori_results$categoria[i] <- 'regulating'}
  if (SE_list[[1]][length(SE_list[[1]])]=="riego"){ori_results$categoria[i] <- 'provisioning'}
  if (SE_list[[1]][1] %in% dd){ori_results$depend[i] <- "dd"}
  if (SE_list[[1]][1] %in% nd){ori_results$depend[i] <- "nd"}
}

write.csv(ori_results, file='hubdistance.csv')


#creem un data frame amb tots els hub
hub_results <- as.data.frame(`0_sum_exp_bio`)
for (i in ls(pattern="sum")[2:length(ls(pattern = "sum"))]){
  hub_results <- bind_rows(hub_results,as.data.frame(get(i)))
}

#eliminem camps sobrants
hub_results <- select(hub_results, -geometry)
hub_results <- select(hub_results, -layer)
hub_results <- select(hub_results, -path)

hub_results$depend <- "buit"
#creem els camps a partir del camp shp
for (i in 1:nrow(hub_results)){
  hub_results$tipo[i] <- "sum"
  SE_list <- str_split(hub_results$shp[i], "_")
  hub_results$sector[i] <- as.integer(SE_list[[1]][1])
  hub_results$SE[i] <- SE_list[[1]][length(SE_list[[1]])]
  if (SE_list[[1]][length(SE_list[[1]])]=="bio"){hub_results$categoria[i] <- 'regulating'}
  if (SE_list[[1]][length(SE_list[[1]])]=="hum"){hub_results$categoria[i] <- 'provisioning'}
  if (SE_list[[1]][length(SE_list[[1]])]=="estetic"){hub_results$categoria[i] <- 'cultural'}
  if (SE_list[[1]][length(SE_list[[1]])]=="recreat"){hub_results$categoria[i] <- 'cultural'}
  if (SE_list[[1]][length(SE_list[[1]])]=="hid"){hub_results$categoria[i] <- 'regulating'}
  if (SE_list[[1]][length(SE_list[[1]])]=="riego"){hub_results$categoria[i] <- 'provisioning'}
  if (SE_list[[1]][1] %in% dd){hub_results$depend[i] <- "dd"}
  if (SE_list[[1]][1] %in% nd){hub_results$depend[i] <- "nd"}
}


write.csv(hub_results, file='hubs_layer.csv')

# #proves
# SE_list[[1]][1]
# if (SE_list[[1]][1] %in% nd){print("és nd")}
# if (SE_list[[1]][1] %in% dd){print("és dd")}
