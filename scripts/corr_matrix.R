library(raster)
plot.new()

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES/resultados')

#cargamos los raster de demand accumluation
provision <- raster('prov_mask.tif')
cultural <- raster('cult_mask.tif')
regulating <- raster('regul_mask.tif')


cultural <- raster(vals=values(cultural),ext=extent(provision),
                crs=crs(provision),
                nrows=dim(provision)[1],ncols=dim(provision)[2])
regulatuing <- raster(vals=values(regulating),ext=extent(provision),
               crs=crs(provision),
               nrows=dim(provision)[1],ncols=dim(provision)[2])

#hacemos la matriz de correlaciones para los actores
SE <- stack(provision, regulating, cultural)
cor.SE <- layerStats(SE, 'pearson', na.rm = TRUE)
print(cor.SE)
write.csv2(cor.SE[[1]], 'correlation.csv')
