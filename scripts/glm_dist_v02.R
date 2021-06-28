#install if necessary
#install.packages('gridExtra')
#install.packages('fitdistrplus')



library(ggplot2)
library(gridExtra)
library(plyr)
library(fitdistrplus)
library(glmmTMB)

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES/csv depurados')

dist <- read.csv('hubdistance.csv')

#comprovem com encaixen les dades en una distribució gamma
fit.gamma <- fitdist(dist$dist)
plot(fit.gamma)
denscomp(fit.gamma, ylim=c(0,0.00015))

#boxplot amb SE
dist$SE_labels <- revalue(dist$SE, 
                               c('bio'='Biodiversity', 'cons'='Drinking water',
                                          'estetic' ='Aesthetic values', 
                                          'recreat'='Recreational uses',
                                          'reg'='Water regulation', 
                                          'riego'='Irrigation'))
ggplot(dist, aes(x=SE_labels, y=dist))+geom_boxplot(fill='red')+labs(x='', y='distance')+
  theme(axis.text.x=element_text(angle =90, hjust=1, vjust = 0))
ggsave(filename = 'plots/SE_boxplot.png')

#convertim els 0 a 10^-6 sumant 10
dist$dist <- dist$dist + .Machine$double.xmin

#glm.dist <- glmmTMB(dist ~ cat_ES + nd/dd + Z_diff + azimut, data=dist, family = ziGamma)
glm.dist
plot(glm.dist)


ggplot(dist, aes(x=az, y=dist, color=categoria))+coord_polar()+geom_point()
#hacerlo igual por dd/nd

       