#install if necessary
#install.packages('gridExtra')


library(ggplot2)
library(gridExtra)
library(plyr)
library(stringr)
library(fitdistrplus)
library(tidyverse)

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES')

dist <- read.csv('csv depurados/hubdistance.csv')

dist$SE <- revalue(dist$SE, c('bio'='Biodiversity', 'hum'='Drinking water',
                                      'estetic' ='Aesthetic values', 
                                      'recreat'='Recreational uses',
                                      'hid'='Water regulation', 
                                      'riego'='Irrigation'))
dist$Category <- revalue(dist$categoria, c('cultural'='Cultural',
                                    'provisioning'='Provisioning',
                                    'regulating'='Regulating'))


#creem els gràfics de frequència de direccions
for (i in unique(dist$SE)){
  subset <- subset(dist, SE == i)
  subset$az_cat <- cut(subset$az, breaks = c(0,45,90,135,180,225,270,315,360),
                         labels = c('NNE','NEE','SEE','SSE','SSW','SWW','NWW','NNW' ),
                         ordered_result = TRUE)
  az_cat <- as.data.frame(table(subset$az_cat))
  az_cat$perc <- az_cat$Freq/sum(az_cat$Freq)
  ggplot(az_cat, aes(x=Var1, y=perc)) + coord_polar() +
    geom_bar(stat='identity', fill='dark red') + 
    labs(x=i, y='Fraction')+theme_set(theme_grey(base_size = 18))
  ggsave(filename = paste('plots/02_dir_',i,'.png', sep=""))
}


#creem el gràfics de direcció, distància i SE
ggplot(dist, aes(x=az, y=dist, color=Category))+
  coord_polar()+geom_point(size=1)+labs(x='Direction (º)', y='Distance (km)')+
  theme(axis.title.y = element_text(hjust = 0.5))
ggsave(filename = 'plots/dist_azimut.png')
ggplot(dist, aes(x=az, y=dist, color=depend))+
  coord_polar()+geom_point(size=1)+
  labs(x='Direction', y='Distance', color='Dependency')
ggsave(filename = 'plots/dist_azimut_depend.png')

#creem histograma de distància
ggplot(dist, aes(dist/1000))+geom_histogram()+
  labs(x='Distance', y='Frequency')

fit.gamma <- fitdist(dist$dist/1000, distr = "gamma", method = "mme")

plot(fit.gamma)

plot_gamma <- denscomp(fit.gamma, xlim=c(0,40), ylim = c(0,0.25),
         datacol="black", legendtext = 'Gamma distribution',
         xlab='Distance (km)', main='', plotstyle = "ggplot")

plot_gamma + theme(axis.text.x = element_text(size=14),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title = element_text(size=14),
                   legend.position = c(0.8,0.95))+ylab("Frequency")+
              scale_x_continuous(breaks = seq(0, 40, by = 5))
ggsave('plots/histogram.png')

ggplot(dist, aes(x=hubs_layer_z_z, y=z_diff))+geom_point()+geom_smooth()

ggplot(dist, aes(dist))+geom_histogram()+stat_function(fun=fit.gamma)

