#install if necessary
#install.packages('gridExtra')
#install.packages('fitdistrplus')

library(ggplot2)
library(gridExtra)
library(plyr)
library(fitdistrplus)
library(glmmTMB)
library(rsq)

setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES')

dist <- read.csv('csv depurados/hubdistance_z.csv')

dist$z_diff <- dist$z_diff * -1

#comprovem com encaixen les dades en una distribució gamma
fit.gamma <- fitdist(dist$dist, distr = "gamma", method = "mme")
plot(fit.gamma)

#boxplot amb SE
dist$SE_labels <- revalue(dist$SE, 
                               c('bio'='Biodiversity', 'hum'='Drinking water',
                                          'estetic' ='Aesthetic values', 
                                          'recreat'='Recreational uses',
                                          'hid'='Water regulation', 
                                          'riego'='Irrigation'))
ggplot(dist, aes(x=SE_labels, y=dist/1000))+geom_boxplot(fill='red')+labs(x='', y='distance')+
  theme(axis.text.x=element_text(angle =90, hjust=1, vjust = 0, size=12))+
  ylab("Distance (km)")
ggsave(filename = 'plots/SE_boxplot.png')

ggplot(dist, aes(x=dist, y=z_diff))+
  geom_smooth()+geom_point(alpha=0.3)


# #convertim els 0 a 10^-6 sumant 10
# dist$dist <- dist$dist + 10^-6
# 
# #calculem el glm i l'avaluem
# glm.dist <- glm(dist ~ categoria + depend + az + z_diff + z, 
#                 data=dist, family = Gamma())

# Remove missing values & standardize the predictors:
gdata <-dist[,c("dist","categoria","depend","az","z_diff","z")]
gdata <- gdata[!rowSums(is.na(gdata)),]
gdata$az <- scale(gdata$az)
gdata$z_diff <- scale(gdata$z_diff)
gdata$z <- scale(gdata$z)

glmfit1 <- glm(dist~az+I(az^2)+z_diff+I(z_diff^2)+z+I(z^2)+depend+categoria,data=gdata,family=Gamma())
summary(glmfit1)
with(summary(glmfit1), 1-deviance/null.deviance)
rsq(glmfit1)
rsq(glmfit1, adj=TRUE)


summary(glm.dist)
plot(glm.dist)
with(summary(glm.dist), 1 - deviance/null.deviance)
rsq(glm.dist, adj=TRUE)