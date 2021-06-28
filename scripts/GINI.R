library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
#library(DescTools)
library(gglorenz)
library(tidyverse)
library(ineq)


setwd('C:/Users/jpueyo/OneDrive - ICRA/Water ES')

dist <- read.csv('csv depurados/hubdistance.csv')
hub <- read.csv('csv depurados/hubs_layer.csv')

#canviem noms
dist$SE <- revalue(dist$SE, c('bio'='Biodiversity', 'hum'='Drinking water',
                              'estetic' ='Aesthetic values', 
                              'recreat'='Recreational uses',
                              'hid'='Water regulation', 
                              'riego'='Irrigation'))
dist$Category <- revalue(dist$categoria, c('cultural'='Cultural',
                                           'provisioning'='Provisioning',
                                           'regulating'='Regulating'))

hub$SE <- revalue(hub$SE, c('bio'='Biodiversity', 'hum'='Drinking water',
                              'estetic' ='Aesthetic values', 
                              'recreat'='Recreational uses',
                              'hid'='Water regulation', 
                              'riego'='Irrigation'))
hub$Category <- revalue(hub$categoria, c('cultural'='Cultural',
                                           'provisioning'='Provisioning',
                                           'regulating'='Regulating'))

#creem un data frame amb les ocurrències de cada hub
occ_hubs <- as.data.frame(table(unlist(dist$id_hub)))
occ_hubs$id_hub <- occ_hubs$Var1
occ_hubs <- select(occ_hubs, -Var1)

#fem el join amb hubs_layer i creem csv
hub_gini <- right_join(hub, occ_hubs, by="id_hub")
write.csv(hub_gini, file="csv depurados/hub_gini.csv")

#calculem coeficient de Gini
gini_coeff <- Gini(occ_hubs$id_hub, occ_hubs$Freq)

#dibuixem la corba dee lorenz
ggplot(occ_hubs, aes(Freq)) +
  stat_lorenz(desc = TRUE, color='red', size=1) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  hrbrthemes::theme_ipsum() +
  labs(x = "Cumulative percentage of hubs",
       y = "Cumulative Percentage of total flow")+
  annotate('text', x=0.75, y=0.05, label=paste('Gini coefficient = ',
  round(gini_coeff,3), sep=' '), fontface=2)
ggsave(filename = 'plots/gini_curve.png')


#creem el camp SE a partir de id_hub
for (i in 1:nrow(occ_hubs)){
  SE_list <- str_split(occ_hubs$id_hub[i], "_")
  occ_hubs$SE[i] <- SE_list[[1]][length(SE_list[[1]])-1]
  if (SE_list[[1]][length(SE_list[[1]])-1]=="bio"){occ_hubs$categoria[i] <- 'regulating'}
  if (SE_list[[1]][length(SE_list[[1]])-1]=="hum"){occ_hubs$categoria[i] <- 'provisioning'}
  if (SE_list[[1]][length(SE_list[[1]])-1]=="estetic"){occ_hubs$categoria[i] <- 'cultural'}
  if (SE_list[[1]][length(SE_list[[1]])-1]=="recreat"){occ_hubs$categoria[i] <- 'cultural'}
  if (SE_list[[1]][length(SE_list[[1]])-1]=="hid"){occ_hubs$categoria[i] <- 'regulating'}
  if (SE_list[[1]][length(SE_list[[1]])-1]=="riego"){occ_hubs$categoria[i] <- 'provisioning'}
}


#canviem noms
occ_hubs$SE <- revalue(occ_hubs$SE, c('bio'='Biodiversity', 'hum'='Drinking water',
                                      'estetic' ='Aesthetic values', 
                                      'recreat'='Recreational uses',
                                      'hid'='Water regulation', 
                                      'riego'='Irrigation'))
occ_hubs$Category <- revalue(occ_hubs$categoria, c('cultural'='Cultural',
                                                   'provisioning'='Provisioning',
                                                   'regulating'='Regulating'))

#Gini per Se
SE <- 1
Gini_value <- 1
j <- 1
for (i in unique(occ_hubs$SE)){
  subset <- subset(occ_hubs, SE == i)
  gini_SE <- Gini(subset$Freq)
  SE[j] <- i
  Gini_value[j] <- gini_SE
  j <- j+1
  print(c(i, gini_SE))
}
ginis_SE <- as.data.frame(SE)
ginis_SE$Gini <- Gini_value

#Gini per cat
category <- 1
Gini_value <- 1
j <- 1
for (i in unique(occ_hubs$Category)){
  subset <- subset(occ_hubs, Category == i)
  gini_cat <- Gini(subset$Freq)
  category[j] <- i
  Gini_value[j] <- gini_cat
  j <- j+1
  print(c(i, gini_cat))
}
ginis_cat <- as.data.frame(Category)
ginis_cat$Gini <- Gini_value

#canviem noms
ginis_SE$SE <- revalue(ginis_SE$SE, c('bio'='Biodiversity', 'hum'='Drinking water',
                            'estetic' ='Aesthetic values', 
                            'recreat'='Recreational uses',
                            'hid'='Water regulation', 
                            'riego'='Irrigation'))

# #relació distància i acumulació
# dist_gini <- right_join(dist, occ_hubs, by="id_hub")
# 
# ggplot(dist_gini, aes(x=dist, y=Freq))+geom_point()+geom_smooth()+
#   labs(x='Distance', y='Flow accumulation')

#gini per categoria

ginis_SE <- ginis_SE[order(ginis_SE$Gini, decreasing=TRUE),]

#ordenar igual que la llegenda
gini_results <- paste('Gini coefficients:\n',
                      ginis_SE[1,1],' = ', round(ginis_SE[1,2],3),'\n',
                      ginis_SE[2,1],' = ', round(ginis_SE[2,2],3),'\n',
                      ginis_SE[3,1],' = ', round(ginis_SE[3,2],3),'\n',
                      ginis_SE[4,1],' = ', round(ginis_SE[4,2],3),'\n',
                      ginis_SE[5,1],' = ', round(ginis_SE[5,2],3),'\n',
                      ginis_SE[6,1],' = ', round(ginis_SE[6,2],3),'\n',
                      sep='')

#Gini per Category
Category <- 1
Gini_value <- 1
j <- 1
for (i in unique(occ_hubs$Category)){
  subset <- subset(occ_hubs, Category == i)
  gini_cat <- Gini(subset$id_hub, subset$Freq)
  Category[j] <- i
  Gini_value[j] <- gini_cat
  j <- j+1
  print(c(i, gini_cat))
}
ginis_cat <- as.data.frame(Category)
ginis_cat$Gini <- Gini_value

ggplot(occ_hubs, aes(x=Freq, color=Category)) +
  stat_lorenz(desc = TRUE, size=1) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  hrbrthemes::theme_ipsum() +
  labs(x = "Cumulative percentage of SPU",
       y = "Cumulative Percentage of supplied SBA",
       legend.title='')+
theme(legend.text=element_text(size=10))+
  annotate('text', x=0.98, y=0.15, 
           label='Gini coeffcient:\nCultural=0.297\nProvisioning=0,549\nRegulating=0.365',
           hjust=1, size=4)+theme_set(theme_minimal(base_size = 11))
ggsave(filename = 'plots/gini_curve_cat.png')


mean(occ_hubs$Freq[occ_hubs$Category == 'Provisioning'])
mean(occ_hubs$Freq[occ_hubs$Category == 'Regulating'])
mean(occ_hubs$Freq[occ_hubs$Category == 'Cultural'])
