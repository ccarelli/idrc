# Code to create maps from PNLT
# Caitlin O'Brien-Carelli, Constant Kingongo
# 8/21/2019
# ----------------------------------------------

# --------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(maptools)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(grid)
# --------------------

# --------------------
# set the user so caitlin can sometimes use your code

user = 'caitlin'

# set working directories
dir = "C:/Users/ccarelli/Documents/pnls_data/"
dt = readRDS(paste0(dir, "tb_hiv_ready_for_analysis.rds"))
setwd('C:/Users/ccarelli/Documents/drc_shape_files/')
  
# subset the data table to only a single year
# you can change this year
year_set = 2018
dt = dt[year(date)==year_set]

# fix the total population variable
dt[ ,population_totale:=as.numeric(population_totale)]

#---------------------------------------------------
# import the shape file and format it to make maps

# import the dps level shapefile
map = shapefile('gadm36_COD_1.shp')

# prepare the map for analysis
coord = fortify(map, region='NAME_1')
coord = data.table(coord)

#---------------------------------------------------
# sum the data to the DPS level
# the data are currently at the facility level

dt[grep("Kongo", dps), dps:="Kongo Central"]

dps = dt[ ,.(population_totale=sum(population_totale, na.rm=T), 
             population_couverte=sum(population_couverte, na.rm=T),
             total_de_cas_incident=sum(total_de_cas_incident, na.rm=T), 
             total_de_cas=sum(total_de_cas, na.rm=T), 
             tb_teste_vih=sum(tb_teste_vih, na.rm=T), tb_vih_positif=sum(tb_vih_positif, na.rm=T),
             tb_vih_positif_tarv=sum(tb_vih_positif_tarv, na.rm=T),
             pvvih_sous_inh = sum(pvvih_sous_inh, na.rm=T)), by=.(dps, year)]

#---------------------------------------------------
# calculate the percentage of tb cases tested for hiv
# round to one decimal place 

dps[ , tx:=round(tb_teste_vih/total_de_cas*100,1) ,by=.(dps,year)]

#---------------------------------------------------
# the hardest part of making maps 

map_names = coord[ ,unique(id)] # names of dps in the shape file
data_names = dps[ ,unique(dps)] # names of dps in the data
data_names[!(data_names %in% map_names)]


# change the names in the shape file 
coord[id=="Bas-Uélé", id:="Bas Uele"]
coord[id=="?????quateur" , id:="Equateur"]
coord[id=="Haut-Lomami" , id:="Haut Lomami"]
coord[id=="Haut-Uélé" , id:="Haut Uele"]
coord[id=="Kasaï", id:="Kasai"]
coord[id=="Kasaï-Central", id:="Kasai Central"]
coord[id=="Maï-Ndombe", id:="Mai Ndombe"]
coord[id=="Nord-Kivu", id:="Nord Kivu"]
coord[id=="Nord-Ubangi", id:="Nord Ubangi"]
coord[id=="Sud-Kivu", id:="Sud Kivu"]
coord[id=="Sud-Ubangi", id:="Sud Ubangi"]
coord[id=="Kasaï-Oriental", id:="Kasai Oriental"]
coord[grep("ateur", id), id:="Equateur"]
coord[id=="Kongo-Central", id:="Kongo Central"]
coord[id=="Haut-Katanga", id:="Haut Katanga"]
coord[id=="Kasai Oriental", id:="Kasai Oriental"]

# check again
map_names = coord[ ,unique(id)]
data_names = dps[ ,unique(dps)]
data_names[!(data_names %in% map_names)]

#---------------------------------------------------
# merge the data and the shape file

setnames(dps, 'dps', 'id')
coord = merge(coord, dps, by='id', all=T)

#---------------------------------------------------
# graphs of the number of tb cases

# total number of tb cases, 2018
ggplot(coord, aes(x=long, y=lat, group=group, fill=total_de_cas)) + 
  geom_polygon() + 
  scale_fill_gradientn('Total TB cases', colours=brewer.pal(9, 'Blues')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Total TB cases, 2018', 
       caption='Source: PNLT data') + 
  theme_minimal(base_size=20) + 
  theme(plot.caption=element_text(size=14)) 

# total number of tb cases, 2018, excluding kinshasa for scale
ggplot(coord[id!='kinshasa'], aes(x=long, y=lat, group=group, fill=total_de_cas)) + 
  geom_polygon() + 
  scale_fill_gradientn('Total TB cases, 2018, excluding Kinshasa for scale', 
                       colours=brewer.pal(9, 'Blues')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Total TB cases, 2018', 
       caption='Source: PNLT data') + 
  theme_minimal(base_size=20) + 
  theme(plot.caption=element_text(size=14)) 

#---------------------------------------------------
# now make a map of the total number of tb cases tested for hiv
# use 'coord' and the variable 'tb_teste_vih'

ggplot(coord, aes(x=long, y=lat, group=group, fill=tb_teste_vih)) + 
  geom_polygon() + 
  scale_fill_gradientn('total number of tb cases tested for hiv', colours=brewer.pal(9, 'Reds')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='tb cases tested for hiv, 2018', 
       caption='Source: PNLT data') + 
  theme_minimal(base_size=20) + 
  theme(plot.caption=element_text(size=14))

#---------------------------------------------------
#tb_vih_positif
-ggplot(coord[], aes(x=long, y=lat, group=group, fill=tb_vih_positif)) + 
  geom_polygon() + 
  scale_fill_gradientn('total number of tb cases tested for hiv positif', colours=brewer.pal(9, 'Reds')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='tb vih positif, 2018', 
       caption='Source: PNLT data') + 
  theme_minimal(base_size=20) + 
  theme(plot.caption=element_text(size=14))
#---------------------------------------------------
# Map percentage of tb cases tested for hiv


ggplot(coord, aes(x=long, y=lat, group=group, fill=tx)) + 
  geom_polygon() + 
  scale_fill_gradientn('Percent (%)', colours=brewer.pal(9, 'Spectral')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Percentage TB cases tested for HIV, 2018', 
       caption='Source: PNLT program data') + 
  theme_minimal(base_size=20) + 
  theme(plot.caption=element_text(size=14))

# --------------------------------------------------------------------
# create labels for the regional maps

# identify centroids and label them
names = data.table(coordinates(map))
setnames(names, c('long', 'lat'))
names[ , id:=unique(coord$id)]

dps[ , label:=paste0(id, ": ", tx, "%")]
labels = dps[ ,.(id, label)]
names = merge(names, labels, by='id', all=T)

pdf(paste0(dir, 'tb_tested_map.pdf'), width=12, height=9)

ggplot(coord, aes(x=long, y=lat, group=group, fill=tx)) + 
  geom_polygon() + 
  scale_fill_gradientn('Percent (%)', colours=brewer.pal(9, 'RdYlBu')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Percentage TB cases tested for HIV, 2018', 
       caption='Source: PNLT program data') + 
  theme_minimal(base_size=20) + 
  theme(text = element_text(size=18)) +
geom_label_repel(data = names, aes(label = label, x = long, y = lat, group = id), inherit.aes=FALSE, size=5)


dev.off()






