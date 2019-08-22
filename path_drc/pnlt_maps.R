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
# set working directories


# local directory - reset this to your directory

dt = readRDS("C:/Users/ckingongo/Documents/Atelier TB 2019/Data tb/tb_vih_analysis.rds")

# set working directory to the location of the shape file 
#setwd('C:/Users/ccarelli/Documents/drc_shape_files/')
setwd("C:/Users/ckingongo/Documents/CARTOGRAPHIES/drc_shape_files/")

# read in the data locally
#dt = readRDS(paste0(dir, "tb_vih_analysis.rds"))

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

dt[dps=="kongo_central_ouest", dps:="kongo_central"]
dt[dps=="kongo_central_est", dps:="kongo_central"]

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
dps[ ,tx:=round(tb_teste_vih/total_de_cas*100,1) ,by=.(dps,year)]



#---------------------------------------------------
# the hardest part of making maps 

map_names = coord[ ,unique(id)] # names of dps in the shape file
data_names = dps[ ,unique(dps)] # names of dps in the data
data_names[!(data_names %in% map_names)]

# change the names in the shape file 
coord[id=="Bas-U????l????", id:="bas_uele"]
coord[id=="?????quateur" , id:="equateur"]
coord[id=="Haut-Lomami" , id:="haut_lomami"]
coord[id=="Haut-U????l????" , id:="haut_uele"]
coord[id=="Kasa????", id:="kasai"]
coord[id=="Kasa????-Central", id:="kasai_central"]
coord[id=="Ma????-Ndombe", id:="mai_ndombe"]
coord[id=="Nord-Kivu", id:="nord_kivu"]
coord[id=="Nord-Ubangi", id:="nord_ubangi"]
coord[id=="Sud-Kivu", id:="sud_kivu"]
coord[id=="Sud-Ubangi", id:="sud_ubangi"]
coord[id=="Kasa????-Oriental", id:="kasai_oriental"]
coord[grep("ateur", id), id:="equateur"]
coord[id=="Kongo-Central", id:="kongo_central"]
coord[id=="Haut-Katanga", id:="haut_katanga"]
coord[id=="Kasai Oriental", id:="kasai_oriental"]
coord[id=="Bas-Uélé", id:="bas_uele"]
coord[id=="Lomami", id:="lomami"]
coord[id=="Maï-Ndombe", id:="mai_ndombe"]
coord[id=="Kwango", id:="kwango"]
coord[id=="Haut-Uélé", id:="haut_uele"]
coord[id=="Lualaba", id:="lualaba"]
coord[id=="Kasaï-Central", id:="kasai_central"]
coord[id=="Kwilu", id:="kwilu"]
coord[id=="Ituri", id:="ituri"]
coord[id=="Maniema", id:="maniema"]
coord[id=="Sankuru", id:="sankuru"]
coord[id=="Tshopo", id:="tshopo"]
coord[id=="Kasaï-Oriental", id:="kasai_oriental"]
coord[id=="Kinshasa", id:="kinshasa"]
coord[id=="Kasaï", id:="kasai"]
coord[id=="Tanganyika", id:="tanganyika"]
coord[id=="Tshuapa", id:="tshuapa"]
coord[id=="Mongala", id:="mongala"]
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
p1<-ggplot(coord, aes(x=long, y=lat, group=group, fill=total_de_cas)) + 
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
p2<-ggplot(coord[id!='kinshasa'], aes(x=long, y=lat, group=group, fill=total_de_cas)) + 
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

p3<-ggplot(coord, aes(x=long, y=lat, group=group, fill=tb_teste_vih)) + 
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
p4<-ggplot(coord[], aes(x=long, y=lat, group=group, fill=tb_vih_positif)) + 
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
#
# Map percentage of tb cases tested for hiv
p5<-ggplot(coord, aes(x=long, y=lat, group=group, fill=tx)) + 
  geom_polygon() + 
  scale_fill_gradientn('percentage of tb cases tested for hiv', colours=brewer.pal(9, 'Spectral')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='percentage tb tested hiv(%), 2018', 
       caption='Source: PNLT data') + 
  theme_minimal(base_size=20) + 
  theme(plot.caption=element_text(size=14))


