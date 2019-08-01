# ----------------------------------------------
# GENEXPERT DATA ANALYSIS
# Create a map of the number of machines per district
#
# Caitlin O'Brien-Carelli
# 4/30/2019
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
library(raster)
library(maptools)
library(LaCroixColoR)
# --------------------

#------------------------------------------------
# input and output directories
# the input directory imports the data, output saves figures

# change to the folder on your computer where all of the TB data are saved
inDir = paste0("C:/Users/ccarelli/Documents/tb_prepped/")

# create a folder for outputs, including figures and cleaned data sets as RDS files
outDir = paste0("C:/Users/ccarelli/Documents/tb_outputs/")

# -----------------------------------------------
# import the prepped data 

dt = readRDS(paste0(inDir, 'clean_genexpert_data.rds'))

# -----------------------------------------------
# prepare the districts to merge the data with the shape file

# some of these districts have new names, and some are misspelled 
merge_new_dists = function(x) {
  x[district=="Bunyangabu" | district=='Bunyangabo', district:="Kabarole"]
  x[district=="Butebo" | district=='Palisa', district:="Pallisa"]
  x[district=="Kagadi", district:="Kibaale"]
  x[district=="Kakumiro", district:="Kibaale"]
  x[district=="Kyotera", district:="Rakai"]
  x[district=="Namisindwa", district:="Manafwa" ]
  x[district=="Omoro", district:="Gulu"]
  x[district=="Pakwach", district:="Nebbi"]
  x[district=="Rubanda", district:= "Kabale"]
  x[district=="Rukiga", district:="Kabale"]
  x[district=="Luwero", district:="Luweero"]
  x[district=="Sembabule", district:="Ssembabule"]
  x[district=="Kabalore", district:="Kabarole"]
  x[district=="Busiisa", district:="Buliisa"]
  x[district=="Bundibujo", district:="Bundibugyo"]
  x[district=="Kaberameido", district:="Kaberamaido"]
  x[district=='Maddu', district:='Gomba']  
  x[district=='Nabilatuk', district:='Nakapiripirit']
}

# run the function on your data set to fix the names
dt = merge_new_dists(dt)

# ----------------------------
# prepare data and shape file to create maps 

# uganda shapefile
map = shapefile('C:/Users/ccarelli/Documents/uga_shape/uga_dist112_map.shp')

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(map, region='dist112')) 
coord[, id:=as.numeric(id)]

# check the names for the merge
map_names = data.table(map@data[c('dist112', 'dist112_na')])
names = dt[ ,unique(district)]
names[!(names %in% map_names$dist112_na)] # this value should be 0

# there is one district not in the data - kyankwanzi
map_names[!(dist112_na %in% names)]

# merge the names
setnames(map_names, c('dist112_na', 'dist112'), 
         c('district', 'id'))

# ---------------------------------------------------------
# merge the shape file and the data
# this creates maps of all time, rather than by date

districts = c("Amudat", "Sheema", "Nakapiripirit", "Apac", "Kole", "Mitooma")
mach = dt[ ,.(machines=sum(machines)), by=.(district)]
mach[ , selected:=(district %in% districts)]

#---------------------------------------------
# merge in the shape file

# merge in the district ids
mach_coord = merge(mach, map_names, by='district', all.x=T)

# merge on id with the shape file
mach_coord = merge(mach_coord, coord, by='id', all=T)

#---------------------------------------------
# map of the districts

mach_coord[is.na(selected), selected:=FALSE]
mach_coord[selected==T, sel_2:='Selected district']
mach_coord[selected==F, sel_2:='Not selected']
bi_colors = c('#e5f5e0', '#006837')

# districts selected
ggplot(mach_coord, aes(x=long, y=lat, group=group, fill=sel_2)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_manual(values = bi_colors)+
  theme_void()+ 
  labs(fill="")+
  theme(text=element_text(size=22)) + coord_fixed()

#---------------------------------------------
# number of machines by district

mach_coord_alt = copy(mach_coord)
mach_coord_alt[selected==F, machines:=NA]

mach_coord_alt[ ]

mach_coord_alt[machines==1, machine_label:='One machine']
mach_coord_alt[machines==2, machine_label:='Two machines']

# total samples
ggplot(mach_coord_alt, aes(x=long, y=lat, group=group, fill=machines)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  theme_void() + 
  labs(title="GeneXpert Machines by district",
       fill="Machines") +
  theme(text=element_text(size=18)) + coord_fixed()

#---------------------------------------------

# total samples
ggplot(mach_coord_alt, aes(x=long, y=lat, group=group, fill=test)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  theme_void() + 
  labs(title="GeneXpert Machines by district",
       fill="Machines") +
  theme(text=element_text(size=18)) + coord_fixed()

#---------------------------------------------


# total samples
ggplot(mach_coord, aes(x=long, y=lat, group=group, fill=test)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Blues'), breaks=c(1 , 2)) + 
  theme_void() + 
  labs(title="Machines by district",
       fill="Machines") +
  theme(text=element_text(size=18)) + coord_fixed()

#---------------------------------------------








