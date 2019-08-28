# ----------------------------------------------
# GENEXPERT DATA ANALYSIS
# Create maps and graphs for GeneXpert data
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
# to code in the same code, we need to set file pathways for each user
# this command automatically detects your username!

user = Sys.info()[['user']]

# -----------------------------------------------
# input and output directories
# the input directory imports the data, output saves figures

# change to the folder on your computer where all of the TB data are saved
inDir = paste0("C:/Users/", user, "/Documents/tb_prepped/")

# create a folder for outputs, including figures and cleaned data sets as RDS files
outDir = paste0("C:/Users/", user, "/Documents/tb_outputs/")

# set working directory to the location of your shape file 
# this allows you to create maps
setwd('J:/Project/Evaluation/GF/mapping/uga/')

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
map = shapefile('uga_dist112_map.shp')

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(map, region='dist112')) 
coord[, id:=as.numeric(id)]

# check the names for the merge
map_names = data.table(map@data[c('dist112', 'dist112_na')])
names = dt[ ,unique(district)]
names[!(names %in% map_names$dist112_na)] # this value should be 0

# there is on district not in the data - kyankwanzi
map_names[!(dist112_na %in% names)]

# merge the names
setnames(map_names, c('dist112_na', 'dist112'), 
         c('district', 'id'))

# ---------------------------------------------------------