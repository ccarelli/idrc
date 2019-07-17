# ----------------------------------------------
# GENEXPERT DATA ANALYSIS
# Create maps and graphs for GeneXpert data
#
# 
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

# -----------------------------------------------
# input and output directories
# the input directory imports the data, output saves figures

# change to the folder on your computer where all of the TB data are saved
inDir = paste0("D:/PCE/TB/Data/GeneXpert/Tb_prepped/")

# create a folder for outputs, including figures and cleaned data sets as RDS files
outDir = paste0("D:/PCE/TB/Data/GeneXpert/Tb_output/")

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
   x[district=='Nakapiripiti', district:='Nakapiripirit']
    }

# run the function on your data set to fix the names
dt = merge_new_dists(dt)

# ----------------------------
# prepare data and shape file to create maps 

# uganda shapefile
map = shapefile('D:/shapefiles/uga_shape/uga_dist112_map.shp')

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

# drop out the extra format variable
dt[ ,c('test', 'quarter', 'year', 'error_rate', 'average_util'):=NULL]

# trim white space for regions
dt[ ,region:=trimws(region)]

# ---------------------------------------------------------
# merge the shape file and the data
# this creates maps of all time, rather than by date

# calculate sums and rates at the district level 
dist = dt[ , lapply(.SD, sum, na.rm=T), by=district, .SDcols=c(6:10,14)]

dist[ ,percent_pos:=round(100*tb_positive/samples_tested, 1)]
dist[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
dist[ ,percent_res_total:=round(100*rif_resist/samples_tested, 1)]
dist[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
dist[ ,percent_indet_total:=round(100*rif_indet/samples_tested, 1)]
dist[ ,error_rate:=round(100*total_errors/samples_tested, 1)]

# merge the map and the data using district ids
dist = merge(dist, map_names, by='district', all=T)
coord = merge(dist, coord, by='id', all=T)

#---------------------------------------------

#----------------------------------------------------------------------
# aesthetic properties for maps and graphs
# create some color palettes for lovely figures

bar_colors = c('GeneXpert Sites'='#9ecae1', 'Reported'='#fc9272')
alt_bar_colors = c('GeneXpert Sites'='#fee090', 'Reported'='#b2abd2')
true_false = c('#bd0026', '#3182bd')
single_red = '#bd0026'
single_blue = '#a6bddb'
turq = '#80cdc1'
ratios = c('#bd0026', '#3182bd', '#74c476', '#feb24c')
sex_colors = c('#bd0026', '#3182bd', '#74c476', '#8856a7')
lav = '#bebada'
croix = brewer.pal(9, 'Reds')

#-------------------------------------------------------------
# CREATE MAPS AND GRAPHS 

# --------------------------------------------------------------
# exploratory graphs

#------------------------------
# reporting by genexpert sites
# 

# --------------------------------------------------------------
# national time trends of major variables

# --------------------------------------------------
# diagnostic graph of machines over time

mach = dt[ ,.(machines=sum(machines)), by=date]

pdf(paste0(outDir,"Machines_over_time.pdf"))
# graph of machines in uganda over time 
ggplot(mach, aes(x=date, y=machines))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title = "GeneXpert Machines Over Time", x="Date", y="Number of machines",
       caption = "Source: NTLP GeneXpert data")+
  theme(text = element_text(size=18))
dev.off()
# --------------------------------------------------
# national time trends 

natl = dt[ , lapply(.SD, sum, na.rm=T), by=date, .SDcols=c(6:10,14)]

# add ratios for each
natl[ ,percent_pos:=round(100*tb_positive/samples_tested, 1)]
natl[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
natl[ ,percent_res_total:=round(100*rif_resist/samples_tested, 1)]
natl[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
natl[ ,percent_indet_total:=round(100*rif_indet/samples_tested, 1)]
natl[ ,error_rate:=round(100*total_errors/samples_tested, 1)]

# shape the data long
natl_long = melt(natl, id.vars='date')

# add a count or percent condition
count_vars = c('samples_tested', 'tb_pos_rif_neg', 'rif_resist',   
               'rif_indet', 'total_errors','tb_positive')
natl_long[variable %in% count_vars, count:=TRUE]
natl_long[is.na(count), count:=FALSE]

# factor the labels for the graphs
natl_long$variable = factor(natl_long$variable, c('samples_tested', 'tb_pos_rif_neg', 'rif_resist',   
                        'rif_indet', 'total_errors','tb_positive',
                        'percent_pos', 'percent_res', 'percent_res_total', 'percent_indet',      
                        'percent_indet_total', 'error_rate'),
       c('Samples tested for TB', 'TB positive samples that were Rifampicin negative',
         'Rifampicin resistant', 'Rifampicin indeterminate', 'Total errors', 
         'Total TB positive', 'Percent TB positive of all samples tested (%)', 'Percent RR out of TB positive (%)',
         'Percent RR of all samples tested (%)', 'Percent of TB+ samples that were RR indeterminate (%)',
         'Percent of all samples that were RR indeterminate (%)', 'Error rate (%)'))
var_names = natl_long[ ,unique(variable)]

list_of_plots = NULL
i = 1

for (v in var_names) {
  title = as.character(v)
  
  if (natl_long[variable==v]$count==T) {
    total_n = natl_long[variable==v, sum(value)]
    subtitle=paste0('Total n = ', total_n)
    y_name = 'Count'
  } else { 
    mean_percent = natl_long[variable==v, round(mean(value), 1)]
    subtitle = paste0("Mean percent per quarter: ", mean_percent, "%")
    y_name = 'Percent (%)'
  }
  
  list_of_plots[[i]] = ggplot(natl_long[variable==v], aes(x=date, y=value)) + 
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x='Date', y=y_name, title=title, 
         subtitle=subtitle, caption='Source: NTLP GeneXpert')
    
  i = i+1
}

# print the pdf - one page for each district
pdf(paste0(outDir, "/genexpert_tb_natl.pdf"),width=12,height=9)

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}
dev.off()

#----------------------------------------------
# rates by district 

# calculate rates 
reg_district = dt[ , lapply(.SD, sum, na.rm=T), by=c('district','date'), .SDcols=c(6:10,14)]
reg_district[ ,percent_pos:=round(100*tb_positive/samples_tested, 1)]
reg_district[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
reg_district[ ,percent_res_total:=round(100*rif_resist/samples_tested, 1)]
reg_district[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
reg_district[ ,percent_indet_total:=round(100*rif_indet/samples_tested, 1)]
reg_district[ ,error_rate:=round(100*total_errors/samples_tested, 1)]

# ---------------------------
# count of total samples and total positive samples
reg_sub_district = reg_district[ ,.(samples_tested, tb_positive, percent_pos) , by=c('district','date')]
reg_sub_district = melt(reg_sub_district, id.vars=c('district','date'))

count_labels = dt[ ,.(samples_tested = sum(samples_tested), tb_positive=sum(tb_positive))]
reg_sub_district$variable = factor(reg_sub_district$variable, c('samples_tested', 'tb_positive',
                                                                'percent_pos'),
                          c('Total samples','Positive for TB', 'Percent positive (%)'))

tri_colors = c('#d73027', '#74add1', '#af8dc3')

reg_sub_district = reg_sub_district[!is.na(district)]

# create a pdf of every district's reporting completeness
dist_list_of_plots = NULL
i = 1

for (d in unique(reg_sub_district$district)) {
  
  district_name = as.character(d)  
  
  dist_list_of_plots[[i]] = ggplot(reg_sub_district[district==d], aes(x=date, y=value, color=variable))+
    geom_point()+
    geom_line()+
    facet_wrap(~variable,scales="free")+
    scale_color_manual(values=tri_colors)+
    theme_bw() +
        labs(title=paste0("Samples tested and Tb positive: ", district_name),
         x="Date", y="", color="")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outDir, "/genexpert_tb_district.pdf"),width=12,height=9)

for(i in seq(length(dist_list_of_plots))){
  print(dist_list_of_plots[[i]])
}
dev.off()

#---------------------