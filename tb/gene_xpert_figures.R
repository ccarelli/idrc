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

# -----------------------------------------------
# input and output directories
# the input directory imports the data, output saves figures

# change to the folder on your computer where all of the TB data are saved
inDir = paste0("C:/Users/Jaffer/Desktop/PCE/TB/Data/GenXpert/Tb_prepped/")

# create a folder for outputs, including figures and cleaned data sets as RDS files
outDir = paste0("C:/Users/Jaffer/Desktop/PCE/TB/Data/GenXpert/Tb_output/")

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

# ---------------------------------------------------------
# merge the shape file and the data
# this creates maps of all time, rather than by date

# calculate sums and rates at the district level 
dist = dt[ , lapply(.SD, sum, na.rm=T), by=district, .SDcols=c(5:9,12)]

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
# 
# pdf(paste0(outDir, 'tb_sample_figures.pdf'), height=9, width=12)

#------------------------------
# reporting by genexpert sites
# 

# --------------------------------------------------------------
# national time trends of major variables

natl = dt[ , lapply(.SD, sum, na.rm=T), by=date, .SDcols=c(5:9, 12)]

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

#----------------------------------------------
  # rates by region 
  
  # calculate rates 
reg_district = dt[ , lapply(.SD, sum, na.rm=T), by=c('district','date'), .SDcols=c(5:9, 12)]
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





















----------------------------------------------
# rates by region 

# calculate rates 
reg = dt[ , lapply(.SD, sum, na.rm=T), by=region, .SDcols=c(5:9, 12)]
reg[ ,percent_pos:=round(100*tb_positive/samples_tested, 1)]
reg[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
reg[ ,percent_res_total:=round(100*rif_resist/samples_tested, 1)]
reg[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
reg[ ,percent_indet_total:=round(100*rif_indet/samples_tested, 1)]
reg[ ,error_rate:=round(100*total_errors/samples_tested, 1)]

# ---------------------------
# count of total samples and total positive samples
reg_sub = reg[ ,.(samples_tested, tb_positive) , by=region]
reg_sub = melt(reg_sub, id.vars='region')


count_labels = reg[ ,.(samples_tested = sum(samples_tested), tb_positive=sum(tb_positive))]
reg_sub$variable = factor(reg_sub$variable, c('samples_tested', 'tb_positive'),
                          c(paste0('Total samples (n = ', count_labels$samples_tested, ')'),
                            paste0('Positive for TB (n = ', count_labels$tb_positive, ')')))

ggplot(reg_sub, aes(x=reorder(region, -value), y=value, label=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  facet_wrap(~variable, scales='free_y') +
  scale_fill_manual(values=sex_colors) +
  labs(x='Region', y='Count', title='Total samples tested and TB positive samples by region') +
  theme_bw() +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

#---------------------------------------------------------------
# tests performed by implementer

part = dt[ , lapply(.SD, sum, na.rm=T), by=impl_partner, .SDcols=c(5:11, 17)]
part[ ,percent_pos:=round(100*tb_positive/samples_tested, 1)]
part[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
part[ ,percent_res_total:=round(100*rif_resist/samples_tested, 1)]
part[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
part[ ,percent_indet_total:=round(100*rif_indet/samples_tested, 1)]
part[ ,error_rate:=round(100*total_errors/samples_tested, 1)]

# count of total samples and total positive samples by implementer
part_sub = part[ ,.(samples_tested, tb_positive) , by=impl_partner]
part_sub = melt(part_sub, id.vars='impl_partner')

# tb positive samples by implementing partner
count_labels2 = part[ ,.(samples_tested = sum(samples_tested), tb_positive=sum(tb_positive))]
part_sub$variable = factor(part_sub$variable, c('samples_tested', 'tb_positive'),
                           c(paste0('Total samples (n = ', count_labels$samples_tested, ')'),
                             paste0('Positive for TB (n = ', count_labels$tb_positive, ')')))

ggplot(part_sub, aes(x=reorder(impl_partner, -value), y=value, label=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  facet_wrap(~variable, scales='free_y') +
  scale_fill_manual(values=c('#7fbc41', '#de77ae')) +
  labs(x='Implementing partner', y='Count', title='Total samples tested and TB positive samples by implementing partner') +
  theme_bw() +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

#---------------------------------------------------------------
# return to regional graphs 

# ---------------------------
#  percent TB positive
ggplot(reg, aes(x=reorder(region, -percent_pos), y=percent_pos, label=percent_pos, fill=turq)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  scale_fill_manual(values=turq) + 
  theme_minimal() +
  labs(x='Region', y='Percent positive (%)', 
       title="Percentage of total samples that were positive for TB") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

# comparison of percentages within total samples
compare = reg[ ,.(error_rate, percent_pos, percent_res_total, percent_indet_total), by=region]
compare = melt(compare, id.vars='region')

compare$variable = factor(compare$variable, c('error_rate', 'percent_pos', 'percent_indet_total',
                                              'percent_res_total'),
                          c('Errors', 'TB positive', 'Rifampicin indeterminate', 
                            'Rifampicin resistant')) 

# percent TB positive, rifampicin indeterminate, rifampicin resistant
ggplot(compare, aes(x=region, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=ratios) +
  labs(x='Region', y='Percent (%)', 
       title="Percentage of total samples", 
       fill='') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 

# rifampicin indeterminate, rifampicin resistant
ggplot(compare[variable!='TB positive'], aes(x=region, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=rev(brewer.pal(3, 'YlOrRd'))) +
  labs(x='Region', y='Percent (%)', 
       title="Percentage of total samples", 
       fill='') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 

#------------------------------------------------------------------------------------
# MAPS OF PROPORTIONS AND COUNTS 

# calculate sample size for labels
samples = dt[ ,sum(samples_tested)]
retreats = dt[ ,sum(retreatments)]
pos = dt[ ,sum(tb_positive)]
childs = dt[ ,sum(children_under_14)]

# total samples
ggplot(coord, aes(x=long, y=lat, group=group, fill=samples_tested)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Blues')) + 
  theme_void() + 
  labs(title="Total samples by district", subtitle=paste0("n = ", samples,  "; April 1 - 7, 2019"),
       fill="Total samples") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# retreatments 
ggplot(coord, aes(x=long, y=lat, group=group, fill=retreatments)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Greens')) + 
  theme_void() + 
  labs(title="Total retreatments by district", 
       subtitle=paste0("n = ", retreats, "; April 1 - 7, 2019"),
       fill="Retreatments") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# children under 14 
ggplot(coord, aes(x=long, y=lat, group=group, fill=children_under_14)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrRd')) + 
  theme_void() + 
  labs(title="Children under 14 submitting samples by district", subtitle="April 1 - 7, 2019",
       fill=" ") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# tb positive samples 
ggplot(coord, aes(x=long, y=lat, group=group, fill=tb_positive)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Purples')) + 
  theme_void() + 
  labs(title="TB positive samples by district", subtitle=paste0("n = ", pos, "; April 1 - 7, 2019"),
       fill="TB positive (count)") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# rifampicin resistant 
ggplot(coord, aes(x=long, y=lat, group=group, fill=rif_resist)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=c('#fcfbfd', '#a50f15'), breaks = c(0, 1)) + 
  theme_void() + 
  labs(title="Rifampicin resistant samples by district", subtitle="Two districts with one RR sample each; April 1 - 7, 2019",
       fill=" ") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# rifampicin indeterminate
ggplot(coord, aes(x=long, y=lat, group=group, fill=rif_indet)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'GnBu'), breaks = c(0, 3, 6, 9)) + 
  theme_void() + 
  labs(title="Rifampicin indeterminate samples by district", subtitle="Two districts with one RR sample each; April 1 - 7, 2019",
       fill=" ") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# -------------------------------------
# rifampicin resistant and indeterminate

# positive for tb, rif, or indeterminate by region 
pos = reg[ ,.(tb_positive, rif_resist, rif_indet), by=region]

# create totals for labels 
labels = pos[ ,lapply(.SD, sum), .SDcols=2:4]

# calculate ratios, label, and visualize
pos[ , res_ratio:=100*rif_resist/tb_positive]
pos[ , ind_ratio:=100*rif_indet/tb_positive]
pos = pos[ ,.(res_ratio, ind_ratio), by=region]
pos = melt(pos, id.vars='region')
pos[variable=='ind_ratio', variable:=paste0('Rifampicin indeterminate (n = ', labels$rif_indet, ')')]
pos[variable=='res_ratio', variable:=paste0('Rifampicin resistant (n = ', labels$rif_resist, ')')]

# rifampicin indeterminate, rifampicin resistant
ggplot(pos, aes(x=region, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=c('#7fbc41', '#fdb863')) +
  labs(x='Region', y='Percent (%)', 
       title="Percentage of TB positive samples", 
       fill='', subtitle=paste0('n = ', labels$tb_positive)) +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 

# -------------------------------------
# maps of percentages 

# percentage tb positive and rifampicin resistant 
idVars = c('id', 'lat', 'long', 'order', 'hole', 'piece', 'group')
perc = coord[ ,.(percent_pos, percent_res), by=idVars]
perc = melt(perc, id.vars=idVars)

perc$variable = factor(perc$variable, c('percent_pos', 'percent_res'),
                      c('Percentage of tests that were TB positive',
                        'Percentage of TB positive tests that were RR'))

ggplot(perc, aes(x=long, y=lat, group=group, fill=value)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'PuBuGn')) + 
  theme_void() + 
  labs(fill="%") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# percent tb positive
ggplot(coord, aes(x=long, y=lat, group=group, fill=percent_pos)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Blues')) + 
  theme_void() + 
  labs(title = "Percentage of samples that were positive for TB" , fill="%") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# percent RR
ggplot(coord, aes(x=long, y=lat, group=group, fill=percent_res)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Reds')) + 
  theme_void() + 
  labs(title = "Percentage of samples that were Rifampicin resistant" , fill="%") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# percent indeterminate for rifampicin
ggplot(coord, aes(x=long, y=lat, group=group, fill=percent_indet)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
  theme_void() + 
  labs(title = "Percentage of samples that were Rifampicin indeterminate" , fill="%") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# error rateggplot(coord, aes(x=long, y=lat, group=group, fill=error_rate)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=croix) + 
  theme_void() + 
  labs(title = "Error rate", subtitle='Percenate of samples that resulted in an error', 
       fill="Percent errors (%)") +
  theme(text=element_text(size=18), plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()
  
  
#-----------------------------------------
  
# number of machines
  range_mach = paste(dist[ ,range(machines, na.rm=T)][[1]], "-", dist[ ,range(machines, na.rm=T)][[2]])
  
  ggplot(coord, aes(x=long, y=lat, group=group, fill=machines)) + 
    geom_polygon() + 
    geom_path(size=0.01, color="#636363") + 
    scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
    theme_void() + 
    labs(title = "GeneXpert machines by district" , fill="Machines", 
         subtitle=paste0("Range: ", range_mach, " machines per district")) +
    theme(text=element_text(size=18), plot.title=element_text(vjust=-1), plot.subtitle=element_text(vjust=-6)) + coord_fixed()
  


  # count of machines by district - bar
  ggplot(dist, aes(x=reorder(district, -machines), y=machines, label=machines, fill='red')) +
    geom_bar(stat="identity") +
    geom_text(size = 4, position=position_stack(vjust = 1.1)) +
    scale_fill_manual(values='red') + 
    theme_minimal() +
    labs(x='District', y='Count', 
         title="Count of GeneXpert Machines by District") +
    theme(axis.text.x=element_text(angle=90)) +
    guides(fill=FALSE)
  
#   
# dt_new = dt[ ,.(genexpert_site, level, district, machines)]
# write.csv(dt_new, "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/gene_machines.csv")
  
  
# ----------------------------------------------------------
# implementing partners  

# total samples by implementing partner
total_part = sum(part$samples_tested)

ggplot(part, aes(x=reorder(impl_partner, -samples_tested), y=samples_tested, fill='Total samples')) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x='Implementing Partner', y='Total Samples', title='Total samples by implementing partner',
       subtitle=paste0('Total samples: ', total_part), 
       fill=" ") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=F)

# ---------------------------
#  percent TB positive

# percent tb positive by implementing partner
ggplot(part, aes(x=reorder(impl_partner, -percent_pos), y=percent_pos, label=percent_pos, fill=lav)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  scale_fill_manual(values=lav) + 
  theme_minimal() +
  labs(x='Implementing partner', y='Percent positive (%)', 
       title="Percentage of total samples that were positive for TB by implementing partner") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

# comparison of percentages within total samples
compare_part = part[ ,.(error_rate, percent_pos, percent_res_total, percent_indet_total), by=impl_partner]
compare_part  = melt(compare_part , id.vars='impl_partner')

compare_part$variable = factor(compare_part$variable, c('error_rate', 'percent_pos', 'percent_indet_total',
                                                        'percent_res_total'),
                               c('Errors', 'TB positive', 'Rifampicin indeterminate', 
                                 'Rifampicin resistant')) 

# percent TB positive, rifampicin indeterminate, rifampicin resistant
ggplot(compare_part, aes(x=impl_partner, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=ratios) +
  labs(x='Implementing Partner', y='Percent (%)', 
       title="Percentage of total samples by implementing partner", 
       fill='') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 
# ---------------------------

pdf(paste0(outDir, "natl_trends_genexpert.pdf"))

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}

dev.off()

# ---------------------------






