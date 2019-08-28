
#creating a rate of change maps for testing / treatment coverage 
#-------------------------------------------------------------------
# Set up R
rm(list=ls())
library(reshape2)
library(ggplot2)
library(foreign)
library(tidyr)
library(stats4)
library(caret)
library(dplyr)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(lubridate)
library(rgeos)
library(raster)
library(rgdal)
library(tibble)
library(maptools)
##-disable scientific notation
options(scipen=999)

#align to original districts in 2015
merge_new_dists <- function(x) {
  x[district_name=="Bunyangabu", district_name:="Kabarole"]
  x[district_name=="Butebo", district_name:="Pallisa"]
  x[district_name=="Kagadi", district_name:="Kibaale"]
  x[district_name=="Kakumiro", district_name:="Kibaale"]
  x[district_name=="Kyotera", district_name:="Rakai"]
  x[district_name=="Namisindwa", district_name:="Manafwa" ]
  x[district_name=="Omoro", district_name:="Gulu"]
  x[district_name=="Pakwach", district_name:="Nebbi"]
  x[district_name=="Rubanda", district_name:="Kabale"]
  x[district_name=="Rukiga", district_name:="Kabale"]
  x[district_name=="Luwero", district_name:="Luweero"]
  x[district_name=="Sembabule", district_name:="Ssembabule"]
  x[district_name=="Bugweri", district_name:="Iganga"]
  x[district_name=="Kapelebyong", district_name:="Amuria"]
  x[district_name=="Kassanda", district_name:="Mubende"]
  x[district_name=="Kikuube", district_name:="Hoima"]
  x[district_name=="Kwania", district_name:="Apac"]
  x[district_name=="Palisa", district_name:="Pallisa"]
  x[district_name=="Nakapiripiti", district_name:="Nakapiripirit"]
  x[district_name=="Kaberameido", district_name:="Kaberamaido"]
  x[district_name=="LIRA", district_name:="Lira"]
  x[district_name=="BUNDIBUGYO", district_name:="Bundibugyo"]
  x[district_name=="Bundibujo", district_name:="Bundibugyo"]
  x[district_name=="Nabilatuk", district_name:="Nakapiripirit"]
  x[district_name=="Bunyangabo", district_name:="Kabarole"]
  x[district_name=="Kabalore", district_name:="Kabarole"]
  x[district_name=="Kaberameido", district_name:="Kaberamaido"]
  x[district_name=="Busiisa", district_name:="Buliisa"]
  x[district_name=="Maddu", district_name:="Gomba"]
  
}

inDir="D:/PCE/Malaria/"
outputs="D:/PCE/Malaria_outputs/"
setwd(inDir)
mala<-read.csv("mal_wep_district_quarterly.csv")
mala<-data.table(mala)

# grab the year from quarterly data 
mala$periodcode<-as.character(mala$periodcode)
mala$organisationunitname<-as.character(mala$organisationunitname)
mala[ ,year:=unlist(lapply(strsplit(periodcode, "Q"), "[" ,1))]

mala[ ,mm:=unlist(lapply(strsplit(periodcode, ""), "[" ,6))]

mala[mm==4,qtr:='10']
mala[mm==3,qtr:='07']
mala[mm==2,qtr:='04']
mala[mm==1,qtr:='01']

mala[ , Date:=paste0(year, '-', qtr, '-01')]
mala[ , Date:=ymd(Date)]



mala[ ,district_name:=unlist(lapply(strsplit(organisationunitname, " "), "[" ,1))]

mala[,c("organisationunitid","organisationunitdescription","periodid","periodname",
        "perioddescription","organisationunitcode","organisationunitname","periodcode","qtr","mm"):=NULL]

new_col=c("Suspected_malaria_fever","Microscopy_positive_cases","RDT_positive_cases","Microscopy_tested_cases",
          "RDT_tested_cases","Microscopy_negative_treated","Microscopy_positive_treated","RDT_positive_treated",
          "Not_tested_treated","RDT_negative_treated","year","Date","district_name")

setnames(mala,new_col)

#------------------------------------------------------------------------------------------------------------------
#miscopy positive Kyenjojo for 2016Q4 as average of 2016Q3 2016Q4 2017Q1
mala[district_name=='Kyenjojo' & Microscopy_positive_treated==972566,Microscopy_positive_treated:=round((4198+3410)/2,0)]
#for 2018Q2 as average of 2018Q1 and 2018Q3
mala[district_name=='Bushenyi' & Microscopy_positive_treated==676925,Microscopy_positive_treated:=round((119+1290)/2,0)]


#generate malaria_testing and malaria_treating
mala[,malaria_testing:=(Microscopy_tested_cases + RDT_tested_cases)]
mala[,malaria_treating:=(Microscopy_negative_treated + Microscopy_positive_treated + RDT_positive_treated + RDT_negative_treated +Not_tested_treated)]
#mala<-mala[year=='2017' | year=='2018']

mala<-merge_new_dists(mala)

#-------------------------for overall trends for all parameters in this sub data
overall_mala_long<-melt(mala,id.vars = c("Date"))
overall_mala_long$value<-as.numeric(overall_mala_long$value)
overall_mala_long<-overall_mala_long[,.(tot=sum(value,na.rm=T)),by=c("variable","Date")]

mala_long<-melt(mala,id.vars = c("district_name","year"))

#------------------------------------------------------------------------------------------------
#change kabale outlier of 2014Q1 to average of 2013q4 and 2014q2
mala_long[district_name=='Kabale' & value==2000479,value:=round((569+786)/2,0)]
#miscopy positive Kyenjojo for 2016Q4 as average of 2016Q3 2016Q4 2017Q1
#mala_long[district_name=='Kyenjojo' & value==972566,value:=round((4198+3410)/2,0)]
# RDT_positive_cases Lwengo for 2017Q3 as average of 2017Q2 and 2017Q4
mala_long[district_name=='Lwengo' & value==808137,value:=round((20979+4770)/2,0)]

#Apac suspected malaria fever for 2018Q1 with average of 2017Q4 and 2018Q2
mala_long[district_name=='Apac' & value==760830,value:=round((82435+15558)/2,0)]

mala_long<-mala_long[,.(tot=sum(value,na.rm=T)),by=c("district_name","variable","year")]

mala_2014<-mala_long[year=='2014',.(district_name,variable,value2014=tot)]
mala_2015<-mala_long[year=='2015',.(district_name,variable,value2015=tot)]
mala_2016<-mala_long[year=='2016',.(district_name,variable,value2016=tot)]
mala_2017<-mala_long[year=='2017',.(district_name,variable,value2017=tot)]
mala_2018<-mala_long[year=='2018',.(district_name,variable,value2018=tot)]
mala_2019<-mala_long[year=='2019',.(district_name,variable,value2019=tot)]

mala_45<-merge(mala_2014,mala_2015,by=c("district_name","variable"))
mala_456<-merge(mala_45,mala_2016,by=c("district_name","variable"))
mala_4567<-merge(mala_456,mala_2017,by=c("district_name","variable"))
mala_45678<-merge(mala_4567,mala_2018,by=c("district_name","variable"))
mala_rates<-merge(mala_45678,mala_2019,by=c("district_name","variable"))


mala_rates<-mala_rates[,.(rate2018=100*((value2018-value2017)/value2017),
           rate2019=100*((value2019-value2018)/value2018),
           rate2017=100*((value2017-value2016)/value2016),
           rate2016=100*((value2016-value2015)/value2015),
           rate2015=100*((value2015-value2014)/value2014)),
           by=c("district_name","variable")]

setnames(mala_rates,"variable","indicator")

mala_rates_long<-melt(mala_rates,id.vars =c("district_name","indicator") )

mala_rates_long$variable<-as.character(mala_rates_long$variable)
mala_rates_long[ ,year:=paste0("201",unlist(lapply(strsplit(variable, ""), "[" ,8)))]

#------------------------------------------------------------------------------
#malaria testing

mala_testing<-mala_rates_long[indicator=='malaria_testing']

# create a pdf of every district's reporting completeness
list_of_plots = NULL
i = 1

for (d in unique(mala_testing$district_name)) {
  
  district = as.character(d)  
  
  list_of_plots[[i]] = ggplot(mala_testing[district_name==d], aes(x=year, y=value,group=1))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Rate of change for: ", district),
         x="Date", y="Rate (%)")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outputs, "/Rate_of_change_for_each_district.pdf"))

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}
dev.off()

#---------------------------------------------------------------
#------------------------------------------------------------------------------
#malaria testing

mala_treating<-mala_rates_long[indicator=='malaria_treating']

# create a pdf of every district's reporting completeness
list_of_plots_treat = NULL
i = 1

for (d in unique(mala_treating$district_name)) {
  
  district = as.character(d)  
  
  list_of_plots_treat[[i]] = ggplot(mala_treating[district_name==d], aes(x=year, y=value,group=1))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Malaria treatment rate of change for: ", district),
         x="Date", y="Rate (%)")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outputs, "/treatment_rate_of_change_for_each_district.pdf"))

for(i in seq(length(list_of_plots_treat))){
  print(list_of_plots_treat[[i]])
}
dev.off()







#---------------------------------------------------------------------------------------------
shapeData <- shapefile("D:/shapefiles/uga_shape/uga_dist112_map.shp")

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)] 

as_tibble(shapeData@data) 


# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)

coordinates_yr <- rbind(coordinates,coordinates,coordinates)

# this uses the repeat function: basically it repeats year over and over
coordinates_yr[, year:=rep(2017:2019, each=nrow(coordinates))]
coordinates_yr<-merge(coordinates_yr,shape_names,by="id")
coordinates_yr$year<-as.character(coordinates_yr$year)

#---------------
#malaria treatment
map_rate_treat<-mala_treating[year=='2017'| year=='2018'| year=='2019']
map_rate_treat<-merge(map_rate_treat,coordinates_yr,by=c("district_name","year"))

map_rate_test<-mala_testing[year=='2017'|year=='2018'| year=='2019']
map_rate_testt<-merge(map_rate_test,coordinates_yr,by=c("district_name","year"))


#-----------------------------------------------------------------
#
ratios1 <- brewer.pal(8, 'Blues')
ratios2 <- brewer.pal(8, 'Greens')
ratios3 <- brewer.pal(8, 'Oranges')
ratios4 <- brewer.pal(8, 'Reds')


pdf(paste0(outputs,"Maps_of_rate_of_malaria_changes.pdf"))
ggplot(map_rate_testt, aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Rate of malaria testing change",
       caption="Source: HMIS",Subtitle="2017-2019",
       fill="Rate")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(map_rate_treat, aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Rate of malaria treatment change",
       caption="Source: HMIS",Subtitle="2017-2018",
       fill="Rate")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
dev.off()

#------------------------------------------------------------------------------
#all overall trends for all parameters
#dataset overall_mala_long

# create a pdf of every district's reporting completeness
overall_mala_long<-overall_mala_long[variable!='year' & variable!='district_name']
list_of_plots_all = NULL
i = 1

for (d in unique(overall_mala_long$variable)) {
  
  indicator = as.character(d)  
  
  list_of_plots_all[[i]] = ggplot(overall_mala_long[variable==d], aes(x=Date, y=tot,group=1))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Count of : ", indicator),
         x="Date", y="Count")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outputs, "/overall_figures_in_wep.pdf"))

for(i in seq(length(list_of_plots_all))){
  print(list_of_plots_all[[i]])
}
dev.off()
#---------------------------------------------------------------
overall_mala_long14<-overall_mala_long[Date>='2014-01-01']

# create a pdf of every district's reporting completeness
list_of_plots_all14 = NULL
i = 1

for (d in unique(overall_mala_long14$variable)) {
  
  indicator = as.character(d)  
  
  list_of_plots_all14[[i]] = ggplot(overall_mala_long14[variable==d], aes(x=Date, y=tot,group=1))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Count of : ", indicator),
         x="Date", y="Count")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outputs, "/overall_figures_in_wep2014_2019.pdf"))

for(i in seq(length(list_of_plots_all14))){
  print(list_of_plots_all14[[i]])
}
dev.off()

#------------------------------------------------------------------------
#generate summaries
mala2010<-mala[year>='2010']
#pdf("Overal_summaries.pdf")
do.call(rbind , by(mala2010$malaria_testing, mala2010$year, summary))
do.call(rbind , by(mala2010$malaria_treating, mala2010$year, summary))
do.call(rbind , by(mala2010$Suspected_malaria_fever,mala2010$year, summary))
#dev.off()
#--------------------------------------------------------------------------------------------
#generate overall average trends
average_malaria_trends<-mala_long[,.(average=round(mean(tot,na.rm=T),0)),by=c("variable","year")]
#consider 2013 and above
average_malaria_trends<-average_malaria_trends[year>='2013']
average_malaria_trends<-average_malaria_trends[variable!='Date']


list_of_plots_averages = NULL
i = 1

for (d in unique(average_malaria_trends$variable)) {
  
  indicator = as.character(d)  
  
  list_of_plots_averages[[i]] = ggplot(average_malaria_trends[variable==d], aes(x=year, y=average,group=1))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Average annual national count for:", indicator),
         x="Date", y="Average")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outputs, "/overall_national_average_figures_for_all_parameters.pdf"))

for(i in seq(length(list_of_plots_averages))){
  print(list_of_plots_averages[[i]])
}
dev.off()


