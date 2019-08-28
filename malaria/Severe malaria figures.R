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


inDir="D:/PCE/Malaria/Data/Malaria_raw_data/"
outputs="D:/PCE/Malaria/Data/Malaria_outputs/"
setwd(inDir)
smala<-read.csv("Severe malaria Monthly.csv")
smala<-data.table(smala)

# grab the year from quarterly data 
smala$periodcode<-as.character(smala$periodcode)
smala$organisationunitname<-as.character(smala$organisationunitname)

smala[ ,year:=paste0('201',(unlist(lapply(strsplit(periodcode, ""), "[" ,4))))]

smala[ ,mm:=unlist(lapply(strsplit(periodcode, ""), "[" ,5))]
smala[ ,mm1:=unlist(lapply(strsplit(periodcode, ""), "[" ,6))]

smala[ ,mth:=paste0(mm,mm1)]

smala[mth>='01' & mth<='03',qtr:='01']
smala[mth>='04' & mth<='06',qtr:='04']
smala[mth>='07' & mth<='09',qtr:='07']
smala[mth>='10' & mth<='12',qtr:='10']


smala[ , Date:=paste0(year, '-', qtr, '-01')]
smala[ , Date:=ymd(Date)]

smala[ ,district_name:=unlist(lapply(strsplit(organisationunitname, " "), "[" ,1))]

tot_smala<-smala[,.(Severe_malaria=sum(X108.5b.severe.malaria,na.rm = T)),by=c("district_name","Date")]

#National overall trend
national_severe_mala<-tot_smala[,.(smal=sum(Severe_malaria)),by="Date"]

pdf(paste0(outputs,"National_severe_malaria_cases_trend_in_Uganda.pdf"))
ggplot(national_severe_mala[Date>'2015-04-01'], aes(x=Date, y=smal)) +
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title="National count of severe malaria cases: July 2015 to June 2019 ",
       Subtitle="2015-2019",
       x="Date", y="Count",
         caption="Source: HMIS",
       fill="count")+
  theme(text=element_text(size=18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", size=0.5))
dev.off()
#distribution of severe malaria over time
#annul
tot_smala<-tot_smala[,year:=year(Date)]

anual_dist_smala<-tot_smala[,.(tot=sum(Severe_malaria,na.rm=T)),by=c("district_name","year")]

#consider only 2016 to 2019
anual_dist_smala<-anual_dist_smala[year>='2016' & year<='2019']

#---------------------------------------------------------------------------------------------
shapeData <- shapefile("D:/shapefiles/uga_shape/uga_dist112_map.shp")

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)] 


# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)

coordinates_yr <- rbind(coordinates,coordinates,coordinates,coordinates)

# this uses the repeat function: basically it repeats year over and over
coordinates_yr[, year:=rep(2016:2019, each=nrow(coordinates))]
coordinates_yr<-merge(coordinates_yr,shape_names,by="id")
#coordinates_yr$year<-as.character(coordinates_yr$year)

#merge in the dataset
map_anual_dist_smala<-merge(anual_dist_smala,coordinates_yr,by=c("district_name","year"))
str(anual_dist_smala)
#-----------------------------------------------------------------
#
ratios1 <- brewer.pal(8, 'Blues')
ratios2 <- brewer.pal(8, 'Greens')
ratios3 <- brewer.pal(8, 'Oranges')
ratios4 <- brewer.pal(8, 'Reds')


pdf(paste0(outputs,"Maps_of_severe_malaria_cases_in_Uganda.pdf"))
ggplot(map_anual_dist_smala, aes(x=long, y=lat, group=group, fill=tot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Distribution of severe malaria cases in Uganda",
       caption="Source: HMIS",Subtitle="2017-2019",
       fill="Count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

dev.off()
#------------------------------------------------------------------------------------------------
dtot_smala<-tot_smala[year>=2015]
#district level trends over
list_of_plots = NULL
i = 1

for (d in unique(dtot_smala$district_name)) {
  
  district = as.character(d)  
  
  list_of_plots[[i]] = ggplot(dtot_smala[district_name==d], aes(x=Date, y=Severe_malaria,group=1))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Count of severe malaria cases for: ", district),
         x="Date", y="Count")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outputs, "/Sever_malaria_cases_for_each_district.pdf"))

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}
dev.off()

