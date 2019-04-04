
rm(list=ls()) # clear memory
library(reshape2)
library(ggplot2)
library(foreign)
library(tidyr)
library(stats4)
library(caret)
library(dplyr)
library(tidyverse)
library(data.table)
library(rgeos)
library(raster)
library(rgdal)
library(tibble)
library(RColorBrewer)
library(maptools)

##-disable scientific notation
options(scipen=999)


setwd("D:/GAVI/GlobalFund_Gavi/Unmerged_Data/HIV/11_12_2018")

# read data 
h15 <- read.csv("HIV_Quarterly_2015.csv")
h16 <- read.csv("HIV_Quarterly_2016.csv")
h17 <- read.csv("HIV_Quarterly_2017.csv")
h18 <- read.csv("HIV_Quarterly_2018.csv")

a56<-merge(h15,h16,by="organisationunitname")
a567<-merge(a56,h17,by="organisationunitname")
a5678<-merge(a567,h18,by="organisationunitname")

hiv<-data.table(a5678)
########reshape long

hiv_data<-melt(hiv,by.vars="organisationunitname")

# identify outliers in the original data set in opd confirmed cases 
hiv_data[ ,unique(variable)]

###need to split variable for merging rasta files
hiv_data$district<- sapply(strsplit(as.character(hiv_data$organisationunitname),' '), "[", 1)

#----------------------------------------------------------------------------------------------------
# add the year
hiv_data[grep(pattern='2015', variable), year:='2015' ]
hiv_data[grep(pattern='2016', variable), year:='2016' ]
hiv_data[grep(pattern='2017', variable), year:='2017' ]
hiv_data[grep(pattern='2018', variable), year:='2018' ]

# month variable
hiv_data[ , variable:=tolower(variable)]

hiv_data[grep(pattern='jul.to.sep.2015', variable), qtr:=1 ]
hiv_data[grep(pattern='oct.to.dec.2015', variable), qtr:=2 ]
hiv_data[grep(pattern='jan.to.mar.2016', variable), qtr:=3 ]
hiv_data[grep(pattern='apr.to.jun.2016', variable), qtr:=4 ]
hiv_data[grep(pattern='jul.to.sep.2016', variable), qtr:=5]
hiv_data[grep(pattern='oct.to.dec.2016', variable), qtr:=6 ]
hiv_data[grep(pattern='jan.to.mar.2017', variable), qtr:=7 ]
hiv_data[grep(pattern='apr.to.jun.2017', variable), qtr:=8 ]
hiv_data[grep(pattern='jul.to.sep.2017', variable), qtr:=9 ]
hiv_data[grep(pattern='oct.to.dec.2017', variable), qtr:=10 ]
hiv_data[grep(pattern='jan.to.mar.2018', variable), qtr:=11 ]
hiv_data[grep(pattern='apr.to.jun.2018', variable), qtr:=12 ]
hiv_data[grep(pattern='jul.to.sep.2018', variable), qtr:=13 ]


hiv_data[ ,range(qtr)]

##remove the quarter year component from
## the indicator variable
hiv_data$indicator <- hiv_data$variable
hiv_data$indicator<-  gsub("_jul.to.sep.2015", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_oct.to.dec.2015", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_jan.to.mar.2016", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_apr.to.jun.2016", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_jul.to.sep.2016", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_oct.to.dec.2016", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_jan.to.mar.2017", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_apr.to.jun.2017", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_jul.to.sep.2017", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_oct.to.dec.2017", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_jan.to.mar.2018", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_apr.to.jun.2018", "", hiv_data$indicator)
hiv_data$indicator<-  gsub("_jul.to.sep.2018", "", hiv_data$indicator)

# view unique indicators
hiv_data[ ,unique(indicator)]

##generate quarterly totals per inidicator in each time point
tothiv_data<-hiv_data[ ,.(tot=sum(value, na.rm=T)),
               by=c("indicator","qtr")]
### collapse data set to generate figures for 2018 narrative
www<-hiv_data[ ,.(tot=sum(value, na.rm=T)),
               by=c("indicator","year")]



tothiv_data$indicator<-factor(tothiv_data$indicator,levels=c("h2.no.tested.for.hiv",
                                                     "h6individuals.tested.hiv.positive",
                                                     "art.new.patients.started.art.in.qter"),
                      labels = c("HIV tests performed",
                                 "Positive HIV tests",
                                 "Number of people newly enrolled on ART"))



##------------------------------------------------------compute proportions

##pick Tested HIV positive
tpos<-tothiv_data[indicator=='Positive HIV tests' ,.(qtr,totpos=tot)]
tnew<-tothiv_data[indicator=='Number of people newly enrolled on ART',.(qtr,totnew=tot)]

allmerged<-merge(tpos,tnew,by='qtr')


allmerged[ ,phivpos:=100*(totnew/totpos), by="qtr"]

#-------------------------------------------------------------------------------------------------------------------
#--get info to map
#map
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
  ##district effective July 2018
  ##"Bugweri"     "Kapelebyong" "Kassanda"    "Kikuube"     "Kwania"
  x[district_name=="Bugweri", district_name:="Iganga"]
  x[district_name=="Kapelebyong", district_name:="Amuria"]
  x[district_name=="Kassanda", district_name:="Mubende"]
  x[district_name=="Kikuube", district_name:="Hoima"]
  x[district_name=="Kwania", district_name:="Apac"]
}
mmap<-hiv_data[,.(district_name=district,indicator,year,value)]
mmap<-merge_new_dists(mmap)

mmap<-mmap[ ,.(tot=sum(value, na.rm=T)),
            by=c("district_name","indicator","year")]
mmap1<-mmap[indicator=='art.new.patients.started.art.in.qter',.(district_name,year,tnew=tot)]
mmap2<-mmap[indicator=='h6individuals.tested.hiv.positive',.(district_name,year,tpos=tot)]

me_map<-merge(mmap1,mmap2,by=c("district_name","year"))
me_map[ ,phivpos:=100*(tnew/tpos),by=c("district_name","year")]

#pick sharefiles for mapping
shapeData <- shapefile("D:/GAVI/uga_shape/uga_dist112_map.shp")

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)] 

as_tibble(shapeData@data) 

# check for unmatched districts
unique(me_map$district_name)[!(unique(me_map$district_name)) %in% unique(shapeData@data$dist112_na)]

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)

coordinates_year <- rbind(coordinates,coordinates,coordinates,coordinates)

# this uses the repeat function: basically it repeats year over and over
coordinates_year[, year:=rep(2015:2018, each=nrow(coordinates))]
coordinates_year$year<-factor(coordinates_year$year,levels=c("2018","2017","2016","2015"),
                              labels = c("2018","2017","2016","2015"))

#- merge data to shape files
totdist<- merge(me_map, shape_names, by='district_name', all=T)

ratios <- brewer.pal(8, 'Spectral')

ratios1 <- brewer.pal(8, 'Greens')
ratios2 <- brewer.pal(8, 'Blues')

map <- merge(coordinates_year, totdist, by=c('id', 'year'), all=T, allow.cartesian=TRUE)



pdf("D:/GAVI/GlobalFund_Gavi/Results_pdf/HIV_cascade_11_12_2018.pdf", width=9, height=6)

#
ggplot(tothiv_data,
       aes(x=qtr, y=tot,color=indicator)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator,scales="free")+
  labs(title='HIV tests performed,Positive HIV tests and Number of people newly enrolled on ART ', subtitle='July 2015 - September 2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))+ 
  theme(legend.position="none")
ggplot(tothiv_data[indicator=='HIV tests performed'],
       aes(x=qtr, y=tot)) +
  geom_point() +
  geom_line() +
  labs(title='HIV tests performed', subtitle='July 2015 - September 2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))
ggplot(tothiv_data[indicator=='Positive HIV tests'],
       aes(x=qtr, y=tot)) +
  geom_point() +
  geom_line() +
  labs(title='Positive HIV tests', subtitle='July 2015 - September 2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))


ggplot(tothiv_data[indicator=='Number of people newly enrolled on ART'],
       aes(x=qtr, y=tot)) +
  geom_point() +
  geom_line() +
  labs(title='Number of people newly enrolled on ART', subtitle='July 2015 - September 2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))

###merged all the 3
ggplot(tothiv_data,
       aes(x=qtr, y=tot,color=indicator)) +
  geom_point() +
  geom_line() +
  labs(title='HIV tests performed,Positive HIV tests and Number of people newly enrolled on ART ', subtitle='July 2015 - September 2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))



#combine 
###merged all the only two 'Number of people newly enrolled on ART' and 'Positive HIV tests'
ggplot(tothiv_data[indicator=='Number of people newly enrolled on ART' | indicator=='Positive HIV tests'],
       aes(x=qtr, y=tot,color=indicator)) +
  geom_point() +
  geom_line() +
  labs(title='Positive HIV tests and Number of people newly enrolled on ART ', subtitle='July 2015 - September 2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))
#combine 


ggplot(allmerged,
       aes(x=qtr, y=phivpos)) +
  geom_point() +
  geom_line() +
  labs(title='Proportion of persons who tested HIV+ and were enrolled on ART', subtitle='July 2015 - September 2018',
       x='Date', y='percent', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c("Jul-Sep'15","Oct-Dec'15","Jan-Mar'16","Apr-Jun'16","Jul-Sep'16","Oct-Dec'16",
                              "Jan-Mar'17","Apr-Jun'17","Jul-Sep'17","Oct-Dec'17","Jan-Mar'18","Apr-Jun'18","Jul-Sep'18"))

##map
ggplot(map, aes(x=long, y=lat, group=group, fill=phivpos)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  theme_void() +
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratios) +
  labs(title="Proportion of persons who tested HIV+ and were enrolled on ART",
       caption="Source: HMIS",
       fill="Proportion")

dev.off()

