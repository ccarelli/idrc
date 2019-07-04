<<<<<<< HEAD
##TB
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
  ##district effective July 2018
  ##"Bugweri"     "Kapelebyong" "Kassanda"    "Kikuube"     "Kwania" Nakapiripirit
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
#--pick mdr related data from HMIS
mdr_h<-read.csv("Treatment success rates_data.csv")
mdr_hmis<-melt(mdr_h,by.vars="organisationunitname")

mdr_hmis<-data.table(mdr_hmis)

mdr_hmis$district_name<- sapply(strsplit(as.character(mdr_hmis$organisationunitname),' '), "[", 1)
mdr_hmis<-merge_new_dists(mdr_hmis)




#----------------------------------------------------------------------------------------------------
# add the year
mdr_hmis[grep(pattern='2015', variable), year:='2015' ]
mdr_hmis[grep(pattern='2016', variable), year:='2016' ]
mdr_hmis[grep(pattern='2017', variable), year:='2017' ]
mdr_hmis[grep(pattern='2018', variable), year:='2018' ]
mdr_hmis[grep(pattern='2019', variable), year:='2019' ]

# month variable
mdr_hmis[ , variable:=tolower(variable)]


mdr_hmis$indicator <- mdr_hmis$variable
mdr_hmis$indicator<-  gsub("_jan.to.mar.2015", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2015", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2015", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2015", "", mdr_hmis$indicator)

mdr_hmis$indicator<-  gsub("_jan.to.mar.2016", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2016", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2016", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2016", "", mdr_hmis$indicator)

mdr_hmis$indicator<-  gsub("_jan.to.mar.2017", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2017", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2017", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2017", "", mdr_hmis$indicator)

mdr_hmis$indicator<-  gsub("_jan.to.mar.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jan.to.mar.2019", "", mdr_hmis$indicator)

##qtr
#mdr_hmis[grep(pattern='jul.to.sep.2015', variable), qtr:=1 ]
#mdr_hmis[grep(pattern='oct.to.dec.2015', variable), qtr:=2 ]
#mdr_hmis[grep(pattern='jan.to.mar.2016', variable), qtr:=3 ]
#mdr_hmis[grep(pattern='apr.to.jun.2016', variable), qtr:=4 ]

mdr_hmis[grep(pattern='jul.to.sep.2016', variable), qtr:=1]
mdr_hmis[grep(pattern='oct.to.dec.2016', variable), qtr:=2 ]
mdr_hmis[grep(pattern='jan.to.mar.2017', variable), qtr:=3 ]
mdr_hmis[grep(pattern='apr.to.jun.2017', variable), qtr:=4 ]
mdr_hmis[grep(pattern='jul.to.sep.2017', variable), qtr:=5 ]
mdr_hmis[grep(pattern='oct.to.dec.2017', variable), qtr:=6 ]
mdr_hmis[grep(pattern='jan.to.mar.2018', variable), qtr:=7 ]
mdr_hmis[grep(pattern='apr.to.jun.2018', variable), qtr:=8 ]
mdr_hmis[grep(pattern='jul.to.sep.2018', variable), qtr:=9 ]
mdr_hmis[grep(pattern='oct.to.dec.2018', variable), qtr:=10 ]
mdr_hmis[grep(pattern='jan.to.mar.2019', variable), qtr:=11 ]

mdr_hmis[is.na(mdr_hmis)]<-0

mdr_hmis[ ,unique(indicator)]

labconf<-mdr_hmis[indicator=='lab.confirmed.mdr.tb',.(district_name,year,qtr,labconf=value)]
conf_treat<-mdr_hmis[indicator=='confirmed.rrmdr.tb.patients.started.treatment',.(district_name,year,qtr,conf_treat=value)]

lab_treat<-merge(labconf,conf_treat,by=c("district_name","year","qtr"))
lab_treat<-lab_treat[qtr!=0,]
lab_treat<-merge_new_dists(lab_treat)



mdr_hmis_year<-mdr_hmis[ ,.(stot=sum(value, na.rm=T),avetot=mean(value, na.rm=T)),
      by=c("district_name","indicator","year")]

mdr_hmis_select<-mdr_hmis_year[district_name=='Kole' | district_name=='Nakapiripirit' | district_name=='Amudat'| district_name=='Apac'| district_name=='Sheema'| district_name=='Mitooma']
  
  
mdr_hmis_qtr<-mdr_hmis[ ,.(stot=sum(value, na.rm=T),avetot=mean(value, na.rm=T)),
                         by=c("district_name","indicator","qtr")]

omdr_hmis_qtr<-mdr_hmis[ ,.(stot=sum(value, na.rm=T),avetot=mean(value, na.rm=T)),
                        by=c("indicator","qtr")]

##graphs

ggplot(omdr_hmis_qtr[qtr>'2'],aes(x=qtr, y=stot)) +
  geom_point() +
  geom_line() +
  labs(title='Proportion ', subtitle='July 2015- March 2019',
       x='Date', y='Proportion', caption='Source: HMIS', color='Percent') +
  theme_bw()+
  #scale_y_continuous(limits = c(0, 50))+
 facet_wrap(~indicator,scales = "free")+
  scale_x_continuous(breaks=c(1,4,8,12,15),labels=c("Jul-Sep'15","Apr-Jun'16","Apr-Jun'17","Apr-Jun'18","Jan-Mar'19"))

ggplot(omdr_hmis_qtr[indicator=='lab.conf.rrmdr.tb' | indicator=='lab.conf.rrmdr.tb'],aes(x=qtr, y=stot)) +
  geom_point() +
  geom_line() +
  labs(title='Proportion ', subtitle='July 2015- March 2019',
       x='Date', y='Proportion', caption='Source: HMIS', color='Percent') +
  theme_bw()+
  #scale_y_continuous(limits = c(0, 50))+
  facet_wrap(~indicator,scales = "free")+
  scale_x_continuous(breaks=c(1,4,8,12,15),labels=c("Jul-Sep'15","Apr-Jun'16","Apr-Jun'17","Apr-Jun'18","Jan-Mar'19"))



#-------------------------read in tb data from HMIS
# read data 
TB <- read.csv("TB indicators - April 2019.csv")
TB<-data.table(TB)
mart<-melt(TB,by.vars="organisationunitname")

# identify outliers in the original data set in opd confirmed cases 
mart[ ,unique(variable)]



###need to split variable for merging rasta filesn
mart$district_name<- sapply(strsplit(as.character(mart$organisationunitname),' '), "[", 1)

mart[ ,unique(district_name)]
mart<-merge_new_dists(mart)

#----------------------------------------------------------------------------------------------------
# add the year
mart[grep(pattern='2016', variable), year:='2016' ]
mart[grep(pattern='2017', variable), year:='2017' ]
mart[grep(pattern='2018', variable), year:='2018' ]
mart[grep(pattern='2019', variable), year:='2019' ]

# month variable
mart[ , variable:=tolower(variable)]


mart$indicator <- mart$variable
mart$indicator<-  gsub("_jan.to.mar.2016", "", mart$indicator)
mart$indicator<-  gsub("_apr.to.jun.2016", "", mart$indicator)
mart$indicator<-  gsub("_jul.to.sep.2016", "", mart$indicator)
mart$indicator<-  gsub("_oct.to.dec.2016", "", mart$indicator)

mart1$indicator<-  gsub("_jan.to.mar.2018", "", mart1$indicator)
mart1$indicator<-  gsub("_apr.to.jun.2018", "", mart1$indicator)
mart$indicator<-  gsub("_jul.to.sep.2017", "", mart$indicator)
mart$indicator<-  gsub("_oct.to.dec.2017", "", mart$indicator)

mart$indicator<-  gsub("_jan.to.mar.2018", "", mart$indicator)
mart$indicator<-  gsub("_apr.to.jun.2018", "", mart$indicator)
mart$indicator<-  gsub("_jul.to.sep.2018", "", mart$indicator)
mart$indicator<-  gsub("_oct.to.dec.2018", "", mart$indicator)
mart$indicator<-  gsub("_jan.to.mar.2019", "", mart$indicator)

totmart<-mart[ ,.(totb=sum(value, na.rm=T),aprop=mean(value, na.rm=T)),
               by=c("indicator","year")]
tdisttb<-mart[ ,.(totb=sum(value, na.rm=T),aprop=mean(value, na.rm=T)),
                by=c("district_name","indicator","year")]
tdisttb<-data.table(tdisttb)

notifytb<-tdisttb[indicator=='total.notified.tb',.(district_name,year,notify_tb=totb)]
comptb<-tdisttb[indicator=='completed..all.tb.cases.',.(district_name,year,comp_tb=aprop)]
curetb<-tdisttb[indicator=='cured..all.tb.cases.',.(district_name,year,cure_tb=aprop)]
lostb<-tdisttb[indicator=='lost..all.tb.cases.',.(district_name,year,lost_tb=aprop)]


allTB1<-merge(notifytb,comptb,by=c("district_name","year"))
allTB2<-merge(allTB1,curetb,by=c("district_name","year"))
allTB<-merge(allTB2,lostb,by=c("district_name","year"))
allTB$success<-(allTB$comp_tb + allTB$cure_tb)
allTB$success[allTB$success>100]<-100
#----------------------------------------------------------------------
# pick shapefiles

shapeData <- shapefile("uga_dist112_map.shp")

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)] 

as_tibble(shapeData@data) 


# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)


coordinates_qtr <- rbind(coordinates,coordinates,coordinates,coordinates)
# this uses the repeat function: basically it repeats year over and over
coordinates_qtr[, year:=rep(2016:2019, each=nrow(coordinates))]
#coordinates_qtr$qtr<-factor(coordinates_qtr$qtr,levels=c(1,2,3,4,5),
          #                    labels = c("Jan-Mar'18","Apr-Jun'18","Jul-Sep'18","Oct-Dec'18","Jan-Mar'19"))
#---------------------------------------------------------------
coord_region<-merge(coordinates,shape_names,by="id")
coord_region_qtr <- rbind(coord_region,coord_region,coord_region,coord_region)
coord_region_qtr [, year:=rep(2016:2019, each=nrow(coord_region))]
coord_region_qtr$year<-factor(coord_region_qtr$year,levels=c(2016,2017,2018,2019),
                             labels = c("2016","2017","2018","2019"))
coordinates_qtr$year<-factor(coordinates_qtr$year,levels=c(2016,2017,2018,2019),
                 labels = c("2016","2017","2018","2019"))
#read in geneXpert data
#read data from .csv files -------------------------------------------------------------------------------------------------
qtr1 <- read.csv("QTR 1 2016.csv")
qtr2 <- read.csv("QTR 2 2016.csv")
qtr3 <- read.csv("QTR 3 2016.csv")
qtr4 <- read.csv("QTR 4 2016.csv")
qtr5 <- read.csv("QTR 1 2017.csv")
qtr6 <- read.csv("QTR 2 2017.csv")
qtr7 <- read.csv("QTR 3 2017.csv")
qtr8 <- read.csv("QTR 4 2017.csv")
qtr9 <- read.csv("QTR 1 2018.csv")
qtr10 <- read.csv("QTR 2 2018.csv")
qtr11 <- read.csv("QTR 3 2018.csv")
qtr12 <- read.csv("QTR 4 2018.csv")
qtr13 <- read.csv("QTR 1 2019.csv")

#introduce time or qtr
qtr1$year<-"2016"
qtr2$year<-"2016"
qtr3$year<-"2016"
qtr4$year<-"2016"
qtr5$year<-"2017"
qtr6$year<-"2017"
qtr7$year<-"2017"
qtr8$year<-"2017"
qtr9$year<-"2018"
qtr10$year<-"2018"
qtr11$year<-"2018"
qtr12$year<-"2018"
qtr13$year<-"2019"
#qtrs
qtr1$qtr<-1
qtr2$qtr<-2
qtr3$qtr<-3
qtr4$qtr<-4
qtr5$qtr<-5
qtr6$qtr<-6
qtr7$qtr<-7
qtr8$qtr<-8
qtr9$qtr<-9
qtr10$qtr<-10
qtr11$qtr<-11
qtr12$qtr<-12
qtr13$qtr<-13



##combine similar
gene1<-rbind(qtr1,qtr2,qtr3,qtr4,qtr5,qtr6,qtr7,qtr8,qtr9,qtr10,qtr11,qtr12,qtr13)
gene1<-data.table(gene1)
###need to split variable for merging rasta files
gene1$district_name<- sapply(strsplit(as.character(gene1$District),' '), "[", 1)
setnames(gene1,"No.","id")

gene1<-gene1[id!=0 ]
gene1<-gene1[id!=""]
gene1<-gene1[id!='ARUA TOTAL']

#Butenga HC IV
gene1$District[gene1$id==40 & gene1$District==""]<-"Dokolo"
gene1$Region[gene1$id==40 & gene1$Region==""]<-"Lira"
gene1$District[gene1$id==97 & gene1$District==""]<-"Kasese"
gene1$Region[gene1$id==97 & gene1$Region==""]<-"Fortportal"
gene1$District[gene1$id==55 & gene1$District==""]<-"Maracha"
gene1$Region[gene1$id==55 & gene1$Region==""]<-"Arua"
##check!!!!!!! to resolve
gene1$District[gene1$District==""]<-"Missing"
gene1$Region[gene1$Region==""]<-"Missing"



#-------------------------convert to numeric
gene1$MTB..Rif.<-as.numeric(as.character(gene1$MTB..Rif.))
gene1$Rif.Resist<-as.numeric(as.character(gene1$Rif.Resist))
gene1[grep(pattern='Lira', Region), Region:="Lira" ]

#----------------------introduce counter for site
gene1$site[gene1$Genexpert.Site!='NA']<-1

#align district names
gene1<-merge_new_dists(gene1)
#add TB from HMIS

gene2<-merge(allTB,gene1, by=c("district_name","year"),all.x=T)

gene<-merge(gene2,lab_treat, by=c("district_name","year","qtr"),all.x=T)


gene$diff<-((gene$notify_tb) - (gene$MTB..Rif. + gene$Rif.Resist))

#merge in data from hmis mdr
#-------------------------------------------------------------------------------------------------------------



test<-merge(shape_names,gene,by="district_name")


#collapse data into district_name Region qtr
ALLdistrict<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                  Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                  postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                  acomp_tb=mean(comp_tb, na.rm=T),
                  acure_tb=mean(cure_tb, na.rm=T),
                  alost_tb=mean(lost_tb, na.rm=T),
                  asuccess=mean(success, na.rm=T),
                  smachine=sum(machines, na.rm=T),
                  diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                  totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                  sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))),
                  sconf_treat=sum(conf_treat, na.rm=T)),
               by=c("district_name","Region","year")]


#overall trends computations
otrends<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                  Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                  postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                  acomp_tb=mean(comp_tb, na.rm=T),
                  acure_tb=mean(cure_tb, na.rm=T),
                  alost_tb=mean(lost_tb, na.rm=T),
                  asuccess=mean(success, na.rm=T),
                  smachine=sum(machines, na.rm=T),
                  diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                  totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                  sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))),
                  sconf_treat=sum(conf_treat, na.rm=T) ),
               by=c("year")]

otrends_long<-melt(otrends,id.vars = c("year"))

otrends_long[ ,unique(variable)]

otrends_long$Indicator<-factor(otrends_long$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess,
                                                              sgene_lab,sconf_treat"),
                           labels = c("Samples tested","MTB positive","Resistant","Indeterminate",
                                      "Errors","Percent TB Positive","Percent resistant",
                                      "Percent Indeterminate","Percent errors","GeneXpert sites",
                                      "Notified TB","Completed TB","Cure TB","Lost TB",
                                      "TB positive by geneXpert","TB Success rate",
                                      "MDR-DR confirmed TB","Started treatmnent for MDR-DR"))

#overal quarterly trends
otrends_qtr<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                  Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                  postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                  acomp_tb=mean(comp_tb, na.rm=T),
                  acure_tb=mean(cure_tb, na.rm=T),
                  alost_tb=mean(lost_tb, na.rm=T),
                  asuccess=mean(success, na.rm=T),
                  smachine=sum(machines, na.rm=T),
                  diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                  totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                  sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))),
                  sconf_treat=sum(conf_treat, na.rm=T) ),
               by=c("qtr")]
otrends_qtr<-otrends_qtr[qtr!='NA']
otrends_qtr_long<-melt(otrends_qtr,id.vars = c("qtr"))

otrends_qtr_long$Indicator<-factor(otrends_qtr_long$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess",
                                                              "sgene_lab","sconf_treat"),
                               labels = c("Samples tested","MTB positive","Resistant","Indeterminate",
                                          "Errors","Percent TB Positive","Percent resistant",
                                          "Percent Indeterminate","Percent errors","GeneXpert sites",
                                          "Notified TB","Completed TB","Cure TB","Lost TB",
                                          "TB positive by geneXpert","TB Success rate",
                                          "MDR-DR confirmed TB","Started treatmnent for MDR-DR"))

Region<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                 Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                 postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                 acomp_tb=mean(comp_tb, na.rm=T),
                 acure_tb=mean(cure_tb, na.rm=T),
                 alost_tb=mean(lost_tb, na.rm=T),
                 asuccess=mean(success, na.rm=T),
                 ratio_samplesp_site=((sum(Samples.tested, na.rm=T))/(sum(site, na.rm=T))),
                 smachine=sum(machines, na.rm=T),
                 diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                 totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                 sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))) ,
                 sconf_treat=sum(conf_treat, na.rm=T)),
                     by=c("Region","year")]

Region<-data.table(Region)
Region<-Region[stot!='0',]

Region$snotify_tb[Region$snotify_tb > 200000]<-200000


Region_long<-melt(Region,id.vars = c("Region","year"))
Region_long$Indicator<-factor(Region_long$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess"),
                               labels = c("Samples tested","MTB positive Rif negative","Resistant","Indeterminate",
                                          "Errors","Percent TB Positive","Percent resistant",
                                          "Percent Indeterminate","Percent errors","GeneXpert sites",
                                          "Notified TB","Completed TB","Cure TB","Lost TB",
                                          "TB positive by geneXpert","TB Success rate"))
##maps
dist<-gene
dist<-data.table(dist)

dist<-merge_new_dists(dist) 
district<-dist[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                   postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                   acomp_tb=mean(comp_tb, na.rm=T),
                   acure_tb=mean(cure_tb, na.rm=T),
                   alost_tb=mean(lost_tb, na.rm=T),
                   asuccess=mean(success, na.rm=T),
                   ratio_samplesp_site=((sum(Samples.tested, na.rm=T))/(sum(site, na.rm=T))),
                   smachine=sum(machines, na.rm=T),
                   diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                   totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                   sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))) ,
                   sconf_treat=sum(conf_treat, na.rm=T)),
              by=c("district_name","year")]
#----------------------------------------------------------------------------------------
district[, list(district_name,max.diff=max(diff)), by=year]

#- merge data to shape files
totdist<- merge(district, shape_names, by='district_name', all=T)

ratios <- brewer.pal(8, 'Accent')
ratios1 <- brewer.pal(8, 'Blues')
ratios2 <- brewer.pal(8, 'Greens')
ratios3 <- brewer.pal(8, 'Oranges')
ratios4 <- brewer.pal(8, 'Reds')


map <- merge(coordinates_qtr, totdist, by=c('id', 'year'), all=T, allow.cartesian=TRUE)
#map <- map[year!='2019']


#--------------Number of partners in the region

part<-unique(gene[,.(Region,I.Partner,year)])
part<-data.table(part)
part[I.Partner!='#REF!'& I.Partner!="",npart:=1]

tpart<-part[ ,.(snpart=sum(npart, na.rm=T)),
              by=c("Region","year")]
#combine with samples tested and others
part_region<-merge(Region,tpart,by=c("Region","year"))
part_region<-data.table(part_region)
part_region<-part_region[Region!='NA',.(Region,year,stot,gene_sites,snpart)]
part_reg<-melt(part_region,id.vars = c("Region","year"))

part_reg$Indicator<-factor(part_reg$variable,levels=c("stot","gene_sites","snpart"),
                 labels = c("Samples tested","GeneXpert sites","Implementing Partners"))

#Generating region maps
ALLdistrict_map<-merge_new_dists(gene)
ALLdistrict_link<-ALLdistrict_map[ ,.(stot1=1),
                                   by=c("district_name","Region","year")]

#read link to regions
Link_district_region<-read_rds("Link_district_region.rds")
#add qtrs
Link_district_region_qtr <- rbind(Link_district_region,Link_district_region,Link_district_region,Link_district_region)

Link_district_region_qtr<-data.table(Link_district_region_qtr)
Link_district_region_qtr[, year:=rep(1:4, each=nrow(Link_district_region))]
Link_district_region_qtr$year<-factor(Link_district_region_qtr$year,levels=c(1,2,3,4),
                           labels = c("2016","2017","2018","2019"))


Linked_region<-merge(coord_region_qtr,Link_district_region_qtr,by=c("district_name","year"))

#merge in shapefiles 
#shapefiles is coord_region_qtr
#Linked_region<-merge(coord_region_qtr,Linked_region,by=c("district_name","year"))
coord_region_qtr_map <- merge( Region,Linked_region, by=c("Region","year"), all=T, allow.cartesian=TRUE)

coord_region_qtr_map$diffmdr<-(coord_region_qtr_map$sconf_treat -coord_region_qtr_map$sgene_lab)
coord_region_qtr_map$diffmdr1<-coord_region_qtr_map$diffmdr
coord_region_qtr_map$diffmdr1[coord_region_qtr_map$diffmdr>3000]<-3000
#------------------------------------------------------------------need to manipulate regional borders

#--------------district level maps------------------------------------------
coord_region_qtr_district<-merge(coord_region_qtr,district,by=c("district_name","year"), all=T, allow.cartesian=TRUE)

coord_region_qtr_district$diffmdr<-(coord_region_qtr_district$sconf_treat -coord_region_qtr_district$sgene_lab)
coord_region_qtr_district$diffmdr1<-coord_region_qtr_district$diffmdr
coord_region_qtr_district$diffmdr1[coord_region_qtr_district$diffmdr>3000]<-3000



#----------------------------------------------------------------------------------------------------data manipulation ends here
##checking regions
#coord_region_qtr_map
pdf("tb_geneXpert_figures_map_4_regional_yearly_per_page.pdf", width=9, height=9)

#coord_region_qtr_map
ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=stot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Samples tested per GeneXpert site",
       caption="Source: GenXpert NTLP",Subtitle="2016-2019",
       fill="count")



ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=postb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="TB positive",
       caption="Source: GenXpert NTLP",Subtitle="2016-2019",
       fill="Percent") 


ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=snotify_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Notified tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="count")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=asuccess)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios7) +
  facet_wrap(~year)+
  labs(title="TB treatment success rate",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=presist)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title=" Percentage resistant",
       caption="Source: GeneXpert",Subtitle="2016-2019",
       fill="Percent")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=acomp_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Completed tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent") 

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios6) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent") 
ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios7) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=diff)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Excess of Notified Tb less TB positive by GeneXpert",
       caption="Source: GeneXpert & HMIS",Subtitle="2016-2019",
       fill="count")
dev.off()

#---------------------------------------------
pdf("tb_geneXpert_figures_map_3_regional_yearly_per_page.pdf", width=9, height=6)

#coord_region_qtr_map
ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=stot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Samples tested per GeneXpert site",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=postb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Percentage of samples tested that are positive",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=(snotify_tb/1000))) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Notified tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="count(000)")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=asuccess)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="TB treatment success rate",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=presist)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title=" Percentage resistant",
       caption="Source: GeneXpert",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acomp_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Completed tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diff)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Excess of Notified Tb less TB positive by GeneXpert",
       caption="Source: GeneXpert & HMIS",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

dev.off()

#overal quarterly trends
#read mdr cases
mdr<-read.csv("mdr_cases.csv")

ggplot(mdr,aes(x=qtr, y=conf_mdr)) +
  geom_point(size=1,color="blue") +
  geom_line(size=1,color="blue") +
  labs(title='Count', subtitle='July 2015 to Mar 2019',
       x='Date', y='Count', caption='Source: HMIS') +
  theme_bw()+
   scale_y_continuous(limits = c(0,100))+
  scale_x_continuous(breaks=c(1,4,7,10,13),
                     labels=c("Jul-Sep'15","Apr-Jun'16","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(mdr,aes(x=qtr, y=opd_presumed_mdr)) +
  geom_point(size=1,color="red") +
  geom_line(size=1,color="red") +
  labs(title='Count', subtitle='July 2015 to Mar 2019',
       x='Date', y='Count', caption='Source: HMIS') +
  theme_bw()+
  scale_y_continuous(limits = c(0,3000))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")


ggplot(otrends_qtr_long[variable=='postb'|variable=='presist'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Proportion', subtitle='2016 to Mar 2019',
       x='Date', y='Percent', caption='Source: GeneXpert NTLP; Notified TB-HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  scale_y_continuous(limits = c(0,20))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(otrends_qtr_long[variable=='presist'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Proportion', subtitle='2016 to Mar 2019',
       x='Date', y='Percent', caption='Source: GeneXpert NTLP; Notified TB-HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")


ggplot(otrends_qtr_long[variable=='sconf_treat' | variable=='sgene_lab'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(otrends_qtr_long[variable=='sconf_treat' ],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  scale_y_continuous(limits = c(0,30000))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(otrends_qtr_long[ variable=='sgene_lab'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

#Rif_Resist
ggplot(otrends_qtr_long[ variable=='Rif_Resist'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

#read in mdr cases
md_r<-read.csv("mdr_cases.csv")

md_r<-data.table(md_r)
ggplot(md_r[qtr>"2"],
       aes(x=qtr, y=conf_mdr,color="blue")) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~conf_mdr, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

##years
##figures for deep dive
otrends_long_yr18<-otrends_long[year!=2019,]
otrends_long_yr18$indicator<-factor(otrends_long_yr18$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess",
                                                              "sgene_lab","sconf_treat"),
                               labels = c("Samples tested","MTB positive","Resistant","Indeterminate",
                                          "Errors","Percent TB Positive","Percent resistant",
                                          "Percent Indeterminate","Percent errors","GeneXpert sites",
                                          "Notified TB","Completed TB","Cure TB","Lost TB",
                                          "TB positive by geneXpert","TB Success rate",
                                          "MDR-DR confirmed TB","Started treatmnent for MDR-DR"))

pdf("mdr_related_figures.pdf", width=9, height=6)

ggplot(otrends_long_yr18[variable=='sconf_treat' | variable=='sgene_lab' | variable=='totpost'],
       aes(x=year, y=(value)),color=variable) +
  geom_bar(stat='identity',color='blue') +
  labs(title='Count', subtitle='2016 - 2018',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  theme(legend.position="none")

ggplot(otrends_long,
       aes(x=year, y=(value)),color=variable) +
  geom_bar(stat='identity',color='blue') +
  labs(title='Count', subtitle='2016 - 2018',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  theme(legend.position="none")

dev.off()

#--------------district level maps
pdf("tb_geneXpert_figures_map_3_district_yearly_per_page.pdf", width=9, height=6)

#coord_region_qtr_district
ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=stot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Samples tested by GeneXpert from each district",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=postb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="TB positive",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=(snotify_tb/1000))) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Notified tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="count(000)")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=asuccess)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="TB treatment success rate",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=presist)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title=" Percentage resistant",
       caption="Source: GeneXpert",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acomp_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Completed tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diff)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Excess of Notified Tb less TB positive by GeneXpert",
       caption="Source: GeneXpert & HMIS",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

dev.off()
=======
##TB
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
  ##district effective July 2018
  ##"Bugweri"     "Kapelebyong" "Kassanda"    "Kikuube"     "Kwania" Nakapiripirit
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
#--pick mdr related data from HMIS
mdr_h<-read.csv("Treatment success rates_data.csv")
mdr_hmis<-melt(mdr_h,by.vars="organisationunitname")

mdr_hmis<-data.table(mdr_hmis)

mdr_hmis$district_name<- sapply(strsplit(as.character(mdr_hmis$organisationunitname),' '), "[", 1)
mdr_hmis<-merge_new_dists(mdr_hmis)




#----------------------------------------------------------------------------------------------------
# add the year
mdr_hmis[grep(pattern='2015', variable), year:='2015' ]
mdr_hmis[grep(pattern='2016', variable), year:='2016' ]
mdr_hmis[grep(pattern='2017', variable), year:='2017' ]
mdr_hmis[grep(pattern='2018', variable), year:='2018' ]
mdr_hmis[grep(pattern='2019', variable), year:='2019' ]

# month variable
mdr_hmis[ , variable:=tolower(variable)]


mdr_hmis$indicator <- mdr_hmis$variable
mdr_hmis$indicator<-  gsub("_jan.to.mar.2015", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2015", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2015", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2015", "", mdr_hmis$indicator)

mdr_hmis$indicator<-  gsub("_jan.to.mar.2016", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2016", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2016", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2016", "", mdr_hmis$indicator)

mdr_hmis$indicator<-  gsub("_jan.to.mar.2017", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2017", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2017", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2017", "", mdr_hmis$indicator)

mdr_hmis$indicator<-  gsub("_jan.to.mar.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_apr.to.jun.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jul.to.sep.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_oct.to.dec.2018", "", mdr_hmis$indicator)
mdr_hmis$indicator<-  gsub("_jan.to.mar.2019", "", mdr_hmis$indicator)

##qtr
#mdr_hmis[grep(pattern='jul.to.sep.2015', variable), qtr:=1 ]
#mdr_hmis[grep(pattern='oct.to.dec.2015', variable), qtr:=2 ]
#mdr_hmis[grep(pattern='jan.to.mar.2016', variable), qtr:=3 ]
#mdr_hmis[grep(pattern='apr.to.jun.2016', variable), qtr:=4 ]

mdr_hmis[grep(pattern='jul.to.sep.2016', variable), qtr:=1]
mdr_hmis[grep(pattern='oct.to.dec.2016', variable), qtr:=2 ]
mdr_hmis[grep(pattern='jan.to.mar.2017', variable), qtr:=3 ]
mdr_hmis[grep(pattern='apr.to.jun.2017', variable), qtr:=4 ]
mdr_hmis[grep(pattern='jul.to.sep.2017', variable), qtr:=5 ]
mdr_hmis[grep(pattern='oct.to.dec.2017', variable), qtr:=6 ]
mdr_hmis[grep(pattern='jan.to.mar.2018', variable), qtr:=7 ]
mdr_hmis[grep(pattern='apr.to.jun.2018', variable), qtr:=8 ]
mdr_hmis[grep(pattern='jul.to.sep.2018', variable), qtr:=9 ]
mdr_hmis[grep(pattern='oct.to.dec.2018', variable), qtr:=10 ]
mdr_hmis[grep(pattern='jan.to.mar.2019', variable), qtr:=11 ]

mdr_hmis[is.na(mdr_hmis)]<-0

mdr_hmis[ ,unique(indicator)]

labconf<-mdr_hmis[indicator=='lab.confirmed.mdr.tb',.(district_name,year,qtr,labconf=value)]
conf_treat<-mdr_hmis[indicator=='confirmed.rrmdr.tb.patients.started.treatment',.(district_name,year,qtr,conf_treat=value)]

lab_treat<-merge(labconf,conf_treat,by=c("district_name","year","qtr"))
lab_treat<-lab_treat[qtr!=0,]
lab_treat<-merge_new_dists(lab_treat)



mdr_hmis_year<-mdr_hmis[ ,.(stot=sum(value, na.rm=T),avetot=mean(value, na.rm=T)),
      by=c("district_name","indicator","year")]

mdr_hmis_select<-mdr_hmis_year[district_name=='Kole' | district_name=='Nakapiripirit' | district_name=='Amudat'| district_name=='Apac'| district_name=='Sheema'| district_name=='Mitooma']
  
  
mdr_hmis_qtr<-mdr_hmis[ ,.(stot=sum(value, na.rm=T),avetot=mean(value, na.rm=T)),
                         by=c("district_name","indicator","qtr")]

omdr_hmis_qtr<-mdr_hmis[ ,.(stot=sum(value, na.rm=T),avetot=mean(value, na.rm=T)),
                        by=c("indicator","qtr")]

##graphs

ggplot(omdr_hmis_qtr[qtr>'2'],aes(x=qtr, y=stot)) +
  geom_point() +
  geom_line() +
  labs(title='Proportion ', subtitle='July 2015- March 2019',
       x='Date', y='Proportion', caption='Source: HMIS', color='Percent') +
  theme_bw()+
  #scale_y_continuous(limits = c(0, 50))+
 facet_wrap(~indicator,scales = "free")+
  scale_x_continuous(breaks=c(1,4,8,12,15),labels=c("Jul-Sep'15","Apr-Jun'16","Apr-Jun'17","Apr-Jun'18","Jan-Mar'19"))

ggplot(omdr_hmis_qtr[indicator=='lab.conf.rrmdr.tb' | indicator=='lab.conf.rrmdr.tb'],aes(x=qtr, y=stot)) +
  geom_point() +
  geom_line() +
  labs(title='Proportion ', subtitle='July 2015- March 2019',
       x='Date', y='Proportion', caption='Source: HMIS', color='Percent') +
  theme_bw()+
  #scale_y_continuous(limits = c(0, 50))+
  facet_wrap(~indicator,scales = "free")+
  scale_x_continuous(breaks=c(1,4,8,12,15),labels=c("Jul-Sep'15","Apr-Jun'16","Apr-Jun'17","Apr-Jun'18","Jan-Mar'19"))



#-------------------------read in tb data from HMIS
# read data 
TB <- read.csv("TB indicators - April 2019.csv")
TB<-data.table(TB)
mart<-melt(TB,by.vars="organisationunitname")

# identify outliers in the original data set in opd confirmed cases 
mart[ ,unique(variable)]



###need to split variable for merging rasta filesn
mart$district_name<- sapply(strsplit(as.character(mart$organisationunitname),' '), "[", 1)

mart[ ,unique(district_name)]
mart<-merge_new_dists(mart)

#----------------------------------------------------------------------------------------------------
# add the year
mart[grep(pattern='2016', variable), year:='2016' ]
mart[grep(pattern='2017', variable), year:='2017' ]
mart[grep(pattern='2018', variable), year:='2018' ]
mart[grep(pattern='2019', variable), year:='2019' ]

# month variable
mart[ , variable:=tolower(variable)]


mart$indicator <- mart$variable
mart$indicator<-  gsub("_jan.to.mar.2016", "", mart$indicator)
mart$indicator<-  gsub("_apr.to.jun.2016", "", mart$indicator)
mart$indicator<-  gsub("_jul.to.sep.2016", "", mart$indicator)
mart$indicator<-  gsub("_oct.to.dec.2016", "", mart$indicator)

mart1$indicator<-  gsub("_jan.to.mar.2018", "", mart1$indicator)
mart1$indicator<-  gsub("_apr.to.jun.2018", "", mart1$indicator)
mart$indicator<-  gsub("_jul.to.sep.2017", "", mart$indicator)
mart$indicator<-  gsub("_oct.to.dec.2017", "", mart$indicator)

mart$indicator<-  gsub("_jan.to.mar.2018", "", mart$indicator)
mart$indicator<-  gsub("_apr.to.jun.2018", "", mart$indicator)
mart$indicator<-  gsub("_jul.to.sep.2018", "", mart$indicator)
mart$indicator<-  gsub("_oct.to.dec.2018", "", mart$indicator)
mart$indicator<-  gsub("_jan.to.mar.2019", "", mart$indicator)

totmart<-mart[ ,.(totb=sum(value, na.rm=T),aprop=mean(value, na.rm=T)),
               by=c("indicator","year")]
tdisttb<-mart[ ,.(totb=sum(value, na.rm=T),aprop=mean(value, na.rm=T)),
                by=c("district_name","indicator","year")]
tdisttb<-data.table(tdisttb)

notifytb<-tdisttb[indicator=='total.notified.tb',.(district_name,year,notify_tb=totb)]
comptb<-tdisttb[indicator=='completed..all.tb.cases.',.(district_name,year,comp_tb=aprop)]
curetb<-tdisttb[indicator=='cured..all.tb.cases.',.(district_name,year,cure_tb=aprop)]
lostb<-tdisttb[indicator=='lost..all.tb.cases.',.(district_name,year,lost_tb=aprop)]


allTB1<-merge(notifytb,comptb,by=c("district_name","year"))
allTB2<-merge(allTB1,curetb,by=c("district_name","year"))
allTB<-merge(allTB2,lostb,by=c("district_name","year"))
allTB$success<-(allTB$comp_tb + allTB$cure_tb)
allTB$success[allTB$success>100]<-100
#----------------------------------------------------------------------
# pick shapefiles

shapeData <- shapefile("uga_dist112_map.shp")

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)] 

as_tibble(shapeData@data) 


# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)


coordinates_qtr <- rbind(coordinates,coordinates,coordinates,coordinates)
# this uses the repeat function: basically it repeats year over and over
coordinates_qtr[, year:=rep(2016:2019, each=nrow(coordinates))]
#coordinates_qtr$qtr<-factor(coordinates_qtr$qtr,levels=c(1,2,3,4,5),
          #                    labels = c("Jan-Mar'18","Apr-Jun'18","Jul-Sep'18","Oct-Dec'18","Jan-Mar'19"))
#---------------------------------------------------------------
coord_region<-merge(coordinates,shape_names,by="id")
coord_region_qtr <- rbind(coord_region,coord_region,coord_region,coord_region)
coord_region_qtr [, year:=rep(2016:2019, each=nrow(coord_region))]
coord_region_qtr$year<-factor(coord_region_qtr$year,levels=c(2016,2017,2018,2019),
                             labels = c("2016","2017","2018","2019"))
coordinates_qtr$year<-factor(coordinates_qtr$year,levels=c(2016,2017,2018,2019),
                 labels = c("2016","2017","2018","2019"))
#read in geneXpert data
#read data from .csv files -------------------------------------------------------------------------------------------------
qtr1 <- read.csv("QTR 1 2016.csv")
qtr2 <- read.csv("QTR 2 2016.csv")
qtr3 <- read.csv("QTR 3 2016.csv")
qtr4 <- read.csv("QTR 4 2016.csv")
qtr5 <- read.csv("QTR 1 2017.csv")
qtr6 <- read.csv("QTR 2 2017.csv")
qtr7 <- read.csv("QTR 3 2017.csv")
qtr8 <- read.csv("QTR 4 2017.csv")
qtr9 <- read.csv("QTR 1 2018.csv")
qtr10 <- read.csv("QTR 2 2018.csv")
qtr11 <- read.csv("QTR 3 2018.csv")
qtr12 <- read.csv("QTR 4 2018.csv")
qtr13 <- read.csv("QTR 1 2019.csv")

#introduce time or qtr
qtr1$year<-"2016"
qtr2$year<-"2016"
qtr3$year<-"2016"
qtr4$year<-"2016"
qtr5$year<-"2017"
qtr6$year<-"2017"
qtr7$year<-"2017"
qtr8$year<-"2017"
qtr9$year<-"2018"
qtr10$year<-"2018"
qtr11$year<-"2018"
qtr12$year<-"2018"
qtr13$year<-"2019"
#qtrs
qtr1$qtr<-1
qtr2$qtr<-2
qtr3$qtr<-3
qtr4$qtr<-4
qtr5$qtr<-5
qtr6$qtr<-6
qtr7$qtr<-7
qtr8$qtr<-8
qtr9$qtr<-9
qtr10$qtr<-10
qtr11$qtr<-11
qtr12$qtr<-12
qtr13$qtr<-13



##combine similar
gene1<-rbind(qtr1,qtr2,qtr3,qtr4,qtr5,qtr6,qtr7,qtr8,qtr9,qtr10,qtr11,qtr12,qtr13)
gene1<-data.table(gene1)
###need to split variable for merging rasta files
gene1$district_name<- sapply(strsplit(as.character(gene1$District),' '), "[", 1)
setnames(gene1,"No.","id")

gene1<-gene1[id!=0 ]
gene1<-gene1[id!=""]
gene1<-gene1[id!='ARUA TOTAL']

#Butenga HC IV
gene1$District[gene1$id==40 & gene1$District==""]<-"Dokolo"
gene1$Region[gene1$id==40 & gene1$Region==""]<-"Lira"
gene1$District[gene1$id==97 & gene1$District==""]<-"Kasese"
gene1$Region[gene1$id==97 & gene1$Region==""]<-"Fortportal"
gene1$District[gene1$id==55 & gene1$District==""]<-"Maracha"
gene1$Region[gene1$id==55 & gene1$Region==""]<-"Arua"
##check!!!!!!! to resolve
gene1$District[gene1$District==""]<-"Missing"
gene1$Region[gene1$Region==""]<-"Missing"



#-------------------------convert to numeric
gene1$MTB..Rif.<-as.numeric(as.character(gene1$MTB..Rif.))
gene1$Rif.Resist<-as.numeric(as.character(gene1$Rif.Resist))
gene1[grep(pattern='Lira', Region), Region:="Lira" ]

#----------------------introduce counter for site
gene1$site[gene1$Genexpert.Site!='NA']<-1

#align district names
gene1<-merge_new_dists(gene1)
#add TB from HMIS

gene2<-merge(allTB,gene1, by=c("district_name","year"),all.x=T)

gene<-merge(gene2,lab_treat, by=c("district_name","year","qtr"),all.x=T)


gene$diff<-((gene$notify_tb) - (gene$MTB..Rif. + gene$Rif.Resist))

#merge in data from hmis mdr
#-------------------------------------------------------------------------------------------------------------



test<-merge(shape_names,gene,by="district_name")


#collapse data into district_name Region qtr
ALLdistrict<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                  Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                  postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                  acomp_tb=mean(comp_tb, na.rm=T),
                  acure_tb=mean(cure_tb, na.rm=T),
                  alost_tb=mean(lost_tb, na.rm=T),
                  asuccess=mean(success, na.rm=T),
                  smachine=sum(machines, na.rm=T),
                  diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                  totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                  sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))),
                  sconf_treat=sum(conf_treat, na.rm=T)),
               by=c("district_name","Region","year")]


#overall trends computations
otrends<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                  Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                  postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                  acomp_tb=mean(comp_tb, na.rm=T),
                  acure_tb=mean(cure_tb, na.rm=T),
                  alost_tb=mean(lost_tb, na.rm=T),
                  asuccess=mean(success, na.rm=T),
                  smachine=sum(machines, na.rm=T),
                  diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                  totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                  sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))),
                  sconf_treat=sum(conf_treat, na.rm=T) ),
               by=c("year")]

otrends_long<-melt(otrends,id.vars = c("year"))

otrends_long[ ,unique(variable)]

otrends_long$Indicator<-factor(otrends_long$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess,
                                                              sgene_lab,sconf_treat"),
                           labels = c("Samples tested","MTB positive","Resistant","Indeterminate",
                                      "Errors","Percent TB Positive","Percent resistant",
                                      "Percent Indeterminate","Percent errors","GeneXpert sites",
                                      "Notified TB","Completed TB","Cure TB","Lost TB",
                                      "TB positive by geneXpert","TB Success rate",
                                      "MDR-DR confirmed TB","Started treatmnent for MDR-DR"))

#overal quarterly trends
otrends_qtr<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                  Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                  postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                  gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                  acomp_tb=mean(comp_tb, na.rm=T),
                  acure_tb=mean(cure_tb, na.rm=T),
                  alost_tb=mean(lost_tb, na.rm=T),
                  asuccess=mean(success, na.rm=T),
                  smachine=sum(machines, na.rm=T),
                  diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                  totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                  sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))),
                  sconf_treat=sum(conf_treat, na.rm=T) ),
               by=c("qtr")]
otrends_qtr<-otrends_qtr[qtr!='NA']
otrends_qtr_long<-melt(otrends_qtr,id.vars = c("qtr"))

otrends_qtr_long$Indicator<-factor(otrends_qtr_long$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess",
                                                              "sgene_lab","sconf_treat"),
                               labels = c("Samples tested","MTB positive","Resistant","Indeterminate",
                                          "Errors","Percent TB Positive","Percent resistant",
                                          "Percent Indeterminate","Percent errors","GeneXpert sites",
                                          "Notified TB","Completed TB","Cure TB","Lost TB",
                                          "TB positive by geneXpert","TB Success rate",
                                          "MDR-DR confirmed TB","Started treatmnent for MDR-DR"))

Region<-gene[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                 Rif_indet=sum(Rif.indet, na.rm=T),tot_errors=sum(total.errors, na.rm=T),
                 postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                 gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                 acomp_tb=mean(comp_tb, na.rm=T),
                 acure_tb=mean(cure_tb, na.rm=T),
                 alost_tb=mean(lost_tb, na.rm=T),
                 asuccess=mean(success, na.rm=T),
                 ratio_samplesp_site=((sum(Samples.tested, na.rm=T))/(sum(site, na.rm=T))),
                 smachine=sum(machines, na.rm=T),
                 diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                 totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                 sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))) ,
                 sconf_treat=sum(conf_treat, na.rm=T)),
                     by=c("Region","year")]

Region<-data.table(Region)
Region<-Region[stot!='0',]

Region$snotify_tb[Region$snotify_tb > 200000]<-200000


Region_long<-melt(Region,id.vars = c("Region","year"))
Region_long$Indicator<-factor(Region_long$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess"),
                               labels = c("Samples tested","MTB positive Rif negative","Resistant","Indeterminate",
                                          "Errors","Percent TB Positive","Percent resistant",
                                          "Percent Indeterminate","Percent errors","GeneXpert sites",
                                          "Notified TB","Completed TB","Cure TB","Lost TB",
                                          "TB positive by geneXpert","TB Success rate"))
##maps
dist<-gene
dist<-data.table(dist)

dist<-merge_new_dists(dist) 
district<-dist[ ,.(stot=sum(Samples.tested, na.rm=T),mtb_rif=sum(MTB..Rif., na.rm=T),Rif_Resist=sum(Rif.Resist, na.rm=T),
                   postb=100*((sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   presist=100*((sum(Rif.Resist, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   pindet=100*((sum(Rif.indet, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   perrors=100*((sum(total.errors, na.rm=T))/(sum(Samples.tested, na.rm=T))),
                   gene_sites=sum(site, na.rm=T), snotify_tb=sum(notify_tb, na.rm=T),
                   acomp_tb=mean(comp_tb, na.rm=T),
                   acure_tb=mean(cure_tb, na.rm=T),
                   alost_tb=mean(lost_tb, na.rm=T),
                   asuccess=mean(success, na.rm=T),
                   ratio_samplesp_site=((sum(Samples.tested, na.rm=T))/(sum(site, na.rm=T))),
                   smachine=sum(machines, na.rm=T),
                   diff=((sum(notify_tb, na.rm=T))-(sum(MTB..Rif., na.rm=T))),
                   totpost=sum(MTB..Rif.,Rif.Resist,Rif.indet, na.rm=T),
                   sgene_lab=((sum(Rif.Resist, na.rm=T))+(sum(labconf,na.rm=T))) ,
                   sconf_treat=sum(conf_treat, na.rm=T)),
              by=c("district_name","year")]
#----------------------------------------------------------------------------------------
district[, list(district_name,max.diff=max(diff)), by=year]

#- merge data to shape files
totdist<- merge(district, shape_names, by='district_name', all=T)

ratios <- brewer.pal(8, 'Accent')
ratios1 <- brewer.pal(8, 'Blues')
ratios2 <- brewer.pal(8, 'Greens')
ratios3 <- brewer.pal(8, 'Oranges')
ratios4 <- brewer.pal(8, 'Reds')


map <- merge(coordinates_qtr, totdist, by=c('id', 'year'), all=T, allow.cartesian=TRUE)
#map <- map[year!='2019']


#--------------Number of partners in the region

part<-unique(gene[,.(Region,I.Partner,year)])
part<-data.table(part)
part[I.Partner!='#REF!'& I.Partner!="",npart:=1]

tpart<-part[ ,.(snpart=sum(npart, na.rm=T)),
              by=c("Region","year")]
#combine with samples tested and others
part_region<-merge(Region,tpart,by=c("Region","year"))
part_region<-data.table(part_region)
part_region<-part_region[Region!='NA',.(Region,year,stot,gene_sites,snpart)]
part_reg<-melt(part_region,id.vars = c("Region","year"))

part_reg$Indicator<-factor(part_reg$variable,levels=c("stot","gene_sites","snpart"),
                 labels = c("Samples tested","GeneXpert sites","Implementing Partners"))

#Generating region maps
ALLdistrict_map<-merge_new_dists(gene)
ALLdistrict_link<-ALLdistrict_map[ ,.(stot1=1),
                                   by=c("district_name","Region","year")]

#read link to regions
Link_district_region<-read_rds("Link_district_region.rds")
#add qtrs
Link_district_region_qtr <- rbind(Link_district_region,Link_district_region,Link_district_region,Link_district_region)

Link_district_region_qtr<-data.table(Link_district_region_qtr)
Link_district_region_qtr[, year:=rep(1:4, each=nrow(Link_district_region))]
Link_district_region_qtr$year<-factor(Link_district_region_qtr$year,levels=c(1,2,3,4),
                           labels = c("2016","2017","2018","2019"))


Linked_region<-merge(coord_region_qtr,Link_district_region_qtr,by=c("district_name","year"))

#merge in shapefiles 
#shapefiles is coord_region_qtr
#Linked_region<-merge(coord_region_qtr,Linked_region,by=c("district_name","year"))
coord_region_qtr_map <- merge( Region,Linked_region, by=c("Region","year"), all=T, allow.cartesian=TRUE)

coord_region_qtr_map$diffmdr<-(coord_region_qtr_map$sconf_treat -coord_region_qtr_map$sgene_lab)
coord_region_qtr_map$diffmdr1<-coord_region_qtr_map$diffmdr
coord_region_qtr_map$diffmdr1[coord_region_qtr_map$diffmdr>3000]<-3000
#------------------------------------------------------------------need to manipulate regional borders

#--------------district level maps------------------------------------------
coord_region_qtr_district<-merge(coord_region_qtr,district,by=c("district_name","year"), all=T, allow.cartesian=TRUE)

coord_region_qtr_district$diffmdr<-(coord_region_qtr_district$sconf_treat -coord_region_qtr_district$sgene_lab)
coord_region_qtr_district$diffmdr1<-coord_region_qtr_district$diffmdr
coord_region_qtr_district$diffmdr1[coord_region_qtr_district$diffmdr>3000]<-3000



#----------------------------------------------------------------------------------------------------data manipulation ends here
##checking regions
#coord_region_qtr_map
pdf("tb_geneXpert_figures_map_4_regional_yearly_per_page.pdf", width=9, height=9)

#coord_region_qtr_map
ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=stot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Samples tested per GeneXpert site",
       caption="Source: GenXpert NTLP",Subtitle="2016-2019",
       fill="count")



ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=postb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="TB positive",
       caption="Source: GenXpert NTLP",Subtitle="2016-2019",
       fill="Percent") 


ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=snotify_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Notified tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="count")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=asuccess)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios7) +
  facet_wrap(~year)+
  labs(title="TB treatment success rate",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=presist)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title=" Percentage resistant",
       caption="Source: GeneXpert",Subtitle="2016-2019",
       fill="Percent")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=acomp_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Completed tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent") 

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios6) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent") 
ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios7) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2019",
       fill="Percent")

ggplot(coord_region_qtr_map, aes(x=long, y=lat, group=group, fill=diff)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Excess of Notified Tb less TB positive by GeneXpert",
       caption="Source: GeneXpert & HMIS",Subtitle="2016-2019",
       fill="count")
dev.off()

#---------------------------------------------
pdf("tb_geneXpert_figures_map_3_regional_yearly_per_page.pdf", width=9, height=6)

#coord_region_qtr_map
ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=stot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Samples tested per GeneXpert site",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=postb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Percentage of samples tested that are positive",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=(snotify_tb/1000))) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Notified tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="count(000)")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=asuccess)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="TB treatment success rate",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=presist)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title=" Percentage resistant",
       caption="Source: GeneXpert",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acomp_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Completed tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diff)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Excess of Notified Tb less TB positive by GeneXpert",
       caption="Source: GeneXpert & HMIS",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_map[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

dev.off()

#overal quarterly trends
#read mdr cases
mdr<-read.csv("mdr_cases.csv")

ggplot(mdr,aes(x=qtr, y=conf_mdr)) +
  geom_point(size=1,color="blue") +
  geom_line(size=1,color="blue") +
  labs(title='Count', subtitle='July 2015 to Mar 2019',
       x='Date', y='Count', caption='Source: HMIS') +
  theme_bw()+
   scale_y_continuous(limits = c(0,100))+
  scale_x_continuous(breaks=c(1,4,7,10,13),
                     labels=c("Jul-Sep'15","Apr-Jun'16","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(mdr,aes(x=qtr, y=opd_presumed_mdr)) +
  geom_point(size=1,color="red") +
  geom_line(size=1,color="red") +
  labs(title='Count', subtitle='July 2015 to Mar 2019',
       x='Date', y='Count', caption='Source: HMIS') +
  theme_bw()+
  scale_y_continuous(limits = c(0,3000))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")


ggplot(otrends_qtr_long[variable=='postb'|variable=='presist'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Proportion', subtitle='2016 to Mar 2019',
       x='Date', y='Percent', caption='Source: GeneXpert NTLP; Notified TB-HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  scale_y_continuous(limits = c(0,20))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(otrends_qtr_long[variable=='presist'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Proportion', subtitle='2016 to Mar 2019',
       x='Date', y='Percent', caption='Source: GeneXpert NTLP; Notified TB-HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")


ggplot(otrends_qtr_long[variable=='sconf_treat' | variable=='sgene_lab'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(otrends_qtr_long[variable=='sconf_treat' ],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  scale_y_continuous(limits = c(0,30000))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

ggplot(otrends_qtr_long[ variable=='sgene_lab'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

#Rif_Resist
ggplot(otrends_qtr_long[ variable=='Rif_Resist'],
       aes(x=qtr, y=(value),color=variable)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~Indicator, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

#read in mdr cases
md_r<-read.csv("mdr_cases.csv")

md_r<-data.table(md_r)
ggplot(md_r[qtr>"2"],
       aes(x=qtr, y=conf_mdr,color="blue")) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(title='Count', subtitle='2016 to Mar 2019',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~conf_mdr, scales="free" )+
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(breaks=c(3,6,9),
                     labels=c("Jul-Sep'16","Oct-Dec'17","Jul-Sep'18"))+
  theme(legend.position="none")

##years
##figures for deep dive
otrends_long_yr18<-otrends_long[year!=2019,]
otrends_long_yr18$indicator<-factor(otrends_long_yr18$variable,levels=c("stot","mtb_rif","Rif_Resist","Rif_indet",
                                                              "tot_errors","postb","presist","pindet",
                                                              "perrors","gene_sites","snotify_tb",
                                                              "acomp_tb","acure_tb","alost_tb","totpost","asuccess",
                                                              "sgene_lab","sconf_treat"),
                               labels = c("Samples tested","MTB positive","Resistant","Indeterminate",
                                          "Errors","Percent TB Positive","Percent resistant",
                                          "Percent Indeterminate","Percent errors","GeneXpert sites",
                                          "Notified TB","Completed TB","Cure TB","Lost TB",
                                          "TB positive by geneXpert","TB Success rate",
                                          "MDR-DR confirmed TB","Started treatmnent for MDR-DR"))

pdf("mdr_related_figures.pdf", width=9, height=6)

ggplot(otrends_long_yr18[variable=='sconf_treat' | variable=='sgene_lab' | variable=='totpost'],
       aes(x=year, y=(value)),color=variable) +
  geom_bar(stat='identity',color='blue') +
  labs(title='Count', subtitle='2016 - 2018',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  theme(legend.position="none")

ggplot(otrends_long,
       aes(x=year, y=(value)),color=variable) +
  geom_bar(stat='identity',color='blue') +
  labs(title='Count', subtitle='2016 - 2018',
       x='Date', y='Count', caption='Source: GeneXpert NTLP;HMIS') +
  theme_bw()+
  facet_wrap(~variable, scales="free" )+
  theme(legend.position="none")

dev.off()

#--------------district level maps
pdf("tb_geneXpert_figures_map_3_district_yearly_per_page.pdf", width=9, height=6)

#coord_region_qtr_district
ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=stot)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Samples tested by GeneXpert from each district",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=postb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="TB positive",
       caption="Source: GenXpert NTLP",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=(snotify_tb/1000))) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Notified tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="count(000)")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=asuccess)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="TB treatment success rate",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=presist)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title=" Percentage resistant",
       caption="Source: GeneXpert",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acomp_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Completed tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios2) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=acure_tb)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Cured tb",
       caption="Source: HMIS",Subtitle="2016-2018",
       fill="Percent")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diff)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios4) +
  facet_wrap(~year)+
  labs(title="Excess of Notified Tb less TB positive by GeneXpert",
       caption="Source: GeneXpert & HMIS",Subtitle="2016-2018",
       fill="count")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios3) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(coord_region_qtr_district[year=='2016'| year=='2017' | year=='2018'], aes(x=long, y=lat, group=group, fill=diffmdr1)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01, color="#636363") +
  theme_void() +
  scale_fill_gradientn(colors=ratios1) +
  facet_wrap(~year)+
  labs(title="Excess of lab confirmed MDR-RR",
       caption="Excess is (Started on MDR-DR treatment minus (Microscopy confirmed MDR-DR plus geneXpert MDR-DR)). Data source: HMIS & GeneXpert-NTLP",
       fill="Count") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

dev.off()
>>>>>>> 9ab71f58bb989a0ace5972d747c4a1054f84cc3c
