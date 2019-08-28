#Super-imposing reporting rates onto weekly malaria cases
#27/08/2019
# --------------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(lubridate)
library(foreign)
# ------------------
options(scipen=999)

indir="D:/PCE/Malaria/"
setwd(indir)
outdir="D:/PCE/Malaria_outputs/"

mala<-read.csv("weekly_mal_cases_new.csv")
mala<-data.table(mala)

mala$organisationunitname<-as.character(mala$organisationunitname)
mala[ ,district:=unlist(lapply(strsplit(organisationunitname, " "), "[" ,1))]


mala[,c("organisationunitid","organisationunitcode","organisationunitname",
        "organisationunitdescription","perioddescription","periodname","periodcode"):=NULL]

new_col<-c("periodid","microscope_positive","rdt_positive","district")
setnames(mala,new_col)

mala$periodid<-as.character(mala$periodid)
mala[ ,year:=unlist(lapply(strsplit(periodid, "W"), "[" ,1))]
mala[ ,week:=unlist(lapply(strsplit(periodid, "W"), "[" ,2))]

mala[is.na(mala)]<-0
mala[,tot_cases:=(microscope_positive + rdt_positive)]
mala_national<-mala[,.(total_cases=sum(tot_cases,na.rm=T)),by=c("year","week")]

#----------------------------------
#pick reporting rates
rep<-read.csv("wep_reportingrates.csv")
rep<-data.table(rep)
rep$periodid<-as.character(rep$periodid)
rep[ ,year:=unlist(lapply(strsplit(periodid, "W"), "[" ,1))]
rep[ ,week:=unlist(lapply(strsplit(periodid, "W"), "[" ,2))]

rep[,c("organisationunitid","organisationunitcode","organisationunitname","periodid",
        "organisationunitdescription","perioddescription","periodname","periodcode"):=NULL]
rep_col<-c("reporting_rates","year","week")
setnames(rep,rep_col)

#----------------------------------------------------
mala_national_rep<-merge(mala_national,rep,by=c("year","week"))
mala_nation_2015<-mala_national_rep[year>='2015']
mala_nation_2015$year<-as.numeric(mala_nation_2015$year)
mala_nation_2015$week<-as.numeric(mala_nation_2015$week)
mala_nation_2015<-setorder(mala_nation_2015,year,week)

#---------------------------------------period
mala_nation_2015[,period:= week]
mala_nation_2015[year==2016,period:=(week + 52)]
mala_nation_2015[year==2017,period:=(week + (2*52))]
mala_nation_2015[year==2018,period:=(week + (3*52))]
mala_nation_2015[year==2019,period:=(week + (4*52))]

#mala_nation_2015[,wk:=paste0("Week",week,"-",year)]

mala_nation_2015<-mala_nation_2015[period<=241]
mala_nation_2015$year<-as.character(mala_nation_2015$year)

mala_nation_2015$Second_axis<-mala_nation_2015$reporting_rates*5000


#----------------------------------------------------------------------------------

  pdf(paste0(outdir,"Superimposed_figures.pdf"))
    ggplot()  + 
    geom_bar(aes(x=mala_nation_2015$period, y=mala_nation_2015$total_cases),
             stat="identity",fill=mala_nation_2015$year)+
    geom_line(aes(x=mala_nation_2015$period,
                  y=mala_nation_2015$Second_axis,color='blue'),stat="identity")+
      scale_y_continuous(sec.axis = sec_axis(~./5000, name = "Reporting rates (%)"))+
      scale_x_continuous(breaks=c(1,52,104,156,202,234),
                     labels=c("Week1_2015", "Week52_2015", "Week52_2016","Week52_2017","Week52_2018","Week33_2019"))+
    theme_bw()+
  labs(x='Date', y='Count', 
       title="Malaria cases with superimposed reporting rates",subtitle = "Week1_2015 - Week33_2019",
       caption="Source: HMIS") +
  theme(text = element_text(size=12),legend.position = "none",
        axis.text.x = element_text(angle = 45)) 

    
#pick graphs for only 2018 week 33 and 2019 week

select_data<-mala_nation_2015[(period>=157 & period<=189) | (period>=209 & period<=241)]
select_data[period>=209 & period<=241,period:=(period - 19)]

ggplot()  + 
  geom_bar(aes(x=select_data$period, y=select_data$total_cases),
           stat="identity",fill=select_data$year)+
  geom_line(aes(x=select_data$period,
                y=select_data$Second_axis,color='blue'),stat="identity")+
  scale_y_continuous(sec.axis = sec_axis(~./5000, name = "Reporting rates (%)"))+
  scale_x_continuous(breaks=c(157,169,186,190,204,219),
                     labels=c("Week1_2018", "Week15_2018", "Week30_2018","Week1_2019","Week15_2019","Week30_2019"))+
  theme_bw()+
  labs(x='Date', y='Count', 
       title="Malaria cases with superimposed reporting rates",subtitle = "Week1_33_2018 and Week1_33_2019",
       caption="Source: HMIS") +
  theme(text = element_text(size=12),legend.position = "none",
        axis.text.x = element_text(angle = 45)) 
dev.off()
