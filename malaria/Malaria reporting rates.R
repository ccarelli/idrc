#-------------------------------
# set up R

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

#-disable scientific notation
options(scipen=999)


inDir="//Malaria_raw_data/"

setwd(inDir)

outDir="//Malaria_prepped_data/"

outputs="//Malaria_outputs/"
#---------------------------------------------------------------
mal<-read.csv("hf_reportingcompleteness_qtrly.csv")
mal<-data.table(mal)

mal[,c("organisationunitid" ,"organisationunitcode" ,"organisationunitdescription",
       "perioddescription","periodcode","periodname"):=NULL]


new_col<-c("facility_name","qtr","Suspected_malaria_fever","RDT_tested_cases")

setnames(mal,new_col)

mal_long<-melt(mal,id.vars = c("facility_name","qtr"))
mal_long<-data.table(mal_long)
mal_long$qtr<-as.character(mal_long$qtr)
# grab the year from quarterly data 
mal_long[ ,year:=unlist(lapply(strsplit(qtr, "Q"), "[", 1))]

# grab the month from quarterly data
mal_long[grep("Q1", qtr), month:='01']
mal_long[grep("Q2", qtr), month:='04']
mal_long[grep("Q3", qtr), month:='07']
mal_long[grep("Q4", qtr), month:='10']

# visualize reporting completeness
mal_long[ , date:=paste0(year, '-', month, '-01')]
#mal_long[ , c("year", "month", "qtr"):=NULL]
mal_long[ , date:=ymd(date)]
#----------------------------------------------------------------------
# drop out the white spaces and put facility_name in lower case
mal_long[ ,facility1:=tolower(facility_name)]
mal_long[ ,facility1:=gsub(" ", "", facility1)]

# search for variable type - remember the order is important

# strange types of facilities - do first so they are overwritten
mal_long[grep("-nr", facility1), facility_level:='NR']
mal_long[grep("clinic", facility1), facility_level:='Clinic/Drug Shop/Medical Centre']
mal_long[grep("drugshop", facility1), facility_level:='Clinic/Drug Shop/Medical Centre']
mal_long[grep("medicalcent", facility1), facility_level:='Clinic/Drug Shop/Medical Centre']
mal_long[grep("healthcarecentre", facility1), facility_level:='Clinic/Drug Shop/Medical Centre']
mal_long[grep("familypharmacy", facility1), facility_level:='Clinic/Drug Shop/Medical Centre']
mal_long[grep("maternityhome", facility1), facility_level:='HC II']


mal_long[grep("hospital", facility1), facility_level:='Hospital/Military Hospital']
mal_long[grep("domiciliaryhome", facility1), facility_level:='HC II']
mal_long[grep("hcii", facility1), facility_level:='HC II']
mal_long[grep("hciii", facility1), facility_level:='HC III']
mal_long[grep("iii", facility1), facility_level:='HC III']
mal_long[grep("hciv", facility1), facility_level:='HC IV']

# typos and strange facility types set to other
mal_long[is.na(facility_level), facility_level:='Other(Static Centre/health centre)']

# drop out the extra variable used to create facility level
mal_long[ ,facility1:=NULL]

#---------------------------------------------------------------------------------------------------------
# analyze reporting completeness for all variables

# quantify the missingness
mal_yr<-mal_long[!is.na(value),.(facilities=length(unique(facility_name))), by=.(year)]
# count the facilities in the entire data set
mal_long[ ,.(facilities=length(unique(facility_name))), by=.(date)]


#-------------------------------
# reporting completeness nationally for all variables

# analyze reporting completeness
# how many facilities reported in that month?
report = mal_long[!is.na(value) ,.(facilities=length(unique(facility_name))), by=.(date)]
report
#-mean reap
fac_report = mal_long[!is.na(value) ,.(facilities=length(unique(facility_name))), by=.(date, facility_level)]
#-------------------------------
#working on the suspected cases tested per facility
#pick suspected cases tested
mal_long_overall<-readRDS(paste0(outDir,"mal_long_overall.rds"))
mal_susp<-mal_long_overall[variable=='Suspected_cases_tested',.(date,susp_cases_tested=Tot_cases)]

mal_susp_report<-merge(mal_susp,report,by="date")

mal_susp_report<-data.table(mal_susp_report)
mal_susp_report$suspected_cases_tested_per_facility<-round(mal_susp_report$susp_cases_tested/mal_susp_report$facilities,0)

mal_susp_report_long<-melt(mal_susp_report,id.vars = c("date"))
# reporting completeness


# how many facilities reported every month?
# when should we start looking at the data?
pdf(paste0(outputs,"Number_of_facilities_reporting_for_malaria.pdf"))
ggplot(report, aes(x=date, y=facilities))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number of facilities reporting for Malaria through WEP',
       x='Date', y='Number of facilities reporting')

ggplot(report[date>='2013-01-01'], aes(x=date, y=facilities))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number of facilities reporting for Malaria through WEP',subtitle = '2013-2019',
       x='Date', y='Number of facilities reporting')

ggplot(fac_report[date>='2013-01-01'], aes(x=date, y=facilities,color=facility_level))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number of facilities reporting for Malaria through WEP by facility level',subtitle = '2013-2019',
       x='Date', y='Number of facilities reporting')

ggplot(fac_report[date>='2013-01-01'], aes(x=date, y=facilities))+
  geom_point()+
  geom_line()+
  facet_wrap(~facility_level,scales = "free")+
  theme_bw() +
  labs(title='Number of facilities reporting for Malaria through WEP by facility level',subtitle = '2013-2019',
       x='Date', y='Number of facilities reporting')

ggplot(mal_susp_report[date>='2013-01-01'], aes(x=date, y=suspected_cases_tested_per_facility))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number of suspected cases tested per facility',subtitle = '2013-2019',
       x='Date', y='Number')


ggplot(mal_susp_report_long[date>='2013-01-01'], aes(x=date, y=value,color=variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~variable,scales="free")+
  theme_bw() +
  labs(title='Number of suspected cases tested per facility',subtitle = '2013-2019',
       x='Date', y='Number')+
  theme(legend.position = "none")

dev.off()



