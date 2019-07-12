
# Quantify the missingness in the data
# Answer the question: how much bias is due to incomplete reporting?
# 4/7/2019

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

#-------------------------------
# set the working directory
setwd("C:/Users/Jaffer/Desktop/PCE/TB/Data/Latest")

# import the prepped data 
tb = readRDS("tb_hmis_prepped_notified.rds")

# peek at the data to make sure it is ok
str(tb)
#-------------------------------
# analyze reporting completeness for all variables

# quantify the missingness

# count the facilities in the entire data set
tb[ ,.(facilities=length(unique(org_unit_id))), by=.(date)]

# shape the data long
tb_long = melt(tb, id.vars=c('org_unit_id', 'facility', 'period_code', 'date', 'facility_level'))

#-------------------------------
# reporting completeness nationally for all variables

# analyze reporting completeness
# how many facilities reported in that month?
report = tb_long[!is.na(value) ,.(facilities=length(unique(org_unit_id))), by=.(date)]
report

# reporting completeness

# how many facilities reported every month?
# when should we start looking at the data?
ggplot(report, aes(x=date, y=facilities))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number of facilities reporting for all TB variables',
       x='Date', y='Number of facilities reporting') +
  theme(text=element_text(size=18))

#----------------------------------------
# reporting by variable/indicator 

# but what does reporting look like for individual indicators?
# analyze reporting completeness
# how many facilities reported in that month?
report2 = tb_long[!is.na(value) ,.(facilities=length(unique(org_unit_id))), by=.(date, variable)]
report2

# reporting with color for variable
ggplot(report2, aes(x=date, y=facilities, color=variable))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.6)+
  theme_bw()

# facet wrap to see overlapping variables
ggplot(report2, aes(x=date, y=facilities, color=variable))+
  geom_point()+
  facet_wrap(~variable, scales='free_y')+
  geom_line()+
  theme_bw() +
  labs(title = 'Number of facilities reporting by variable', 
       x='Date', y="Number of facilities reporting", color='Variable')

#----------------------------------------
# reporting by facility level and variable

# facilities reporting by level and variable
fac_report = tb_long[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                     by=.(date, variable, facility_level)]

ggplot(fac_report, aes(x=date, y=facilities, color=facility_level)) +
  geom_point()+
  geom_line()+
  facet_wrap(~variable)+
  labs(title='Reporting by variable and health facility level',
       x='Date', y='Number of facilities reporting')

ggplot(fac_report, aes(x=date, y=facilities, color=variable)) +
  geom_point()+
  geom_line()+
  facet_wrap(~facility_level)+
  theme_bw()+
  labs(title='Reporting by variable and health facility level',
       x='Date', y='Number of facilities reporting')

#----------------------------------------
# reporting by health facility level
fac_report2 = tb_long[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                      by=.(date, facility_level)]

ggplot(fac_report2, aes(x=date, y=facilities, color=facility_level)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title='Reporting by health facility level',
       x='Date', y='Number of facilities reporting')


#----------------------------------------

tbno<-tb_long[variable=='total_notified_tb',.(tb_notified=sum(value,na.rm=T)),by="date"]

tbno_reporting<-merge(tbno,report,all=T,by="date")
tbno_rep_long<-melt(tbno_reporting,id.vars="date")

#----------------------------------------
#pick out period
tbno_rep_long_pe<-tbno_rep_long[date>'2015-07-01' & date<'2019-04-01',]

pdf("tb_notified_and_reporting_completeness.pdf",height = 9,width = 12)
ggplot(tbno_rep_long, aes(x=date, y=value, color=variable)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~variable,scales="free")+
  labs(title='TB cases notified and Reporting by health facility level',
       x='Date', y='Count')

ggplot(tbno_rep_long_pe, aes(x=date, y=value, color=variable)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~variable,scales="free")+
  labs(title='TB cases notified and Reporting by health facility level',
       x='Date', y='Count')

dev.off()



#----------------------------------------
# reporting completeness by district

# need to merge in the districts - fake data
district = c(1:112)
tb_long = cbind(tb_long, district)

# number of facilities that report for each district for each month
report_dist = tb_long[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                      by=.(date, district)]

# create a pdf of every district's reporting completeness
list_of_plots = NULL
i = 1

for (d in unique(report_dist$district)) {
  
  district_name = as.character(d)  
  
  list_of_plots[[i]] = ggplot(report_dist[district==d], aes(x=date, y=facilities))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Reporting completeness: ", district_name),
         x="Date", y="Facilities reporting")
  
  i = i+1
}


# print the pdf - one page for each district
pdf("tb_district_completeness.pdf")

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}
dev.off()

#---------------------------------------

#----------------------------------------
# reporting completeness by district and health facility level

# number of facilities that report for each district for each month
report_dist_level = tb_long[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                            by=.(date, district, facility_level)]

# create a pdf of every district's reporting completeness
list_of_plots = NULL
i = 1

for (d in unique(report_dist$district)) {
  
  district_name = as.character(d)  
  
  list_of_plots[[i]] = ggplot(report_dist[district==d], aes(x=date, y=facilities, color=facility_level))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Reporting completeness: ", district_name),
         x="Date", y="Facilities reporting", color='Facility level')
  
  i = i+1
}


# print the pdf - one page for each district
pdf("tb_district_completeness.pdf")

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}
dev.off()

#---------------------------------------
# number of bacteriologically confirmed cases per facility

cases = tb_long[variable=='ptb_bact_conf_cases', .(value=sum(value, na.rm=T)), by=date]
cases = merge(cases, report, by='date', all=T)
cases[ ,mean_cases:=value/facilities]
setnames(cases, 'value', 'conf_cases')

# number of cases per facility per month
ggplot(cases, aes(x=date, y=mean_cases))+
  geom_point()+
  geom_line()+
  theme_bw()

cases_long = melt(cases, id.vars='date')
cases_long = cases_long[2015 < year(date) & year(date) < 2019 ]

cases_long$variable = factor(cases_long$variable, c('facilities', 'conf_cases', 'mean_cases'),
                             c('Facilities reporting', 'Confirmed cases', 'Mean cases per facility'))

ggplot(cases_long, aes(x=date, y=value, color=variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  labs(x='Date', y="", color="")

#---------------------------------------
# table of facilities reporting

tb[ , year:=year(date)]
period = tb[year==2017 | year==2018]
period = period[!is.na(total_notified_tb) ,.(facilities=length(unique(org_unit_id))), by=.(year, facility_level)]
period = period[order(year, facilities)]
period = dcast(period, facility_level~year)
write.csv(period, "table_columns.csv")

tb[ , length(unique(org_unit_id)), by=year]
tb[!is.na() , length(unique(org_unit_id)), by=year]

tb_qtr_mean<-tbno_rep_long_pe[,.(smean=mean(value,na.rm=T)),by="variable"]

tbno_rep_long_pe[,year:=]
tbno_rep_long_pe[,.(smean=mean(value,na.rm=T)),by=c("year","variable")]


tb[!is.na()]
